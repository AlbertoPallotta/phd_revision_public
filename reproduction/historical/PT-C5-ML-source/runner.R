## ============================================
## Chapter 5 — PortfolioTesteR empirical illustration
## ============================================
##
## THE QUESTION
##   Can a machine-learning model, given only past prices, rank 98 US stocks
##   well enough to build a long-only portfolio that survives realistic
##   trading costs?
##
## THE SETUP
##   98 US equities, weekly, 2014-2023 (522 weeks), split/dividend adjusted.
##   Features (all lagged one week, no look-ahead):
##     momentum 12 / 26 / 52 weeks, RSI-14, distance from MA20
##   Label: forward return over h weeks, for h in {1, 4, 13}.
##
## THE WALK-FORWARD
##   Train 156 weeks -> predict 4 -> slide 4 -> refit from scratch. 91 windows.
##   Every score is out-of-sample: the model never sees the weeks it ranks.
##
## TWO SPECIFICATIONS
##   pooled     one XGBoost model across all 98 names (baseline)
##   per_group  one model per GICS sector, plus max 3 names per sector at
##              selection (PRIMARY specification)
##
## PORTFOLIO
##   z-score each week's scores -> top 20 -> softmax weights (temperature 12)
##   -> 10% per-name cap -> renormalise to fully invested.
##
## EVALUATION
##   Gross, then net of proportional costs at 5/10/15/20/25/50 bps per unit
##   turnover (turnover = half the L1 change in weights). Also split
##   pre-2020 vs post-2020 at a fixed 2020-01-01 date.
##
## WHAT IT FOUND
##   1. HORIZON DEPENDENCE. Gross performance rises monotonically from h=1
##      to h=13 in both specifications. The quarterly horizon is strongest.
##   2. SECTOR-NEUTRAL BEATS POOLED at the medium and quarterly horizons, in
##      both subperiods. Within-sector ranking stays informative once
##      implementable constraints are applied. This is the headline result.
##   3. COSTS BITE, AND WHERE IS QUANTIFIED. At h=13 sector-neutral,
##      annualised return falls ~24.6% (5bp) -> 21.5% (20bp) -> 15.5% (50bp);
##      Sharpe 1.34 -> 1.17 -> 0.85. Robust under low-to-moderate costs,
##      degrading materially under high frictions - consistent with the
##      strategy's non-trivial turnover.
##
## WHAT IT IS NOT
##   A structured test case for the library, not an investment claim.
##   Paper-inspired rather than a strict replication. The contribution is
##   that the whole workflow - features, leakage-safe labels, walk-forward
##   validation, cross-sectional forecasting, constrained portfolio
##   construction, cost-aware evaluation - is expressed in one framework.
##
## REPRODUCIBILITY
##   Configuration fully specified: database path, symbol list, sector map,
##   sample dates, horizons, schedule, seed. Requires the private SQLite
##   database (not distributed); everything else runs from CRAN.
## ============================================
library(data.table)
library(PortfolioTesteR)

# ⬇️ defines ml_backtest_multi() and pt_collect_results()
source("ml_helpers.R")

# ---- Config ---------------------------------------------------
set.seed(42)
DB_PATH      <- "sp500_history.db"
SYMBOLS_FILE <- "recommended_symbols_2014_2024.txt"
SECTORS_FILE <- "sp500_sectors.csv"

START_DATE <- "2014-01-01"
END_DATE   <- "2024-01-01"
SPLIT_DATE <- as.Date("2020-01-01")
HORIZONS   <- c(1L, 4L, 13L)
SCHEDULE   <- list(is = 156L, oos = 4L, step = 4L)

COST_GRID  <- c(5, 10, 15, 20, 25, 50)
TOP_K      <- 20L
CAP_NAME   <- 0.10
MAX_PER_GROUP <- 3L            # sector cap (mpg = 3)
INIT_CAPITAL  <- 1e5
NAME_PREFIX   <- "Paper-lite "

# ---- Universe & data -----------------------------------------
symbols <- readLines(SYMBOLS_FILE, warn = FALSE)
univ    <- symbols[!symbols %in% c("SPY","^VIX")]

weekly_prices <- PortfolioTesteR::sql_adapter_adjusted(
  db_path      = DB_PATH,
  symbols      = univ,
  start_date   = START_DATE,
  end_date     = END_DATE,
  frequency    = "weekly",
  use_adjusted = TRUE
)

cat("Weekly rows:", nrow(weekly_prices), "\n")
stopifnot(nrow(weekly_prices) >= (SCHEDULE$is + SCHEDULE$oos))

syms    <- setdiff(names(weekly_prices), "Date")
sectors <- data.table::fread(SECTORS_FILE)[Symbol %in% syms]
gmap_UP <- unique(sectors[, .(Symbol, Group = Sector)])

# ---- Features (leakage-safe weekly) ---------------------------
X <- PortfolioTesteR::ml_prepare_features(
  prices_weekly = weekly_prices,
  include = c("mom12","mom26","mom52","rsi14","dist20")
)

# ---- Deterministic model --------------------------------------
model <- if (requireNamespace("xgboost", quietly = TRUE)) {
  PortfolioTesteR::ml_make_model(
    "xgboost",
    params  = list(
      objective = "reg:squarederror",
      eta       = 0.05,
      max_depth = 4,
      subsample = 1.0,          # deterministic
      colsample_bytree = 1.0,   # deterministic
      nthread   = 1             # deterministic
    ),
    nrounds = 200L
  )
} else if (requireNamespace("glmnet", quietly = TRUE)) {
  PortfolioTesteR::ml_make_model("ridge")
} else {
  PortfolioTesteR::ml_make_model("linear")
}

# ---- Backtests (two calls) ------------------------------------
set.seed(42)
bt_pooled_list <- ml_backtest_multi(
  features_list   = X,
  prices_weekly   = weekly_prices,
  horizons        = HORIZONS,
  fit_fn          = model$fit,
  predict_fn      = model$predict,
  schedule        = SCHEDULE,
  transform       = "zscore",
  selection       = list(top_k = TOP_K),
  weighting       = list(method = "softmax", temperature = 12),
  caps            = list(max_per_symbol = CAP_NAME),
  group_mode      = "pooled",
  initial_capital = INIT_CAPITAL,
  name_prefix     = NAME_PREFIX,
  seed            = 42
)

set.seed(42)
bt_neutral_list <- ml_backtest_multi(
  features_list   = X,
  prices_weekly   = weekly_prices,
  horizons        = HORIZONS,
  fit_fn          = model$fit,
  predict_fn      = model$predict,
  schedule        = SCHEDULE,
  transform       = "zscore",
  selection       = list(top_k = TOP_K, max_per_group = MAX_PER_GROUP),
  weighting       = list(method = "softmax", temperature = 12),
  caps            = list(max_per_symbol = CAP_NAME, max_per_group = NULL),
  group_mode      = "per_group",
  group_map       = gmap_UP,
  initial_capital = INIT_CAPITAL,
  name_prefix     = NAME_PREFIX,
  seed            = 42
)

# quick health prints
for (h in HORIZONS) {
  key <- paste0("H", h, "w")
  cat(key, "scores rows (pooled):",   nrow(bt_pooled_list[[key]]$scores),  "\n")
  cat(key, "weights rows (neutral):", nrow(bt_neutral_list[[key]]$weights), "\n")
}

# ---- Collect all series/tables (no plotting here) -------------
results <- pt_collect_results(
  bt_pooled_list  = bt_pooled_list,
  bt_neutral_list = bt_neutral_list,
  weekly_prices   = weekly_prices,
  horizons        = HORIZONS,
  split_date      = SPLIT_DATE,
  cost_bps        = COST_GRID,
  freq            = 52
)



## ========= Chapter 3 figures (print to plot pane) =========

# Horizon for the equity/IC figures
key <- "H13w"



# Labels
Y13 <- PortfolioTesteR::make_labels(prices = weekly_prices, horizon = 13L, type = "log")
Y4  <- PortfolioTesteR::make_labels(prices = weekly_prices, horizon  = 4L,  type = "log")

# Rebuild masked scores for H=13 from the actual backtests (exactly as used in the text)
S_neutral_oos <- scores_oos_only(bt_neutral_list[[key]]$scores, bt_neutral_list[[key]]$weights)
S_pooled_oos  <- scores_oos_only(bt_pooled_list [[key]]$scores,  bt_pooled_list [[key]]$weights)

# A tiny caption utility so titles are compact and never collide
caption <- function(txt, line = 0.5, cex = 0.9) mtext(txt, side = 3, adj = 0, line = line, cex = cex)

## -------- Figure 3.5 — Rolling IC (26w), sector-neutral (OOS-only, H=13) --------
PortfolioTesteR::ml_plot_ic_roll(S_neutral_oos, Y13, window = 26)
caption("Fig. 3.5 — Neutral OOS IC (26w, H=13)")

## -------- Figure 3.6 — Rolling IC (26w), pooled (OOS-only, H=13) --------
PortfolioTesteR::ml_plot_ic_roll(S_pooled_oos, Y13, window = 26)
caption("Fig. 3.6 — Pooled OOS IC (26w, H=13)")

## -------- Figure 3.7A — OOS equity & drawdown (sector-neutral, H=13) --------
op <- par(no.readonly = TRUE); par(oma = c(0,0,1.2,0))  # outer top margin for a neat caption
plot(bt_neutral_list[[key]]$backtest, type = "performance")
mtext("Fig. 3.7A — OOS Equity & Drawdown (Neutral, H=13)", side = 3, line = 0.2, cex = 0.9, outer = TRUE)
par(op)

## -------- Figure 3.7B — OOS equity & drawdown (pooled, H=13) --------
op <- par(no.readonly = TRUE); par(oma = c(0,0,1.2,0))
plot(bt_pooled_list[[key]]$backtest, type = "performance")
mtext("Fig. 3.7B — OOS Equity & Drawdown (Pooled, H=13)", side = 3, line = 0.2, cex = 0.9, outer = TRUE)
par(op)

## -------- Figure 3.7C — Daily performance diagnostics (Neutral, H=13) --------
syms <- setdiff(names(weekly_prices), "Date")
daily_px <- PortfolioTesteR::sql_adapter_adjusted(
  db_path = DB_PATH, symbols = syms,
  start_date = START_DATE, end_date = END_DATE,
  frequency = "daily", use_adjusted = TRUE
)
pa_neutral <- PortfolioTesteR::analyze_performance(bt_neutral_list[[key]]$backtest, daily_px)
op <- par(no.readonly = TRUE); par(oma = c(0,0,1.2,0))
plot(pa_neutral, type = "summary")  # 4 panels: equity, DD, rolling vol, cum. returns
mtext("Fig. 3.7C — Daily Diagnostics (Neutral, H=13)", side = 3, line = 0.2, cex = 0.9, outer = TRUE)
par(op)

## -------- Figure 3.7 — Feature IC (26w), H=4: mom52 --------
PortfolioTesteR::ml_plot_ic_roll(scores_dt = X$mom52, labels_dt = Y4, window = 26)
caption("Fig. 3.7 — Feature IC (mom52, 26w, H=4)")

## -------- Figure 3.8 — Feature IC (26w), H=4: rsi14 --------
PortfolioTesteR::ml_plot_ic_roll(scores_dt = X$rsi14, labels_dt = Y4, window = 26)
caption("Fig. 3.8 — Feature IC (rsi14, 26w, H=4)")
