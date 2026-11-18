# Execution and reproduction guide

## Preparing inputs for a first strategy

### Example preparation

The [README](README.md#first-example-moving-average-crossover) and
[ma_crossover.R](inst/examples/ma_crossover.R) contain the complete moving-average
example. It aggregates the supplied 15-minute observations, including gaps, into
daily strategy bars. Its 20/50 crossover, opposite-crossover exit and 2% stop use
the familiar `my_strat(price, ...)` and named-column grammar. The stop is based on
the scheduled next opening. Ask-derived `NextOpen` is also used for signal exits;
the synthetic spread does not imply a full spread charge on every transaction.

### Supplementary scheduled partial-exit demonstration

This separate example uses the first 160 bundled observations, a synthetic spread
of 0.04 and two price-unit quantities. It schedules three entries and closes half
of each position before its remaining half. Bollinger bands provide display
context; they do not select these entries. Their 20-observation calculation has
19 warm-up observations. This short example demonstrates the interfaces, not a
thesis strategy or a meaningful annualised empirical study.

Run this block in a fresh R session using the library selected in INSTALL.txt:

```r
library(TradeTesterR)
library(TTR)

demo <- new.env(parent = globalenv())
sys.source(system.file("examples", "csv_demo_setup.R",
                       package = "TradeTesterR", mustWork = TRUE), envir = demo)
ex <- demo$ex
bt <- backtest(ex$dat, exit_fun = exit_TP_SL_signal,
               CloseTradeOnOppositeSignal = FALSE, AddToPosition = FALSE,
               verbose = FALSE)
analyze_performance(bt, initial_equity = 10000)
plotEquityCurve(bt, initial_equity = 10000)
plotSingleTrade(bt, ex$price, trade_num = 1, window_days = 10,
                indicators = c("upperBB", "lowerBB"))
```

Its [setup file](inst/examples/csv_demo_setup.R) retains the complete preparation
and bundled-input checks. The wrapper keeps its preparation objects in `demo`;
the main README example shows all moving-average preparation directly.

### Fields and defaults

These are different roles; the setup file's complete column list is not a list
of mandatory inputs for every strategy.

| Role | Fields and behaviour |
|---|---|
| Core prepared input | Unique, nonmissing POSIXct `date`, signed `OrderSize`, and the ask/bid OHLC fields needed by the entry and exit rules (`Open.a` through `Close.a`, and `.b` counterparts). Supply both complete OHLC sides for the default workflow. Raw OHLC alone is not this execution table. |
| Entry events | Nonzero `OrderSize` emits an instruction; zero or `NA` does not. Positive size buys, negative size sells short. Do not forward-fill it across execution rows. |
| Defaults when entire columns are absent | `EntryType` defaults to `"Market"`; `EntryTime` uses the next strategy boundary or execution timestamp; `EntryPrice` is derived from the appropriate ask/bid opening. These defaults do not repair missing values within an explicitly supplied column. Supply the intended threshold for a limit or stop order. |
| Optional fixed exits | `TakeProfit` and `StopLoss` are price levels; omitting these columns disables the corresponding fixed bracket. `TradeTimeLimit`, signal exits and the run boundary are separate mechanisms. |
| Conditional exit inputs | Scheduled `exit_long`/`exit_short` fractions use `NextOpenTime` and a matching observed opening price in `NextOpen`. Numbered target/stop columns require their corresponding `_amount` and `NextOpenTime` fields. `CLOSE_CANDLES_SINCE_ENTRY` needs the sparse `NextOpen` clock. |
| Optional timing controls | `EntryValidUntil`, `CancelOrder` and `end_time` define additional expiry/cancellation/horizon rules; see the [clock and execution conventions](#instructions-states-and-clocks). |

The **strategy clock** defines when rules become available; the **execution clock**
contains the bars on which orders can fill. The main moving-average example
uses daily strategy bars and the supplied 15-minute execution bars; the clocks
coincide in the supplementary scheduled example.
For coarser strategy rules joined to finer bars, keep `NextOpenTime`/`NextOpen`
as sparse clock markers and do not forward-fill them. Any state alignment must
respect availability; finer execution bars cannot make future inputs available.
The [clock and execution conventions](#instructions-states-and-clocks) explains default expiry,
same-bar assumptions and why entry instructions differ from persistent state.

`OrderSize` fixes the P&L units: the engine computes
`Side * (ExitPrice - EntryPrice) * abs(ExitAmount)` without a separate contract
multiplier. The main example uses 4 × 50 = 200 price-unit exposure; the
supplementary scheduled example uses two price-unit quantities. For futures, state
whether the multiplier is encoded in quantity or applied once in post-processing;
do not apply it twice. `getOrderSize(type="Futures")` returns contract-count
arithmetic, not automatic engine multiplier or margin handling. See
[units and performance](#units-and-performance).

The quick start explicitly disables opposite-signal closure; the public default
is `TRUE`. `AddToPosition=FALSE` is the default, and the default time limit is four
weeks from activation. The moving-average recipe explicitly sets
`TradeTimeLimit = lubridate::weeks(100000)`; this retains its existing time-limit
setting rather than imposing the four-week default. Equity/performance summaries
use realised exit P&L, not a
complete mark-to-market account. An exit-leg row is not necessarily a whole trade;
group `Order_ID` within its run and inspect remaining quantities.

### Reading the plots

`plotTrades(trades=...)` and `plotSingleTrade(trade_num=...)` index rows of the
exit ledger. Partial exits therefore give multiple rows for one `Order_ID`, and
the single-row title describes that leg's P&L. In the supplementary scheduled example,
`trade_num=1` displays
the first half exit. The `$` labels in generic plots do not apply a currency
conversion; interpretation still follows the quantity units above.

In this snapshot, `plotTrades()` repeats the last requested indicator series
when several indicators are supplied. The earlier diagnostic call with three Bollinger indicators
produced traces containing only the middle-band series, rather than distinct
upper, lower and middle bands. Do not use that call for a multi-band display.
The supplementary `plotSingleTrade()` route retains the two-band call checked
in the prior example evidence. A different input or overlay needs its own check.
The main example instead uses `plotEquityCurve()`. See the
[documentation checks](../../reproduction/SOFTWARE_PROVENANCE.md#documentation-example-checks).
The multi-indicator plotting limitation remains unfixed.

## Instructions, states and clocks

An order instruction is an event. `OrderSize`, entry fields and attached fixed TP/SL are emitted once. The engine then copies that fixed bracket over the bars evaluated for the trade. A missing ATR on other bars is not, by itself, evidence of wrong execution.

The internal `.align_daily_rules()` has a separate state allowlist. It selects the last rule with `AvailableAt <= execution date` and retains it only before `ValidUntil`. Its state behaviour is a bounded backward as-of join. It never applies LOCF to `OrderSize`. It preserves the execution timestamps and rejects duplicate or overlapping rules. The reproduction's `RuleTime` and `OrderRuleTime` identify the source observation; `EntryTime` is the actual activation/fill timestamp, and `ExitTime` is used for realised P&L.

Unfilled entries expire at the end of the strategy bar in which they become eligible, even when no new signal appears. A new signal creates a new instruction at its newly calculated level. This supersedes the earlier source snapshot's GTC choice. Sparse `NextOpenTime` or `NextOpen` markers define the strategy clock; with neither, execution bars are the only available clock. With no supplied strategy clock, an activation between execution timestamps is eligible from the first observed opening at or after activation and expires at the following execution boundary. This fallback never extends a supplied strategy boundary or `EntryValidUntil`, and never overrides cancellation, opposite-signal closure, TTL or the run horizon. The audit retains the original activation time. Do not forward-fill these clock markers. Four-hour and weekly clocks are tested as well as daily clocks. The optional `EntryValidUntil` timestamp supplies an explicit exclusive expiry; the alignment helper sets it from `ValidUntil`. Entry expiry and `CancelOrder` never remove the fixed protection of a filled position. An instruction without any observed execution bar in its interval remains unfilled or is reported unplaced by the alignment helper.

The supplied ES file is aggregated by its observed New York calendar dates. A completed daily rule becomes available at the next observed daily opening timestamp. State expires at the following local calendar midnight. Sunday reopenings can consequently have a short validity interval ending at local midnight, rather than lasting through Monday. This calendar-day convention is retained deliberately; it is not an exchange-session calendar. The final daily row cannot generate a next-session instruction without an observed activation time. Orders with nonfinite ATR, TP or SL are excluded and recorded, rather than allowed to enter without the intended bracket.

`NextOpen` remains a sparse daily clock marker. The ES adapter writes the constant 1 on exact observed daily marker rows; the time-exit code only tests whether it is missing. This avoids exposing a future opening price as persistent state. A missing marker bar is reported, not silently replaced by a five-minute tick. Other legacy strategies may still use actual `NextOpen`/`NextOpenTime` for their supplied signal exits. Their causal preparation remains the caller's responsibility.

## Entry, exit and window policies

Market entries use a supplied price or a matched opening price. A buy stop already crossed at an eligible opening fills at ask open; a sell stop fills at bid open. A stop first touched later inside the execution bar retains the threshold-price approximation. A buy limit already marketable at an eligible opening fills at ask open at or below its limit; a sell limit uses bid open at or above its limit. Later intrabar touches retain the limit threshold. There is no general slippage, commission, liquidity, partial-entry-fill, broker rejection or margin model. The model does not infer the order of all movements within an OHLC bar.

For a held long position, fixed/dynamic protective stops and targets use bid prices for both triggering and closing; for a short position, they use ask prices. An order already executable at an eligible opening uses that opening quote. This gives crossed targets opening-price improvement as well as crossed stops their available opening fill. This is the package's explicit quote-based trigger convention; live brokers and exchanges may instead use last-trade or other trigger methods. Updating a dynamic level only after a strategy bar closes remains a deliberate strategy rule. The update takes effect at its supplied `NextOpenTime`; it never applies retrospectively. A separate fixed `StopLoss`, if supplied, remains attached to its trade. An earlier intrabar stop touch is not postponed to the next opening. A position first entered intrabar cannot receive a protective exit at that same bar's earlier opening. This convention models immediate execution of an already-crossed eligible stop; it does not simulate broker acceptance or rejection.

Use execution data sufficiently fine for the strategy's order sequencing. Finer bars reduce, but do not eliminate, unresolved intrabar movement; finer data cannot eliminate actual price jumps or make a new instruction effective before its inputs were available. The limit/stop entry threshold remains in `bt$orders$EntryPrice`, while `bt$results$EntryPrice` records the actual modelled fill. Fixed protective price levels are not rebased to a changed entry fill, so a monetary amount used to construct them is not a guaranteed realised loss.

In single-position mode (`AddToPosition=FALSE`), entries compete while flat. The first eligible fill in chronological execution-bar order wins and cancels the other active alternatives. New entry activations while a position remains open are ignored, including while it is partially closed. Future fills cannot retrospectively remove earlier completed trades. At a shared timestamp, expiry/cancellation is processed before new activation, opening fills before touches, and an unresolved same-phase tie uses stable instruction order. The selected trade has `AmbiguousEntry=TRUE`; `bt$orders` records the other candidate IDs and the winning order. This tie rule is an assumption, not observed price chronology.

A protective exit prevents re-entry on that same execution bar, including when it is priced at the opening; a new instruction on a later bar can enter. Opening-versus-touch ordering does not relax this entry restriction. Scheduled signal and boundary exits retain their existing admission rules. The legacy opposite-signal option can close at a boundary and permit the opposite entry there. An opposite signal is identified by signed nonzero size and finite activation time, independently of whether its entry price is executable: an invalid opposite entry can therefore close the current trade when this option is TRUE. Set it to FALSE to disable that behaviour, as the thesis recipes do. Public defaults remain `CloseTradeOnOppositeSignal=TRUE` and `AddToPosition=FALSE`; the thesis recipes explicitly disable opposite-signal closure. `AddToPosition=TRUE` evaluates overlapping entries independently, without an aggregate position or competing-entry cancellation.

`bt$orders` has one row per nonzero entry instruction, including runs with no fills. It records source/activation times, expiry, disposition, disposition time, winner and ambiguity. It is an instruction audit, not an exchange execution report. `bt$results` remains the exit-leg ledger and `bt$data` remains the inspection table. A strategy table still supplies at most one instruction per timestamp.

For the ES illustration, unresolved same-phase fixed TP/SL collisions are resolved at SL, with `AmbiguousBar=TRUE` and `ExitReason="StopLoss (same-bar assumption)"`. If one eligible bracket order is already crossed at the opening and the other requires later movement, the opening resolves their order without this flag. Targets first touched later within a bar retain the threshold approximation; eligible opening targets use the closing-side opening quote. The stop-first convention is an author-selected assumption, not proof of the actual intrabar sequence, a guaranteed bound, or a solution to entry-before-exit ordering within the fill bar. The public exit function retains `ambiguity="unknown"` as its compatibility default; unknown returns cause performance and plot functions to fail explicitly. The illustration opts in with `exit_args=list(ambiguity="stop_first")`.

Long fixed/trailing stops trigger on bid low and short stops on ask high. Fixed/dynamic targets use bid high for long and ask low for short. A trigger-side change can advance an exit and change later trade selection; its total P&L effect is not bounded by one spread. These are explicit bar-based conventions, not a general broker fill model. Terminal short closes now use ask close, and terminal long closes use bid close.

Scheduled signal exits use an actually observed opening timestamp and the supplied `NextOpen`, checked against that opening's ask/bid quotes. Exits are ordered by execution time and then opening before touch, before remaining quantities are calculated. An earlier protective hit therefore precedes a later scheduled signal. A valid next-session opening survives a weekend gap; an unavailable future opening cannot contribute its price beyond an exclusive run horizon. Numbered dynamic exits retain the existing remaining-fraction convention and one dynamic exit per execution bar: an opening precedes a later touch; within the same phase, later declared components replace earlier ones, with stops processed after targets. Dynamic levels before a filled trade's evaluated slice are not automatically reconstructed; this remains a limitation to inspect.

`CLOSE_CANDLES_SINCE_ENTRY=N` uses the `(N+1)`th nonmissing daily marker in the post-fill subset as an exclusive cutoff, preserving the existing counting convention. If that marker is beyond the data horizon, already observed TP/SL hits are still evaluated. If a position remains, it closes on the last available bar, with a distinct end reason. Ordinary full terminal exits are timestamped at the estimated bar close (median execution spacing), capped by the explicit horizon. A directly called exit function without engine terminal metadata uses the last supplied timestamp.

`TradeTimeLimit` is measured from scheduled entry activation, including pending time. `end_time` is an exclusive bar-selection boundary. A terminal close may be timestamped exactly at that boundary using the previous complete bar's close; the opening price at the boundary is excluded. Crossing incomplete bars are excluded using median timestamp spacing. Callbacks must return observed execution times or the permitted terminal boundary and conserve quantity. Missing `CurrentPosSize` is derived; supplied values are checked. Fractional-second timestamps can be represented, but median bar duration is an approximation, especially with irregular tick observations.

The ES adapter clips its bar stream separately for every training/test window and passes the exact horizon to `backtest()`. Positions are liquidated at that run's end; pending orders and holdings are not carried from one independent window into the next. Training and test windows retain the original 156/26-week lengths and 26-week step. Generalisation of overlapping windows remains future work. No default flip of `CloseTradeOnOppositeSignal` or new Kelly API is included.

## Units and performance

The generic engine computes `Side * (ExitPrice - EntryPrice) * abs(ExitAmount)`. Its units depend on `OrderSize`; it has no separate contract-multiplier argument. The ES strategy enters four contracts, keeps `Returns_points` as contract-point P&L, and converts once with an explicit `ContractMultiplier=50` USD per price point. `Returns` in its saved ledger is therefore USD. The bundled CL recipes already encode their chosen multiplier in OrderSize; applying the ES conversion to them again would be wrong. Their numeric multiplier is a recipe assumption, not a verified specification for every contract named CL.

Trade statistics aggregate partial legs by Order_ID. IDs are local to a backtest: include the window/run identity when pooling ledgers. Equity and timeline plots use ExitTime. The daily realised-equity grid now includes calendar days, including Sunday futures closes, and starts its first return and drawdown at initial capital. Its default annualisation factor is 365, changed from 252. Missing/nonfinite returns are errors, never zero-profit trades. A supplied risk-free vector with any absolute value over one is treated as a percent series as a whole; pass consistently scaled inputs.

Sortino uses the root mean square of downside excess returns over the complete daily grid, including zero downside on nonnegative days. These equity statistics are based on realised P&L unless a genuine equity curve is supplied; they do not measure intervening unrealised drawdowns or exposure. If equity reaches zero or becomes negative, `nonpositive_equity=TRUE` and CAGR, volatility, Sharpe and Sortino are NA. Cash P&L, terminal-capital return and drawdowns remain available. This does not implement liquidation, margin or continued-trading feasibility. Irregular external equity curves and mixed/inconsistent risk-free units remain outside the demonstrated validation cases.

Daily performance grouping follows R's `as.Date(POSIXct)` default, UTC. This is distinct from the New York calendar dates used to aggregate the supplied ES strategy bars.

`optimizeStrategy()` accepts a strategy callback and frozen risk-free data. NULL risk-free means zero; it no longer downloads Treasury yields on every call. A legacy `my_strat` in the calling environment remains a compatibility fallback. The callback receives a copy of its window with an `execution_end` attribute. During its synchronous call, `backtest()` also inherits that exclusive end, so legacy strategies using a global full execution feed are bounded without changing their function grammar. The context restores after nested calls or errors; it does not propagate into user-created parallel workers. Custom engines must honour the attribute. Out-of-window returned trades raise an error. Empty results score zero net profit; a window with no defined selected metric fails explicitly. Equal maxima select the first grid row.

Finer execution data reduces unresolved ordering but cannot guarantee a realistic fill, queue position or price. Five minutes is the supplied illustration's resolution, not a package requirement. Choose execution data finer than the strategy clock where possible; inspect ambiguity flags and the execution ledger. Using the same coarse bars for indicators and fills increases path uncertainty, especially for limit/stop entries and protective exits in the entry bar.

`compareISOS()` reports actual IS net profit even when another metric selected the parameters, counts completed trades rather than partial legs, and uses the same recorded performance calculation for OOS Sharpe. `NetProfit_Degradation` and `Performance_Ratio` now compare profit per calendar day. The old unequal-duration raw totals are retained under `Raw_` names. Missing duration metadata gives NA normalised fields. Neither normalisation nor the correlation establishes robustness or removes selection bias.

## Technical evidence

The delivery's [technical evidence index](../../environment/HISTORICAL_TECHNICAL_INDEX.txt)
identifies the original checked artifacts, dates, environments and outcomes.
New installation/example checks must identify their own successor archive.
The stated assumptions and known limits remain: tests cover specified cases,
not every combination of instructions, callbacks and market data.
