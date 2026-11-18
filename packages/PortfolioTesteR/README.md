<h1 align="center">PortfolioTesteR</h1>

<p align="center"><img src="man/figures/logo.png" alt="PortfolioTesteR logo" width="600"></p>

## Test Investment Strategies with English-Like Code

PortfolioTesteR is an open-source R package for exploring investment strategies
with clear, readable code. Choose your data, calculate signals, select assets,
assign weights and inspect the resulting portfolio.

It is designed for students, individual investors and researchers who want to
develop ideas step by step, using functions such as `filter_top_n()`,
`weight_equally()` and `run_backtest()`.

## Why PortfolioTesteR?

- 📝 **Readable strategy steps:** function names describe the choices you make.
- 🎓 **Built for exploration:** examine prices, signals, selections and weights
  as ordinary R tables.
- 🧩 **Combine your own rules:** connect indicators, filters and allocation methods
  to express a portfolio strategy.
- 📊 **Inspect performance:** view trades, residual cash, equity and performance
  summaries, with their calculation assumptions documented below.
- 🚀 **Start with included data:** try the weekly momentum example without
  downloading a dataset.
- 💻 **Free and open source:** use and adapt the package under the MIT licence.

## Installation

Follow [INSTALL.txt](../../INSTALL.txt) to install the version supplied with
this repository. The quick start below uses the bundled sample data. Installation
commands in older vignettes may select a different version.

## Quick Start Example

```r
library(PortfolioTesteR)

# Load sample data
data(sample_prices_weekly)

# Calculate 12-week momentum
momentum <- calc_momentum(sample_prices_weekly, lookback = 12)

# Select top 10 momentum stocks
top_momentum <- filter_top_n(momentum, n = 10)

# Weight selected stocks equally
weights <- weight_equally(top_momentum)

# Run backtest
result <- run_backtest(
  prices = sample_prices_weekly,
  weights = weights,
  initial_capital = 100000,
  name = "Simple Momentum"
)

# View results
print(result)
summary(result)
plot(result)
```

The quick start uses the default `na_policy="legacy_zero"`: the first 12 momentum
observations are zero and can enter Top-N ties. It also uses same-row targets and
prices. Keep these choices explicit when adapting it; the [momentum availability](#momentum-availability)
and [execution and cost](#execution-and-cost-assumptions) sections explain the
existing alternatives and assumptions.

## Main Functions

### Data Loading
- `sql_adapter()` - Load from SQLite databases
- `csv_adapter()` - Load from CSV files
- `load_mixed_symbols()` - Combine different data sources

### Technical Indicators
- `calc_momentum()` - Price momentum
- `calc_rsi()` - Relative Strength Index
- `calc_moving_average()` - Simple moving averages
- `calc_stochastic_d()` - Stochastic oscillator
- `calc_rolling_volatility()` - Rolling historical volatility (std / range / MAD / abs-return)
- `calc_distance()` - Distance between price series

### Stock Selection
- `filter_top_n()` - Select top/bottom N stocks
- `filter_above()` - Select above threshold
- `filter_below()` - Select below threshold
- `filter_between()` - Select within range
- `combine_filters()` - Combine multiple conditions
- `apply_regime()` - Filter based on market conditions

### Portfolio Weighting
- `weight_equally()` - Equal weight (1/N)
- `weight_by_signal()` - Weight by signal strength
- `weight_by_rank()` - Weight by ranking
- `weight_by_volatility()` - Price-input volatility weighting: rank (default), equal or inverse variance; not reciprocal-volatility weighting
- `weight_by_hrp()` - Hierarchical Risk Parity

### Backtesting & Analysis
- `run_backtest()` - Execute strategy backtest
- `analyze_performance()` - Additional analytics using a backtest result and a daily-price table
- `print()` - Display backtest results
- `summary()` - Show detailed statistics
- `plot()` - Visualize performance

## Sample Data Included

The package exposes these three R data objects for immediate testing:
- `sample_prices_weekly` - Weekly prices for 20 instruments (6 January 2017 to 31 December 2019)
- `sample_prices_daily` - Daily prices for detailed analysis
- `sample_sp500_sectors` - Sector classifications

It also retains `inst/extdata/sp500_sectors.csv` and `inst/extdata/sp500_sectors.rds`.
The sample data are for exploring the package. The larger database used in the
thesis research is not included.

Load them with:
```r
data(sample_prices_weekly)
data(sample_prices_daily)
data(sample_sp500_sectors)
```

## Documentation

Use the function help pages for interface documentation; selected topics also include examples. Prices, signals, selections and weights use a `Date` column and instrument columns; check each function's help for its input and alignment rules. Start with [run_backtest](man/run_backtest.Rd), [calc_momentum](man/calc_momentum.Rd) and [filter_top_n](man/filter_top_n.Rd):

```r
?run_backtest
?calc_momentum
?filter_top_n
```

## Disclaimer

This package is for educational and research purposes only. Past performance does not guarantee future results. Always conduct your own research before making investment decisions. The author is not responsible for any financial losses incurred from using this software.

## Author

Alberto Pallotta

## License

MIT License

## Contributing

We welcome contributions! Please feel free to submit issues and pull requests on [GitHub](https://github.com/AlbertoPallotta/PortfolioTesteR).

## Implementation scope

The package's educational accessibility is a design aim; reduced cognitive load or
superiority over other tools has not been established by controlled user studies.

The table summarises the capabilities supplied by this version and their principal assumptions. An implemented interface does not establish economic validity for every dataset or strategy.

| Method or capability | Implementation | Boundary |
| --- | --- | --- |
| Price adapters and indicators | Implemented | Operate on supplied prices; adjustment provenance and universe history remain input requirements. |
| Ranking, filtering and top-N selection | Implemented | Sector/group operations use caller-supplied mappings. |
| Sector-relative indicators and market/sector breadth | Implemented | `calc_sector_relative_indicators()`, `rank_within_sector()`, `calc_market_breadth()` and `calc_sector_breadth()` use supplied classifications and observations; they do not reconstruct historical sector membership. |
| Equal, signal, rank and inverse-volatility weights | Implemented | Each method has its own input and normalisation rules. |
| Portfolio volatility targeting | Implemented as a weight transformation | `vol_target()` scales weights using estimated portfolio volatility; its default down-only mode adds CASH. This does not add borrowing or margin accounting to the engine. |
| Hierarchical risk parity | Implemented with assumptions | Clustering and recursive allocation depend on the covariance sample and lookback. |
| Equal risk contribution | Approximation | Iterative updates have no robust long-only or convergence guarantee. |
| General maximum diversification | Not implemented as an optimiser | The existing `max_div` branch returns inverse-volatility weights and ignores correlations. |
| Global minimum-variance optimisation | Not implemented | General covariance-based minimum-variance optimisation is not supplied. |
| Position and exposure caps | Limited implementation | Position-count limits differ from weight caps; upward renormalisation can undo weight caps. |
| Binary regime filtering | Implemented | Regime dates must align with the selection panel. |
| Multiple-regime classification and weighting | Implemented | `create_regime_buckets()` creates regime labels and `weight_by_regime()` selects configured weighting rules; callers must align regimes with selection rows. |
| Fractional regime exposure | Limited implementation | Scaling selections does not guarantee the same final exposure after weighting and normalisation. |
| Portfolio execution and cash | Implemented for the intended long-only workflow | Whole-share trades, cash-constrained purchases and residual cash; unsupported target weights are not comprehensively rejected. |
| Cash interest, borrowing, margin and complete short-sale accounting | Not implemented | Residual cash earns zero. Signed-weight utilities do not establish short-sale accounting. |
| Integrated execution fees, slippage and market impact | Not implemented in `run_backtest()` | The engine reports gross results. |
| Turnover-based cost summaries | Separate post-processing | `pt_collect_results()` deducts a target-weight turnover proxy; this is not executed traded notional. The separate `portfolio_returns(cost_bps > 0)` path is not operational in this version (warns, returns gross); cost handling is the separate `pt_collect_results()` post-processing step. |
| Portfolio performance and benchmark-relative analytics | Implemented with assumptions | Sharpe, Sortino, drawdown and benchmark-relative summaries depend on the supplied return series, evaluation period, annualisation and benchmark alignment. |
| Multiple-testing corrections | Not implemented | White Reality Check, SPA and Deflated Sharpe Ratio corrections are not built in; parameter search does not provide them. |
| Parameter grids and walk-forward evaluation | Implemented with assumptions | Overlapping windows require an explicit interpretation; performance-decay modelling is not implemented. |
| Tabular ML | Implemented with optional backends | Linear uses base R; ridge, RF and XGBoost require their respective optional packages. |
| Sequence ML | Optional implementation | GRU/LSTM/CNN require a usable Keras backend. Otherwise the factory announces linear fallback and exposes the actual type. |
| Cross-sectional rank information coefficient | Implemented | `ml_ic_series_on_scores()` defaults to Spearman correlation on supplied score/label rows; the caller must restrict evaluation to the intended formation/OOS dates. |
| Independent ML portfolio holding calendar | Not supplied by the ML wrapper | Predictions on each input row become allocation opportunities; forecast horizon and refit step are different choices. |
| Historical index membership and delisting-return service | Not implemented | Universe membership, classification history and delisting treatment require external data/policies. |
| Unified regime overlay and performance-decay extensions | Future research | These were identified as optional extensions, not implemented capabilities. |

## Conventions behind the thesis illustrations

The sections below document the reporting, execution, cost, dependency and data
conventions used for the thesis illustrations, and the known limitations. They are
reference material: the quick start above does not depend on them.

## Annualisation of backtest reports

`print(result)`, `summary(result)` and `backtest_metrics(result)` use the
following conventions. Reporting does not change the underlying trades,
weights, cash or equity.

- **Annual return:** CAGR from the first trading valuation to the final
  valuation, with elapsed years calculated as calendar days divided by 365.25.
- **Annual purchase turnover:** total purchase notional divided by arithmetic
  mean equity and elapsed years across the full valuation window, including
  warmup. Initial purchases and both endpoints count. This is different from
  half of all traded notional and from the target-weight cost proxy below.
- **Annual volatility:** sample standard deviation of the active
  valuation-to-valuation simple returns, excluding the initial stored zero,
  multiplied by the square root of the sampling rate. By default, the rate is
  the number of observed return intervals divided by elapsed active years.
  Unequal intervals are flagged: this scaling does not resample returns or
  adjust each interval separately.
- **Choosing a rate:** for a conventional four-week rate, use
  `print(result, periods_per_year = 13)` or
  `backtest_metrics(result, periods_per_year = 13)`. This changes risk scaling,
  not date-based CAGR or turnover. It does not change the trading calendar,
  model-refitting schedule or prediction horizon. Four-week groups are not
  calendar months.
- **Risk ratios:** the reported Sharpe-labelled ratio is CAGR divided by
  annualised standard deviation, with a zero risk-free rate. It is not the
  arithmetic-mean excess-return definition. Sortino uses the analogous
  negative-return standard deviation convention.
- **Short or invalid series:** a single valuation gives `NA` annual return
  and turnover. Volatility needs at least two return intervals; zero volatility
  gives an `NA` ratio. The reporters reject duplicate, unordered or missing dates.

`backtest_metrics()` also returns `annual_purchase_turnover` and an
`annualisation` record showing the windows, rate and convention. Other analytics
functions and ML collectors have their own documented conventions.

## Execution and cost assumptions

The workflow separates data, signals/models, selection, weights and execution.
`run_backtest()` considers the supplied targets on each price row. Targets
on row t are filled at row t's supplied prices, with no engine-side decision lag.
Any required lag must already be present in signals or weights, for example via
`panel_lag()`. `ml_prepare_features()` lags features by one period; the quick-start
momentum chain above does not add a lag. Examples using same-row signals and
prices therefore require an explicit information-availability assumption. Repeating
target weights can generate trades as prices move; it does not mean holding
shares unchanged. On a weekly input grid, fitting an ML model every four rows
still permits weekly allocation updates. Prediction horizons and refit steps
must not be described as portfolio holding periods.

The engine tracks whole-share positions and residual cash, processes sales
before purchases, and limits purchases to available cash. Residual cash has a
return of zero. Portfolio-return descriptions must include its weight; zero
cash return is not an assumption of zero cash allocation. In `summary(result)`,
“Periods fully invested” counts observations with cash below 1% of initial
capital; it does not mean zero cash.

The thesis illustration runs gross backtests and then applies `pt_collect_results()` cost processing. Its default charge is `(bps / 10000) * turnover`, where turnover is half the sum of absolute changes in target asset weights between successive retained invested allocations. The first invested allocation is compared with zero asset weights.

In the portfolio illustration, transaction costs are approximated from changes in the intended portfolio allocation, rather than actual purchases and sales. This estimate omits the costs of moving entirely into cash, can understate reinvestment costs, and does not capture rebalancing caused by price movements when the intended allocation remains unchanged.

The helper removes all-zero target rows and compares a re-entry with the last invested allocation rather than with cash. The re-entry deduction can therefore be higher or lower than one calculated from cash using the same proxy. Costs are matched by date and deducted from returns after execution; they do not reduce the cash available for trades.

The separate `portfolio_returns(cost_bps > 0)` path is not operational in
this version: it warns and returns gross returns. Its callers
`tune_ml_backtest()` and `wf_sweep_tabular()` inherit that limitation.
`ml_backtest()` has no `cost_bps` argument and reports gross backtest results.
The warning and help direct users to the separate `pt_collect_results()`
post-processing step; none of these functions invokes that collector automatically.

## Optional ML dependencies

Heavy ML packages remain in `Suggests`. Loading PortfolioTesteR and requesting
linear models do not require them. Explicit ridge/RF/XGBoost requests produce an
actionable error when their package is absent. Sequence requests retain their
linear fallback, now with a message and `requested_type`/`actual_type` fields.
No dependency is automatically installed. The advanced vignette chooses and
labels an available baseline; neural sections remain explicitly opt-in.

## Thesis illustration and reproducibility

The thesis portfolio illustration uses a fixed list of current constituents
retrospectively over the requested 2014-2024 interval (observations described in
the runner are 2014-2023). This induces survivorship bias. Absolute performance
figures are illustrative and are not evidence of deployable strategy skill.

The [repository home page](../../README.md) introduces the chapter materials.
Use the [reproduction instructions](../../reproduction/README.txt) to find saved
figures and tables, or redraw the supplied figures locally. The SQLite research
database is not included. Complete study reruns need the original inputs,
software and environment listed in the
[data-availability information](../../DATA_AVAILABILITY.txt).

Research scripts may use different helpers and model backends from the package
examples. Installing another optional ML package can change the backend selected
by the historical runner. Read the
[helper and environment notes](../../reproduction/SOFTWARE_PROVENANCE.md#research-helpers-and-inputs)
before running those scripts.

### Thesis companion examples

The dedicated `inst/examples/thesis_multifactor_A1.R` illustrates Top-5 momentum
OR Top-5 low-volatility selection followed by 60/40 positive-momentum and explicit
reciprocal-volatility allocation over that union. It uses propagated momentum
availability, positive finite volatility, grouped four-week dates (not calendar
months), same-row gross execution and residual cash. An empty allocation leg
renormalises the surviving leg to 100%; two empty legs give zero asset targets.
This recipe has its own selection and allocation rules; see the [example provenance](../../reproduction/SOFTWARE_PROVENANCE.md#portfoliotester) for its distinction from the older multifactor example.

```r
recipe <- new.env(parent = globalenv())
sys.source(system.file("examples", "thesis_multifactor_A1.R",
                       package = "PortfolioTesteR"), envir = recipe)
# Runs the single declared example on unchanged bundled samples:
# a1 <- recipe$thesis_multifactor_a1()
```

The parameter-grid and walk-forward specifications are in
`inst/reproducibility/thesis_recipes_B3.R`. Their saved results and the
multifactor illustration are indexed in the
[Chapter 4 materials](../../reproduction/chapter4/README.txt). Redrawing these
figures uses saved CSVs; rerunning the original studies requires additional
inputs and their original software. The installed
`reproducibility/THESIS_EXAMPLES.txt` describes the fixed strategy specifications.

The package's `.draw_heatmap_grid()` helper can place numeric score labels in
the wrong cells. The supplied thesis heatmap uses a separate exporter whose
labels were checked against all 12 saved scores. See the
[heatmap notes](../../reproduction/SOFTWARE_PROVENANCE.md#example-and-execution-history).

The older `example_multi_factor.R` and `example_risk_parity.R` have known stale
calls; their successful execution has not been validated. Package checks do
not certify every standalone script as runnable.

### Momentum availability

Use `calc_momentum(prices, lookback = 12, na_policy = "propagate")` when
selection must wait for defined endpoint momentum. The first 12 observations
are unavailable; an all-NA signal row produces no Top-N selection. Missing or
non-finite endpoint prices, a zero lagged denominator and non-finite calculated
returns are unavailable; genuine zero returns remain eligible. Intermediate
prices are not part of this endpoint-return availability rule.

The default `na_policy = "legacy_zero"` substitutes zero for unavailable
momentum, including during warmup. Use `na_policy = "propagate"` explicitly when
you want selection to wait; choosing it does not change other indicators,
benchmark recipes or ML feature helpers automatically. The Chapter 4 companion
recipes use this explicit choice. Their results are kept separate from the
earlier runs in the [software record](../../reproduction/SOFTWARE_PROVENANCE.md#example-and-execution-history).

Snapshot details and check history: [software provenance](../../reproduction/SOFTWARE_PROVENANCE.md#portfoliotester).
