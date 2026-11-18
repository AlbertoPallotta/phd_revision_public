# PortfolioTesteR 0.1.4.9008

## Bounded thesis example and documentation restorations

- Add a separate Top-5 OR multifactor recipe with explicit propagated momentum
  and positive-finite reciprocal-volatility allocation, retaining the 60/40
  normalised blend and the grouped four-week grid.
- Supply the fixed B3 grid/WF recipe and companion reproduction instructions.
  Its explicit momentum availability and pre-OOS history distinguish the new
  illustrations from legacy examples and unidentified historical figures.
- Correct price-input/rank terminology in weight_by_volatility examples and
  README, and use distinct IS/OOS columns in the diagnostic examples/help.
- Correct the README weekly sample coverage to the unchanged bundled dates.
- No active production function, signature, export, default, data, ML, cost,
  stop or window-architecture change. Existing result identities are preserved.
- example_multi_factor.R and example_risk_parity.R retain known stale calls;
  successful execution of every standalone example has not been validated.

# PortfolioTesteR 0.1.4.9007

## Momentum availability documentation

- Include the opt-in momentum availability policy in the README candidate scope.
- Clarify that, when at least one but fewer than n assets are available, Top-N
  selects all available assets and equal weighting renormalises their target
  weights to sum to one. This documents existing selection/weighting behaviour.
- No executable code, default policy, strategy, reporting, tests or data change
  relative to 0.1.4.9006. Its numerical results retain their execution identity.

# PortfolioTesteR 0.1.4.9006

## Explicit momentum availability

- Add opt-in `calc_momentum(..., na_policy = "propagate")`: missing/non-finite
  endpoints, zero lagged denominators and non-finite returns remain unavailable.
  Genuine zero returns remain valid. Existing calls retain `legacy_zero` exactly.
- The selected getting-started momentum illustration uses the explicit policy.
  Benchmark recipes and ML feature helpers retain their existing defaults.
- The Chapter 4 companion opts in for four identified momentum recipes and
  separately records changed results. This is not a global default migration.
- No change to reporting, execution, filters, weighting, costs or bundled data.

# PortfolioTesteR 0.1.4.9005

## Reporting documentation

- Define the existing Sortino denominator and unavailable cases in
  backtest_metrics() help; list the two added reporting fields in its return value.
- Clarify that summary() return-distribution statistics retain the full stored
  return vector, including warmup rows and the initial synthetic zero.
- No executable code, reporting calculation, strategy, input data or test changes
  relative to 0.1.4.9004. The annualisation correction is unchanged.

# PortfolioTesteR 0.1.4.9004

## Backtest reporting annualisation

- Correct elapsed-date annual return and full-window purchase turnover in
  backtest reporting; remove the fixed weekly assumption from these reporters.
- Share the reporting calculation with backtest_metrics(), fixing its warmup
  branch's use of an uninitialised frequency variable.
- Expose an optional periods_per_year risk-scaling argument in print() and
  backtest_metrics(), with an observed-interval default and explicit
  unequal-interval approximation label. Exclude the artificial starting zero
  from annualised risk calculations; document windows and unavailable cases.
- Preserve existing risk-ratio definitions, trading calculations, schedules,
  result objects, cost processing and ML methodology. Annualised
  reporting changes, including on weekly data; total returns remain unchanged.

# PortfolioTesteR 0.1.4.9003

## Stop-loss date compatibility

- Preserve the Date class while iterating stop-monitoring dates. StopLoss
  transaction rows now retain dates before binding, avoiding mixed-class failures
  with older data.table versions. The change does not alter stop thresholds,
  prices, sizing, cash accounting or rebalancing rules.
- Add a triggered-stop regression test that checks transaction dates both before
  and after binding, so newer data.table coercion cannot hide the defect.

# PortfolioTesteR 0.1.4.9002

## Explicit transaction cost boundary

- Clarify that portfolio_returns(cost_bps > 0) is not operational: it warns and
  returns gross returns; pt_collect_results() supplies the separate documented
  turnover-proxy cost sweep. Engine-consistent cost deduction is not implemented
  by this path. No cost deduction or numerical calculation changes.
- Replace the misleading turnover-helper warning diagnosis, retaining a warning
  rather than an error. Align the help for portfolio_returns(), tune_ml_backtest()
  and wf_sweep_tabular(), and explain that ml_backtest() has no cost_bps argument.

# PortfolioTesteR 0.1.4.9001

## Compatibility and scope documentation

- Preserve Date classes in the three ML panel intersections for compatibility
  with R versions whose set operations remove date classes. Add alignment and
  sequence regression coverage without changing refit or holding schedules.
- Declare testthat >= 3.1.7 for the optional-backend tests.
- Complete the scope matrix and clarify same-row execution and caller-supplied
  decision lags. Align risk-weighting help with the implemented approximations.
- Document the inoperative portfolio_returns(cost_bps > 0) path in its help and
  README. Cost calculations, the thesis collector and research inputs remain
  unchanged; these are documentation changes only.
- Clarify that the recovered research inputs have no public location supplied
  by this package; complete historical-environment verification is not
  established by the supplied records.

# PortfolioTesteR 0.1.4.9000

## PhD examiner amendments

- Give actionable errors for absent optional tabular ML packages.
- Announce the existing sequence linear fallback and expose requested/actual
  model types without changing fit/predict signatures or fitting calculations.
- Keep optional-backend tests conditional and retain backend-free ensemble tests.
- Render the advanced vignette with an available, correctly named baseline and
  label sequence examples by the selected factory type.
- Add the implementation scope matrix, execution/cash/cost assumptions,
  survivorship disclosure and precise GitHub source-reference instructions.
- Add an opt-in environment/input recording script for new reproduction runs.

This candidate preserves the historical thesis helper and numerical strategy
implementation. Broader methodological and numerical corrections are outside
this amendment. Version 0.1.4 and its existing tag remain historical references.

# PortfolioTesteR 0.1.4

## New
- `ml_backtest_multi()` — run ML backtests across multiple horizons/schedules.
- `pt_collect_results()` — one-call collector for coverage, IC (raw & OOS-only), turnover, perf tables, and cost sweeps.
- `scores_oos_only()` — keep scores only on OOS decision dates.

## Improvements
- Vignettes stabilized on knitr/rmarkdown (no Quarto dependency during checks).
- Expanded `R/globals.R` to silence data.table NSE NOTES.
- Regenerated docs with UTF-8 to avoid Non-ASCII warnings.

## Compatibility
- No breaking API changes.
