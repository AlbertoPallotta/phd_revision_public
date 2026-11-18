# Opening-pricing extension (14 October 2026)

* Extend eligible opening-quote pricing to limit entries and fixed/dynamic targets.
  Preserve original thresholds in the order audit and each trade's fixed levels.
* Trigger and fill protective stops on bid for longs and ask for shorts. Exit
  timing and subsequent trade selection can change, not just execution prices.
* For an inferred execution clock only, permit an off-grid activation to use its
  first observed execution bar. Explicit boundaries, expiry, TTL and cancellation
  remain binding; the recorded activation time is unchanged.
* Document and test existing opposite-signal semantics; retain calendar-day
  aggregation and disclose short Sunday validity. Public strategy calls stay valid.
* Repair portable example-manifest paths and check metadata. The previous records
  below remain historical; later full reruns have separate execution records.

# Stop-pricing extension (14 October 2026)

* Use observed opening quotes for eligible stop entries and fixed/dynamic stop
  exits already crossed at that opening. Retain intrabar threshold approximations,
  strategy syntax, instruction audit thresholds and fixed protective price levels.
* Distinguish opening exits from later touches when resolving protective orders.
  Do not apply an earlier opening to a position entered intrabar, or admit a new
  entry on the same execution bar as a protective exit.
* Document the model and add focused long/short, dynamic-update, expiry, timing
  and quantity checks. Revise two audited CL regression totals; historical
  reproduction records remain evidence for the previous source snapshot, not this patch.

# TradeTesterR 0.1.2.9001

* Process entry fills chronologically, expire unfilled entries after their next
  strategy bar and preserve each filled trade's fixed protection. Record all
  instructions, cancelled alternatives and unresolved entry ties in an audit.
* Order opening fills before intrabar touches. Ignore new entries while holding
  and prevent same-bar re-entry after an intrabar close in single-position mode.
* Order exit legs by execution time; validate scheduled opening prices and
  horizons across session gaps. Preserve legacy dynamic-exit collision priority.
* Bound synchronous legacy strategy calls during optimisation without changing
  their function grammar. Restore the bound after errors and nested calls.
* Correct Sortino's downside denominator. Flag nonpositive equity and suppress
  percentage risk metrics that are undefined after capital exhaustion.
* Batch sorted-clock searches to avoid repeatedly scanning full execution feeds.
* Add regression fixtures, instruction ledgers, complete example/optimisation
  runs and broader technical-indicator checks. Existing example source files
  retain their syntax. Revised results differ materially from the old engine;
  the recorded comparisons document those differences.

# TradeTesterR 0.1.2.9000

* Add internal bounded state/event alignment and a pinned ES reproduction.
* Preserve observed exits when the future candle cutoff is unavailable; enforce
  explicit execution horizons and exit conservation; distinguish terminal reasons.
* Add an opt-in fixed-bracket stop-first collision policy and use ask prices for
  terminal short closes. Preserve the public unknown-collision default.
* Reject unresolved returns in performance and plots; anchor initial returns and
  drawdowns at initial capital. Use a 365-day calendar grid including weekends.
* Use trade-level plot summaries and consistent IS/OOS metrics; expose raw and
  duration-adjusted profit comparisons without claims of statistical robustness.
* Accept explicit optimisation callbacks and frozen risk-free inputs, with no
  automatic yield download. Preserve the legacy callback lookup as a fallback.
* Narrow scope claims, declare TTR for examples/tests, remove conflicting broad
  imports, and adopt runnable offline help fixtures from the unsent CRAN archive.

# TradeTesterR 0.1.2

* analyze_performance(): trade statistics are now computed per trade
  (partial-exit legs aggregate to a single trade). The equity curve is
  padded onto a daily grid before annualization, correcting period length,
  CAGR, volatility and Sharpe figures. Walk-forward optimization metrics
  inherit these corrections (net_profit is unchanged).
* Removed duplicate definitions of xts_to_dt() and calculateTradeMetrics()
  (behavior unchanged).
* Example cleanups: a development loader line removed from
  mean_reversion_long_short.R; a dead commented dependency deleted from
  boilerplate.R.

# TradeTesterR 0.1.1

* Fixed: exit_TP_SL_signal() synthesized a phantom closing leg for any trade
  with two or more genuine exit legs (residual position was inferred from a
  per-row rather than cumulative figure). On the packaged boilerplate this
  removes 8 phantom rows (+$1,156 of spurious P&L) and recovers one
  previously suppressed trade; the example's net changes from -1427.39 to
  -1899.92 while all genuine trade legs are unchanged.
* New regression tests enforce exit-amount conservation per trade.
