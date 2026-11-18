# Software provenance and historical execution records

The repository commit identifies the supplied delivery; the
[file manifest](../manifests/delivery-files.csv) identifies its archives.
[Execution identities](../manifests/execution-identities.csv) record the software
used for saved results. A later archive with the same package version is not the
historical executable. [Historical access](../manifests/historical-access.csv)
identifies inputs and exact executables that are not distributed; a recorded hash
does not establish access to the file.

## TradeTesterR

The recorded baseline is version 0.1.2 at commit
`8f81be890799b2dc8d4e0268280d90d071434d12`. The supplied 0.1.2.9001
snapshot carries forward the 14 October 2026 opening-pricing implementation.
Source-project and installable-archive membership differ; the
[archive-member map](../manifests/archive-member-projection.csv) records this.
Execution assumptions are in [GUIDE.md](../packages/TradeTesterR/GUIDE.md).

The historical 14 October executable archive has SHA-256
`6bf8b8bd4ff49934046d1214e874ca65e72c1462f0fe779861a62c8da65d749a`.
It is not included. Its completed Windows R 4.6.1
`R CMD check --as-cran --no-manual` in `as-cran-network` recorded
0 errors, 0 warnings and 1 incoming-feasibility NOTE concerning the development
version. Manual generation was excluded. These are results for that archive,
not a new check of the supplied successor or CRAN acceptance. The
[dated technical index](../environment/HISTORICAL_TECHNICAL_INDEX.txt),
[check log](../environment/tradetester/14oct-00check.log) and
[execution map](../manifests/execution-identities.csv) retain the scope and
the distinction between original execution and later copy provenance.

The fixture results below are recorded in the package's
[regression tests](../packages/TradeTesterR/tests/testthat/test-exit-conservation.R),
run as part of the 14 October check (tests OK):
- Market-entry conservation: 104 exit legs grouped into 70 trades.
- Stop-bracket fixture: 143 trades with the revised opening-price treatment;
  this fixture also exercises negative orders.
- Ask/bid merge: exact same-frequency intersection. The regression fixtures and
  the README demonstration use a fixed 0.04 spread; the
  [ES reproduction adapter](chapter5/tradetester/scripts/es_strategy.R) uses 0.25.
  These are recipe settings, not a package spread.
- Partial-exit conservation: zero over/under-exit and zero nonzero final positions.
- Multiple profit targets: a two-target conservation fixture; output rows
  represent exit legs rather than completed trades.
- Walk-forward optimisation: regression tests exercise an explicit strategy callback with enforced window bounds,
  horizon isolation between windows and restoration of scoped optimisation bounds
  (`test-reviewer-corrections.R`, `test-execution-revision.R`), run in the same 14 October check.

Those fixture counts do not describe the README's moving-average example or the
guide's supplementary scheduled demonstration. Both use the unchanged bundled
CSV and a synthetic 0.04 spread. No former synthetic dataset is added.

### Introductory and supplementary examples

The main README and `inst/examples/ma_crossover.R` adapt the original Chapter 3
20/50 SMA and 2% stop teaching pattern. The original displayed function shows
the temporal shifts, indicators, crossover and stop; the shipped script also
supplies quantity, reverse-crossing exit, execution preparation and engine call.
The complete runnable script is not claimed to be verbatim submitted thesis code,
and its complete pre-viva ancestry has not been established. It is separate from
the completed platform-comparison adapter and the ES empirical study.

The strategy body and preparation calculations of the existing shipped template
are retained. Plain comments, explicit default arguments,
necessary package attachments and result/equity inspection form its user-facing
presentation. The recipe encodes 4 × 50 = 200 price-unit exposure in `OrderSize`.
The 10,000 reporting capital does not alter that sizing. Its scheduled `NextOpen`
comes from the ask series; signal exits keep that supplied opening price, so a
synthetic bid does not establish a full spread charge on every transaction.

The guide retains the three-entry, partial/final-exit demonstration using
`csv_demo_setup.R`. Its original rules, selected observations and input assertions
remain; the two closing comments describe the returned objects. Expected trade
counts, conservation and plot accuracy belong in the separate verification record.
The dated checks below describe their original scripts/archives and are not
fresh validation of a subsequently edited presentation.

The historical ES study needs the exact longer `es.txt` input and frozen
risk-free RDS listed in the access map, as well as its original executable.
These are not distributed. The separate standalone input ending in 2022 is a
different file and is not a substitute. Historical ES/walk-forward commands are
source for inspection, not package-installation checks or self-contained reruns.

## PortfolioTesteR

The supplied 0.1.4.9008 snapshot carries forward the dependency, date-alignment,
documentation, reproducibility, annualisation and momentum-availability changes
and contains the dedicated A1/B3 examples. The recorded baseline declares 0.1.4
at `1cda69b18bb0328ff10e6779f42319ac567b8ca6`; the older `v0.1.4` tag
resolves to `394d29e8b68b240b0876f82e666c67da9bbf5d4f`. Version text alone
does not identify executed source. Complete verification of the original
research environment is not established by the available records.

The supplied archive hash identifies the current package without requiring
CRAN acceptance. An upstream moving branch, older tag or baseline SHA is not
the current delivery identity. A repository reference must identify the
repository, full commit and `packages/PortfolioTesteR` subdirectory; installation
uses the manifest-verified archives described in [INSTALL.txt](../INSTALL.txt).

### Installation and preserved vignettes

[INSTALL.txt](../INSTALL.txt) section A gives the ordinary R installation route;
section C retains the Windows verification and separate-library route used for
the recorded checks. The three PortfolioTesteR sample R data
objects and two extdata files retain their exact viva bytes; see
[DATA_AVAILABILITY.txt](../DATA_AVAILABILITY.txt).

The upstream `install_github()` command in the preserved vignette source/HTML
does not select this examination snapshot. Use the manifest-verified archives.
The prebuilt vignette products remain unchanged and are not evidence of a new
run. The source project, installable tarball and chapter companion have separate
roles. Historical results keep their original archive identities even when a
later documentation successor has the same package version.

### Research helpers and inputs

The research input bundle is separate from the package. No public location for
the complete bundle is supplied. It includes `runner.R`, the separately sourced
`ml_helpers.R` (plural), a symbol list, sector mapping and SQLite database.
The package's `R/ml_helper.R` (singular) is a different file. Preserve the
research originals and record which helper is sourced. The historical runner
selects a backend automatically, so installing a new optional package can
change the model used. Record the actual backend, input identities and
environment for any new run; this does not reconstruct the historical environment.
The [access map](../manifests/historical-access.csv) identifies the exact
undistributed inputs and executables. The SQLite research database is not supplied.

### Recording a new run

The companion script below records a **new run's** actual environment and input
hashes. It does not reconstruct the historical environment or rerun the thesis:

```r
source(system.file("reproducibility", "record_environment.R",
                   package = "PortfolioTesteR"))
# Use a new output directory, the actual backend, and the verified source SHA.
# record_environment("run-record", model_backend = "xgboost",
#                    source_commit = "ACTUAL_SOURCE_COMMIT_IF_ASSIGNED",
#                    input_files = c("runner.R", "ml_helpers.R",
#                      "recommended_symbols_2014_2024.txt",
#                      "sp500_sectors.csv", "sp500_history.db"))
```

### Example and execution history

The dedicated A1 example uses Top-5 momentum OR Top-5 low-volatility selection
and a 60/40 positive-momentum/reciprocal-volatility allocation over that union.
It is distinct from the older weekly AND/Top-10/equal-weight example; their
financial specifications are not interchangeable.

The original 41-evaluation A1/B3 run used the 21 October 2026 0.1.4.9008 archive
`d0cf3140f84295213418070b7c9b700b6951489baa30b0d96b9131aabcf978dc`.
The fixed specifications are retained in
[THESIS_EXAMPLES.txt](../packages/PortfolioTesteR/inst/reproducibility/THESIS_EXAMPLES.txt):
one A1 example, 12 full-sample grid evaluations, 24 in-sample and 4 out-of-sample
walk-forward evaluations. B1 results used 0.1.4.9006 with the stated propagation
policy. Retained Chapter 5 portfolio results used 0.1.4.9007. Later documentation
archives did not produce those results.

The original historical launcher requires its original identity and synthetic
checks, 9007 baseline and workspace. Figure construction from original result
objects has its own result-validation prerequisites. Those requirements have
not been satisfied by supplying prior PASS records or by the separate CSV
plot-only adapter. The exact historical archives, baseline and result objects
are not distributed. See the [Chapter 4 companion](chapter4/README.txt),
[runner contracts](../manifests/runner-contracts.csv) and access map.

The supplied documentation changes do not alter runtime functions, signatures,
defaults, exports or calculations; prebuilt vignette products and the designated
viva datasets are preserved. Clarifying the non-operational `cost_bps` path did
not activate deductions or change the thesis collector. The package's
`.draw_heatmap_grid()` helper can place numeric score labels in the wrong cells.
The supplied thesis heatmap uses a separate exporter whose labels were checked
against all 12 saved scores; the package helper remains unchanged.
The known stale calls in
`example_multi_factor.R` and `example_risk_parity.R` remain disclosed in the
package README and have not been validated as runnable examples.

### Documentation example checks

Detailed records of the 4 November 2026 example checks are retained by the
author and are not distributed with this delivery. These checks used the
already supplied package archives, not a future documentation successor.

The 4 November TradeTesterR quick start was the scheduled demonstration now
retained in the guide. Its execution completed, but the multi-indicator
`plotTrades()` output did not pass band-accuracy inspection. Its `upperBB`,
`lowerBB` and `midBB` traces all contain the middle-band series. The indicator
loop uses a deferred formula referring to the final indicator name; the separate
`plotSingleTrade()` implementation captures each vector and its bands agree with
the supplied HLC inputs. No plotting-function repair is included in these
documentation changes. Candles, selected entry/exit markers and all three checked
equity/drawdown curves agree with their saved inputs and exit ledgers.

The then-current quick-start plot selectors address ledger rows: `trades=1:3` displays two legs
of the first trade and the first leg of the second; `trade_num=1` displays the first
half exit (0.62 price-unit P&L), not the first completed trade's two-leg total of
1.04. The numerical checks used the full-precision saved widget payloads, with
timestamps compared as calendar instants and warm-up observations aligned by date.
These are output/plot findings, not evidence of changed thesis backtest results.

A bounded local check on 4 November 2026 used the supplied package identities
with Windows R 4.6.1. The then-current TradeTesterR README scheduled demonstration completed
three trades in six exit legs. The `ma_crossover.R` version checked on that date
completed 30 trades, matching its header at that time. This is historical evidence,
not a result guarantee for the revised README/script or other inputs. `dynamic_exits_trailing.R` completed 107 trades; its retained header
still says 14. That header is not a result guarantee for this snapshot. Its stop
level is recalculated as previous low minus 0.8 ATR and can decrease; the script
does not impose a monotonic trailing-stop constraint. The two template versions checked on that date also
contained a legacy “minute stream” comment; the supplied observations are 15-minute
bars with gaps. Their fixed spread and quantity multiplier are recipe assumptions.

The PortfolioTesteR quick start executed unchanged with the existing 20-instrument,
158-row weekly sample, including its default zero-substitution warm-up policy.
Its plot and printed output were checked against the resulting object. These are
new documentation executions, not historical thesis reproductions or new claims
of strategy effectiveness. The other eight TradeTesterR strategy scripts were
not executed in this review, and no optimisation or ML research sweep was run.

### Reporting and momentum history

The reporting correction introduced in 0.1.4.9004 applies to `print(result)`,
`summary(result)` and `backtest_metrics(result)` while retaining existing calls.
It does not change trades, weights, cash, equity, total returns or the backtest
object. Annualised figures can differ from earlier reports even on weekly grids:
elapsed intervals replace row counts, and the initial stored zero is excluded
from risk estimates. The existing Sharpe/Sortino definitions were not replaced.
Other analytics and ML collectors retain their own conventions; this was not a
package-wide metrics rewrite.

The explicit momentum-availability policy was introduced in 0.1.4.9006. The
default `legacy_zero` keeps the earlier zero-substitution behaviour, including
warmup ties. The selected Chapter 4 recipes opt in to `propagate`; their figures
must be identified separately from the earlier frozen runs. Existing benchmark
recipes, ML feature helpers and other indicators were not changed globally.

### Repository contents and reproduction records

The package data retain exactly their designated viva files: one TradeTesterR
CSV and the five PortfolioTesteR files. No additional research database or
duplicate input-price panels are included. The complete list and access limits
are in [DATA_AVAILABILITY.txt](../DATA_AVAILABILITY.txt). Authorship, licences and
relevant provenance are retained.

The [figure/table map](../manifests/figure-table-map.csv) covers 20 figures and
15 tables. Six tables are descriptive; nine have numerical or inspection-evidence
routes. All 15 table-display CSVs transcribe the controlling Word cells; they are
not new analyses. Historical results retain the original TradeTesterR 14 October
and PortfolioTesteR 9006, 9007 or original-9008 identities. These differ from the
supplied documentation archives even when version numbers are the same.

The display scripts read saved summaries, with canonical-data preparation for
Figure 3.1. B1 display scripts do not repeat historical RDS/object checks. Neither
prior PASS records nor a fresh plot establishes a repeated historical strategy,
model or ledger validation. Original study launchers keep their own dependency
and identity requirements; they are not shortcuts around missing inputs.

The dated [technical evidence index](../environment/HISTORICAL_TECHNICAL_INDEX.txt)
identifies each check's original software and scope. New example execution uses
its own logs. File checksums establish content identity, not access to unavailable
inputs, scientific validity or approval of the thesis. Later development and
upstream moving branches have separate identities from this repository snapshot.

### How the archives were built

The supplied package tarballs were built from `packages/` without rebuilding
vignettes, generating a manual or resaving data. The ten prebuilt PortfolioTesteR
members and six bundled data files were preserved; source ZIPs also retain
project files that R excludes from an installable tarball. The
[archive-member map](../manifests/archive-member-projection.csv) records those
differences. Rebuilding uses these same instructions, with `$Delivery` and `$R`
pointing to the source folder and R executable:

```text
For source-archive construction only, from a new empty build directory:
  & $R CMD build --no-build-vignettes --no-manual --resave-data=no "$Delivery/packages/TradeTesterR"
  & $R CMD build --no-build-vignettes --no-manual --resave-data=no "$Delivery/packages/PortfolioTesteR"
Stage the ten identified PT prebuilt archive members first. Compare their hashes
after build; stop if the build removes/changes any, rather than regenerate a
research vignette. Compare the six data hashes in source, built archive and
installed package. Preserve source-project materials even when R build excludes
them; a source ZIP and an installable tarball have different member inventories.
```

Installation alone does not rebuild those archives or rerun their historical
checks. The earlier source-project, check and archive identities remain those
listed in the technical records, including the separate 24 October archives.
The nine CSV-based TradeTesterR help examples are guarded by `requireNamespace`;
their recorded required check explicitly required TTR so a skip was not a PASS.
Historical model and platform dependencies are separate from introductory
example dependencies.

### Public companion path configuration

Selected historical helpers take caller-supplied input, output and library paths
instead of the author's working-folder locations. Their current hashes and
pre-publication source hashes are recorded in
[runner contracts](../manifests/runner-contracts.csv). Two TradeTesterR environment
records also use neutral path placeholders; their original execution dates,
results and source identities are retained in
[execution identities](../manifests/execution-identities.csv). These path/configuration
derivatives do not recalculate the saved results or supply missing historical
inputs. The four package archives, package source, examples and figure commands
are unchanged by the public move.
