REPRODUCING THE THESIS ILLUSTRATIONS LOCALLY

Start with the repository's README.md for links to the packages and chapters.
This page explains how to view the saved results and redraw their figures.

1. VIEW A FIGURE OR TABLE

Open manifests/figure-table-map.csv and find the figure or table number.
The selected_output column gives the file to open. The input_relative_paths
column identifies its source records; rows_filter explains the relevant rows
or display calculation. Paths are relative to the repository folder.

The table CSVs in reproduction/tables reproduce the cells printed in the thesis.
They are not newly calculated study results. The index also points to the
supporting numerical records for the tables that have them.

2. TRY THE PACKAGES

Follow INSTALL.txt first, then run the introductory example in either package
README in a fresh R session. These examples use bundled data. TradeTesterR's
guide also contains a scheduled partial-exit example. The package examples are
separate from the chapter study scripts.

3. REDRAW THE FIGURES

The commands below use the supplied results and unchanged plotting scripts.
They do not repeat the strategy searches, backtests or model fitting. Figure
3.1 additionally reconstructs prices and Bollinger bands from the bundled CSV.

Use the plain commands if you installed with INSTALL.txt section A, or the
PowerShell block if you set up the separate library in section C. Figure 3.1
needs data.table, xts, zoo and TTR. The plotting scripts also need an R build
with Cairo graphics support.

Plain commands (Windows, macOS or Linux)

Run these from the repository folder with Rscript available in your terminal.
On Windows, if Rscript is not recognised, use the full path to your installed
Rscript.exe; in PowerShell, put & before a quoted executable path. The commands
use R's normal library locations; if you installed into another library, make
it available to Rscript (the PowerShell block below does this for section C).

Replace OUTPUT with a folder path outside the repository. The folder may be new
or existing, but each command's own subfolder (for example OUTPUT/fig31) must
not exist yet; the script creates it. For another run, choose a new OUTPUT path.
Run one command at a time and stop if it reports an error.

Figure 3.1 — trading illustration
  Rscript --vanilla "reproduction/chapter3/section3_5/scripts/plot_figure31_delivery.R" "reproduction/chapter3/section3_5" "packages/TradeTesterR/inst/extdata/crude_wti_futures.csv" "OUTPUT/fig31"

Figures 3.2–3.5 — parameter search and walk-forward results
  Rscript --vanilla "reproduction/chapter3/section3_6/scripts/plot_section36_delivery.R" "reproduction/chapter3/section3_6" "OUTPUT/fig32-35"

Figures 4.1–4.2 — portfolio examples
  Rscript --vanilla "reproduction/chapter4/B1/scripts/plot_csv_delivery.R" "reproduction/chapter4/B1/saved/plot-data" "OUTPUT/fig41-42"

Figure 4.3 — CCI portfolio illustration
  Rscript --vanilla "reproduction/chapter4/CCI/scripts/plot_delivery.R" "reproduction/chapter4/CCI/evidence/figure-data.csv" "OUTPUT/fig43"

Figures 4.4–4.7 — multifactor and parameter-search illustrations
  Rscript --vanilla "reproduction/chapter4/A1-B3/scripts/plot_csv_delivery.R" "reproduction/chapter4/A1-B3/figures" "OUTPUT/fig44-47"

Figures 5.1–5.3 — portfolio-study results
  Rscript --vanilla "reproduction/chapter5/portfolio/export/scripts/plot_delivery.R" "reproduction/chapter5/portfolio/export" "OUTPUT/fig51-53"

Figures 5.4–5.8 — trading-study results
  Rscript --vanilla "reproduction/chapter5/tradetester/export/scripts/plot_delivery.R" "reproduction/chapter5/tradetester/export" "OUTPUT/fig54-58"

PowerShell block for the separate library (INSTALL.txt section C)

Use the same PowerShell session and R library configured in section C, after
its file verification succeeds; that setup defines $Delivery and $Rscript and
selects the library. Set $Output to a new folder outside the repository, for
example  $Output = 'C:/path/to/new-figure-output'  , then run the commands.
Each one creates its own subfolder and refuses to overwrite an existing one;
for another run, choose a different $Output.

Figure 3.1 — trading illustration
  & $Rscript --vanilla "$Delivery/reproduction/chapter3/section3_5/scripts/plot_figure31_delivery.R" "$Delivery/reproduction/chapter3/section3_5" "$Delivery/packages/TradeTesterR/inst/extdata/crude_wti_futures.csv" "$Output/fig31"
  if ($LASTEXITCODE -ne 0) { throw 'Figure command failed; inspect its output before continuing.' }

Figures 3.2–3.5 — parameter search and walk-forward results
  & $Rscript --vanilla "$Delivery/reproduction/chapter3/section3_6/scripts/plot_section36_delivery.R" "$Delivery/reproduction/chapter3/section3_6" "$Output/fig32-35"
  if ($LASTEXITCODE -ne 0) { throw 'Figure command failed; inspect its output before continuing.' }

Figures 4.1–4.2 — portfolio examples
  & $Rscript --vanilla "$Delivery/reproduction/chapter4/B1/scripts/plot_csv_delivery.R" "$Delivery/reproduction/chapter4/B1/saved/plot-data" "$Output/fig41-42"
  if ($LASTEXITCODE -ne 0) { throw 'Figure command failed; inspect its output before continuing.' }

Figure 4.3 — CCI portfolio illustration
  & $Rscript --vanilla "$Delivery/reproduction/chapter4/CCI/scripts/plot_delivery.R" "$Delivery/reproduction/chapter4/CCI/evidence/figure-data.csv" "$Output/fig43"
  if ($LASTEXITCODE -ne 0) { throw 'Figure command failed; inspect its output before continuing.' }

Figures 4.4–4.7 — multifactor and parameter-search illustrations
  & $Rscript --vanilla "$Delivery/reproduction/chapter4/A1-B3/scripts/plot_csv_delivery.R" "$Delivery/reproduction/chapter4/A1-B3/figures" "$Output/fig44-47"
  if ($LASTEXITCODE -ne 0) { throw 'Figure command failed; inspect its output before continuing.' }

Figures 5.1–5.3 — portfolio-study results
  & $Rscript --vanilla "$Delivery/reproduction/chapter5/portfolio/export/scripts/plot_delivery.R" "$Delivery/reproduction/chapter5/portfolio/export" "$Output/fig51-53"
  if ($LASTEXITCODE -ne 0) { throw 'Figure command failed; inspect its output before continuing.' }

Figures 5.4–5.8 — trading-study results
  & $Rscript --vanilla "$Delivery/reproduction/chapter5/tradetester/export/scripts/plot_delivery.R" "$Delivery/reproduction/chapter5/tradetester/export" "$Output/fig54-58"
  if ($LASTEXITCODE -ne 0) { throw 'Figure command failed; inspect its output before continuing.' }

After running a command, check the console for errors and open the PNG/PDF
files it produced. The original file names may describe the strategy rather
than carry the thesis figure number. For Figure 4.3, the final files are in the
output subfolder named figures; its qa subfolder holds a comparison display.
reproduction/chapter4/README.txt gives the portfolio examples' source and
scheduling details.

Regenerating a figure checks its presentation from the supplied inputs; it does
not recalculate the thesis's study results. Keep newly generated plots and logs
separate from the saved material in the repository.

4. READ OR RERUN A HISTORICAL STUDY

The chapter folders also contain the original study code. Do not assume that
these files can be run using only the installed packages: some retain their
required historical folder layouts, helper files and prerequisite checks.
Public script copies take historical input/library paths from their documented
arguments or environment variables; see each script's header. Omitted paths
stop with a configuration error. Current packages or sample data are not
substitutes for the historical inputs. Their exact inputs,
commands and software requirements are recorded in manifests/runner-contracts.csv
and manifests/historical-access.csv.

The longer ES input, frozen risk-free object, PortfolioTesteR research database
and some exact historical executables are not supplied. Without them, the full
studies cannot be reproduced from this repository alone. A replacement dataset
or current package version would constitute a different run. DATA_AVAILABILITY.txt
explains the available data; reproduction/SOFTWARE_PROVENANCE.md records the
software and environment evidence for the saved results.
