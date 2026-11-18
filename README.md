# TradeTesterR and PortfolioTesteR

## Software and examples accompanying the PhD thesis

This repository brings together the two R packages developed for the thesis,
the code used in its illustrations, and saved figures and results organised by
chapter. You can browse the results without installing R, try the package
examples with their included data, or use the reproduction scripts to redraw
the supplied figures on your computer.

## Start here

| What would you like to do? | Where to start |
| --- | --- |
| Explore individual trades, orders and exits | [TradeTesterR: introduction and first strategy](packages/TradeTesterR/README.md) |
| Build a portfolio from signals, selections and weights | [PortfolioTesteR: introduction and quick start](packages/PortfolioTesteR/README.md) |
| Find a figure or table cited in the thesis | [Figure and table index](manifests/figure-table-map.csv) — search for its number and open the file in `selected_output`. |
| Install the packages and run the examples | [Installation instructions](INSTALL.txt), followed by the example in either package README. |
| Redraw thesis figures from the supplied results | [Local reproduction instructions](reproduction/README.txt), with commands grouped by chapter. |

## Materials by chapter

| Chapter | Contents |
| --- | --- |
| [Chapter 3 — TradeTesterR](reproduction/chapter3/) | Strategy illustrations, selected execution records and walk-forward results. |
| [Chapter 4 — PortfolioTesteR](reproduction/chapter4/) | Portfolio examples, parameter-search results and walk-forward illustrations. |
| [Chapter 5 — empirical illustrations](reproduction/chapter5/) | Saved portfolio and trading-study figures, numerical summaries and study scripts. |

The [table files](reproduction/tables/) hold the cells shown in the thesis tables;
where a table has supporting numerical records, the figure and table index lists
them. Reading these files does not require running a backtest.

## Working locally

1. Download the repository snapshot linked in the thesis and extract it into a
   folder on your computer. Keep its subfolders together.
2. Follow section A of [INSTALL.txt](INSTALL.txt) to install the supplied packages
   from R on Windows, macOS or Linux. Optional download checks and the separate
   Windows library setup are in sections B and C.
3. Run a package's introductory example in a fresh R session, or choose a
   chapter's figure commands from the reproduction instructions. Save newly
   generated output in a separate folder.

## What can be reproduced from these files?

The introductory package examples use the included sample data. The figure scripts redraw
the thesis illustrations from saved results; Figure 3.1 also reconstructs its
price and indicator display from the bundled TradeTesterR data. These scripts
do not rerun the full backtests or fit the research models again.

Complete historical study reruns require additional inputs and the software
used for those runs. The PortfolioTesteR research database, the longer ES study
input, the frozen risk-free data and some original executables are not included.
The [data-availability information](DATA_AVAILABILITY.txt) explains the data limits;
[historical access](manifests/historical-access.csv) lists the unavailable historical
inputs and executables.
The original study scripts are supplied for inspection, but are not all
self-contained launchers that can be run immediately after installation.

## Software records

For archive checksums, the software used for each saved result and the scope of
earlier checks, see [software provenance](reproduction/SOFTWARE_PROVENANCE.md).
Use the archives included here when following this thesis; a current upstream
package can differ from the version that produced a saved result.
