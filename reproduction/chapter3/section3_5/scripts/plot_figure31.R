# Plot only: reads matched evidence; does not execute a backtest.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)
task <- normalizePath(args[1], winslash = "/", mustWork = TRUE)
figdir <- file.path(task, "figures")
Sys.setenv(TZ = "UTC")
read_plot <- function(name) read.csv(file.path(figdir, name), stringsAsFactors = FALSE)
stamp <- function(x) as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
prices <- read_plot("figure3_1_observed_ask_close.csv")
prices$date <- stamp(prices$date)
bands <- read_plot("figure3_1_available_band_levels.csv")
bands$NextOpenTime <- stamp(bands$NextOpenTime)
trade <- read_plot("figure3_1_first_trade.csv")
entry <- stamp(trade$EntryTime)
exit <- stamp(trade$ExitTime)
curve <- read_plot("figure3_1_cumulative_pnl.csv")
curve$ExitTime <- stamp(curve$ExitTime)
stopifnot(!anyNA(prices$date), !anyNA(bands$NextOpenTime), !anyNA(curve$ExitTime))
draw <- function() {
  par(mfrow = c(2, 1), mar = c(3.1, 4.7, 2.1, 1.0), mgp = c(2.2, 0.75, 0),
      tcl = -0.25, las = 1, cex = 0.86, family = "serif", bty = "l")
  limits <- range(c(prices$Close, bands$lowerBB, bands$upperBB), na.rm = TRUE)
  limits[2] <- limits[2] + 2
  plot(prices$date, prices$Close, type = "l", col = "#555555", lwd = .65,
       xlim = c(entry - 5*86400, exit + 4*86400), ylim = limits,
       xlab = "2016 (UTC)", ylab = "", xaxt = "n", main = "")
  mtext("Supplied price units", side = 2, line = 3.6, las = 0)
  abline(h = axTicks(2), col = "#e5e5e5", lwd = .5)
  lines(prices$date, prices$Close, col = "#555555", lwd = .65)
  dates <- seq(as.POSIXct("2016-02-08", tz = "UTC"), as.POSIXct("2016-03-07", tz = "UTC"), by = "7 days")
  axis.POSIXct(1, at = dates, format = "%d %b")
  lines(bands$NextOpenTime, bands$lowerBB, type = "s", col = "#21618c", lty = 2)
  lines(bands$NextOpenTime, bands$upperBB, type = "s", col = "#21618c", lty = 2)
  points(entry, trade$EntryPrice, pch = 24, bg = "#147d55", col = "#147d55", cex = 1.35)
  points(exit, trade$ExitPrice, pch = 25, bg = "#912b2b", col = "#912b2b", cex = 1.35)
  title("(a) Earliest completed trade", adj = 0, font.main = 1, cex.main = 1.12)
  legend("top", legend = c("Observed ask close", "Available bands", "Entry", "Exit"),
         col = c("#555555", "#21618c", "#147d55", "#912b2b"),
         lty = c(1,2,NA,NA), pch = c(NA,NA,24,25),
         pt.bg = c(NA,NA,"#147d55","#912b2b"), ncol = 4, bty = "n", cex = .9)
  times <- c(as.POSIXct("2016-01-03 23:00:00", tz = "UTC"), curve$ExitTime)
  values <- c(0, curve$cumulative_realised_pnl)
  plot(times, values, type = "s", col = "#174b70", lwd = 1.25,
       xlab = "Year (UTC)", ylab = "", xaxt = "n", main = "")
  mtext("P&L in example units", side = 2, line = 3.6, las = 0)
  abline(h = axTicks(2), col = "#e5e5e5", lwd = .5)
  abline(h = 0, col = "#777777", lwd = .6)
  lines(times, values, type = "s", col = "#174b70", lwd = 1.25)
  ticks <- as.POSIXct(paste0(seq(2016, 2024, 2), "-01-01"), tz = "UTC")
  axis.POSIXct(1, at = ticks, format = "%Y")
  title("(b) Cumulative realised P&L over the supplied sample", adj = 0, font.main = 1, cex.main = 1.12)
}
png(file.path(figdir, "Figure3_1_mean_reversion.png"), width = 7.1, height = 5.0,
    units = "in", res = 300)
draw()
dev.off()
pdf(file.path(figdir, "Figure3_1_mean_reversion.pdf"), width = 7.1, height = 5.0,
    family = "Times")
draw()
dev.off()
cat("Figure 3.1 generated from saved evidence; no backtests executed.\n")
