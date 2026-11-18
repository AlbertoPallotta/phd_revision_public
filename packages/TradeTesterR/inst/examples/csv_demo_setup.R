# TradeTesterR CSV demonstration setup.
# Functional documentation demonstration; not a thesis strategy/result replacement.
# Source into a fresh environment; no network, file writes, optimisation or model fit.
if (!requireNamespace("TTR", quietly = TRUE))
  stop("This documentation example requires the optional TTR package.")
stopifnot(requireNamespace("TradeTesterR", quietly = TRUE),
          requireNamespace("data.table", quietly = TRUE),
          requireNamespace("xts", quietly = TRUE))
suppressPackageStartupMessages(library(data.table))
csv_path <- system.file("extdata", "crude_wti_futures.csv",
                        package = "TradeTesterR", mustWork = TRUE)
raw <- data.table::fread(csv_path, colClasses = c(open_time = "character"))
stopifnot(identical(names(raw), c("open_time", "open", "high", "low", "close", "volume")))
data.table::setnames(raw, c("date", "Open", "High", "Low", "Close", "Volume"))
raw[, date := as.POSIXct(date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")]
stopifnot(!anyNA(raw$date), !anyDuplicated(raw$date))
data.table::setorder(raw, date)
# Fixed first 160 supplied 15-minute rows, including any original time gaps.
price <- data.table::copy(raw[1:160, .(date, Open, High, Low, Close)])
stopifnot(nrow(price) == 160L,
          price$date[1] == as.POSIXct("2016-01-03 23:00:00", tz = "UTC"),
          price$date[160] == as.POSIXct("2016-01-05 15:45:00", tz = "UTC"))
# BBands uses the HLC typical price; 20 observations, SMA/default, two SD.
# Rows 1:19 are warm-up only. Bands are display indicators, not trade selectors.
bands <- TTR::BBands(price[, .(High, Low, Close)], n = 20, sd = 2)
price[, `:=`(upperBB = bands[, "up"], lowerBB = bands[, "dn"], midBB = bands[, "mavg"])]
price[, `:=`(NextOpenTime = shift(date, type = "lead"),
             NextOpen = shift(Open - 0.04, type = "lead"),
             EntryTime = shift(date, type = "lead"),
             EntryPrice = NA_real_, EntryType = "Market", OrderSize = NA_real_,
             exit_long = 0, exit_long_2 = 0)]
# Three scheduled long instructions chosen by row position, never by profitability.
# Two price-unit quantities; no contract multiplier, commission or funding model.
# Enter on the next observed ask open. Close half on row +9's bid open and the
# remaining half on row +13's bid open. No TP/SL or intrabar trigger is used.
entries <- c(40L, 80L, 120L)
price[entries, `:=`(OrderSize = 2, EntryPrice = price$Open[entries + 1L])]
price[entries + 8L, exit_long := 0.5]
price[entries + 12L, exit_long_2 := 1]
ask <- xts::xts(as.matrix(price[, .(Open, High, Low, Close)]), order.by = price$date)
quotes <- TradeTesterR::mergeAskBid(ask, ask - 0.04)
dat <- merge(quotes, price, by = "date", all.x = TRUE, sort = TRUE)
ex <- list(dat = dat, price = price)
# ex contains the prepared execution table and the price/indicator table.
# The guide uses these objects to inspect partial exits and the first exit leg.
# Annualised statistics from this very short demonstration have no study meaning.
