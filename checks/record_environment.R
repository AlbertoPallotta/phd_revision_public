# Delivery support. Args: library, NEW output file, archive path, SHA256, command.
a <- commandArgs(TRUE)
stopifnot(length(a) == 5L, dir.exists(a[1]), !file.exists(a[2]))
.libPaths(c(normalizePath(a[1], winslash = "/"), .libPaths()))
loadNamespace("TradeTesterR"); loadNamespace("PortfolioTesteR")
capture.output(print(Sys.time()), print(commandArgs()), print(R.version),
               print(.libPaths()), print(sessionInfo()),
               print(find.package(c("TradeTesterR", "PortfolioTesteR"))),
               cat("Archive:", a[3], "\nSHA256:", a[4], "\nCommand:", a[5], "\n"),
               file = a[2])
