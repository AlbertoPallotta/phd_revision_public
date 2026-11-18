# Delivery support. No strategy/function calls. Parse and namespace checks.
# Arguments: accepted source, successor source, persistent library, NEW log path.
a <- commandArgs(TRUE)
stopifnot(length(a) == 4L, !file.exists(a[4]))
old <- normalizePath(a[1], winslash = "/", mustWork = TRUE)
new <- normalizePath(a[2], winslash = "/", mustWork = TRUE)
.libPaths(c(normalizePath(a[3], winslash = "/", mustWork = TRUE), .libPaths()))
pkg <- read.dcf(file.path(new, "DESCRIPTION"))[, "Package"]
stopifnot(pkg %in% c("TradeTesterR", "PortfolioTesteR"))
stopifnot(identical(readLines(file.path(old, "NAMESPACE")),
                    readLines(file.path(new, "NAMESPACE"))))
# These identified sources use default C order without Collate overrides or
# platform-specific R subdirectories. Stop if those assumptions change.
stopifnot(identical(Sys.setlocale("LC_COLLATE", "C"), "C"))
od <- read.dcf(file.path(old, "DESCRIPTION"))
nd <- read.dcf(file.path(new, "DESCRIPTION"))
collate_fields <- c("Collate", "Collate.windows", "Collate.unix")
stopifnot(!any(collate_fields %in% colnames(od)),
          !any(collate_fields %in% colnames(nd)),
          length(list.dirs(file.path(old, "R"), recursive = FALSE)) == 0L,
          length(list.dirs(file.path(new, "R"), recursive = FALSE)) == 0L)
of <- sort(list.files(file.path(old, "R"), pattern = "\\.[Rr]$"))
nf <- sort(list.files(file.path(new, "R"), pattern = "\\.[Rr]$"))
stopifnot(identical(of, nf))
for (f in of) stopifnot(identical(parse(file.path(old, "R", f), keep.source = FALSE),
                                   parse(file.path(new, "R", f), keep.source = FALSE)))
ns <- loadNamespace(pkg)
stopifnot(normalizePath(find.package(pkg), winslash = "/") ==
          normalizePath(file.path(a[3], pkg), winslash = "/"))
# Compare every final effective top-level definition, retaining duplicate origins.
# Evaluation is limited to function expressions; package source is not sourced.
definitions <- list(); origins <- list(); assignment_count <- 0L
for (f in nf) for (x in parse(file.path(new, "R", f), keep.source = FALSE)) {
  if (is.call(x) && as.character(x[[1]])[1] %in% c("<-", "=") &&
      length(x) == 3L && is.symbol(x[[2]]) && is.call(x[[3]]) &&
      identical(x[[3]][[1]], as.name("function"))) {
    name <- as.character(x[[2]])
    assignment_count <- assignment_count + 1L
    definitions[[name]] <- x[[3]]
    origins[[name]] <- c(origins[[name]], f)
  }
}
n <- length(definitions)
stopifnot(n > 0L)
for (name in names(definitions)) {
  expected <- eval(definitions[[name]], envir = ns)
  actual <- get(name, envir = ns, inherits = FALSE)
  stopifnot(is.function(actual), identical(formals(expected), formals(actual)),
            identical(body(expected), body(actual)))
}
capture.output(cat("PASS parsed R/API; effective installed functions:", n,
                   "; top-level assignments:", assignment_count, "\n"),
               cat("LC_COLLATE:", Sys.getlocale("LC_COLLATE"), "\n"),
               cat("Effective source order:\n"), print(nf),
               cat("Duplicate assignment origins:\n"),
               print(origins[lengths(origins) > 1L]),
               print(R.version), print(.libPaths()), print(find.package(pkg)),
               print(sessionInfo()), file = a[4])
