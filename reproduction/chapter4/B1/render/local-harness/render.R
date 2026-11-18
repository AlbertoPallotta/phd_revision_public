root <- normalizePath("portfolio-phd-amendment-r7-2026-10-19",winslash="/")
Sys.setenv(PT_EXPECT_PATH=file.path(root,"library-candidate/PortfolioTesteR"))
status <- system2(file.path(R.home("bin"),"Rscript.exe"),
 c("--vanilla",shQuote(file.path(root,"scripts/render_candidates.R")),shQuote(file.path(root,"b1-propagate"))))
stopifnot(status==0L)
