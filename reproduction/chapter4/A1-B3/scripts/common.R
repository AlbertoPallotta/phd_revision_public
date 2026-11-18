root <- normalizePath(Sys.getenv("PT_BUILDER_ROOT"), winslash = "/", mustWork = TRUE)
repdir <- file.path(root, "thesis-reproduction")
for (d in c("evidence", "logs", "results", "figures", "environment"))
  dir.create(file.path(repdir, d), showWarnings = FALSE, recursive = TRUE)
options(warn = 1, digits = 17)
library(PortfolioTesteR)
library(data.table)
stopifnot(as.character(packageVersion("PortfolioTesteR")) == "0.1.4.9008",
          normalizePath(find.package("PortfolioTesteR"), winslash = "/") ==
            normalizePath(file.path(root,"library-candidate/PortfolioTesteR"),winslash="/"))
recipes <- new.env(parent = globalenv())
sys.source(system.file("examples", "thesis_multifactor_A1.R", package="PortfolioTesteR"), recipes)
sys.source(system.file("reproducibility", "thesis_recipes_B3.R", package="PortfolioTesteR"), recipes)
mat <- function(p) as.matrix(as.data.frame(p)[,setdiff(names(p),"Date"),drop=FALSE])
write_csv <- function(x, p) data.table::fwrite(x, file.path(repdir,p), na="NA")
near <- function(x,y,tol=1e-12) isTRUE(all.equal(unname(x),unname(y),tolerance=tol,check.attributes=FALSE))
checks <- list()
check <- function(id, yes, detail="") {
  checks[[length(checks)+1L]] <<- data.frame(check=id,status=if(isTRUE(yes))"PASSED" else "FAILED",detail=detail)
  if (!isTRUE(yes)) stop(id,": ",detail)
}
save_checks <- function(name) write_csv(data.table::rbindlist(checks),paste0("evidence/",name,".csv"))
