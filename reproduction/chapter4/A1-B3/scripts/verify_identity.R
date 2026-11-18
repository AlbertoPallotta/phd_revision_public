# Supply PT_BASELINE_SOURCE (historical PT9007 source) and PT_BUILDER_ROOT (new A1/B3 workspace).
required_path <- function(name) {
  value <- Sys.getenv(name, unset="")
  if (!nzchar(value)) stop("Missing configuration: set ", name, " to the required historical path.", call.=FALSE)
  normalizePath(value, winslash="/", mustWork=TRUE)
}
baseline <- required_path("PT_BASELINE_SOURCE")
builder_root <- required_path("PT_BUILDER_ROOT")
if (read.dcf(file.path(baseline,"DESCRIPTION"))[1,"Version"] != "0.1.4.9007") stop("Historical PT9007 baseline source required.")
source(file.path(builder_root,"thesis-reproduction/scripts/common.R"))
src <- file.path(root,"PortfolioTesteR")
files <- list.files(file.path(baseline,"R"),pattern="\\.R$",full.names=FALSE)
rows <- lapply(files,function(f) data.frame(file=f,identical=identical(
  parse(file.path(baseline,"R",f),keep.source=FALSE),parse(file.path(src,"R",f),keep.source=FALSE))))
write_csv(rbindlist(rows),"evidence/parsed-production-identity.csv")
check("all_18_parsed_R_files_identical",length(files)==18 && all(rbindlist(rows)$identical))
check("namespace_bytes_unchanged",identical(readBin(file.path(src,"NAMESPACE"),"raw",1e6),readBin(file.path(baseline,"NAMESPACE"),"raw",1e6)))
defs <- list()
for(f in files) for(e in parse(file.path(src,"R",f),keep.source=FALSE))
  if(is.call(e) && identical(e[[1]],as.name("<-")) && is.symbol(e[[2]]) &&
     is.call(e[[3]]) && identical(e[[3]][[1]],as.name("function"))) defs[[as.character(e[[2]])]] <- e[[3]]
matches <- lapply(names(defs),function(n) {
  fn <- get(n,envir=asNamespace("PortfolioTesteR"),inherits=FALSE)
  expected <- eval(defs[[n]],envir=new.env(parent=asNamespace("PortfolioTesteR")))
  data.frame(name=n,identical=identical(formals(fn),formals(expected)) && identical(body(fn),body(expected)))
})
write_csv(rbindlist(matches),"evidence/installed-function-identity.csv")
check("installed_all_function_definitions",all(rbindlist(matches)$identical),paste(length(matches),"functions"))
for(rel in c("inst/examples/thesis_multifactor_A1.R","inst/reproducibility/thesis_recipes_B3.R")) {
  installed <- file.path(find.package("PortfolioTesteR"),sub("^inst/","",rel))
  check(paste0("installed_recipe_",basename(rel)),identical(readBin(installed,"raw",1e7),readBin(file.path(src,rel),"raw",1e7)))
}
for(f in list.files(file.path(baseline,"data"),full.names=FALSE))
  check(paste0("data_",f),identical(readBin(file.path(src,"data",f),"raw",1e8),readBin(file.path(baseline,"data",f),"raw",1e8)))
writeLines(capture.output(sessionInfo()),file.path(repdir,"environment/installed-session.txt"))
writeLines(capture.output(.libPaths()),file.path(repdir,"environment/library-paths.txt"))
deps <- installed.packages();write_csv(as.data.frame(deps[,c("Package","Version","LibPath"),drop=FALSE]),"environment/dependencies.csv")
writeLines(capture.output(Sys.getenv(c("R_LIBS","R_LIBS_USER","LC_ALL","LANG","TZ","NOT_CRAN","RUN_SEQ","RUN_LIVE","RSTUDIO_PANDOC","_R_CHECK_FORCE_SUGGESTS_"))),file.path(repdir,"environment/process-settings.txt"))
save_checks("identity-checks")
cat(length(files),"parsed files identical;",length(matches),"installed functions match source.\n")
