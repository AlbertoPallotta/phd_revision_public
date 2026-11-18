# Source this file, then call record_environment() explicitly for a NEW run.
# It does not execute a strategy or reconstruct a historical session.
record_environment <- function(output_dir, model_backend,
                               source_commit = NA_character_,
                               input_files = character()) {
  stopifnot(is.character(output_dir), length(output_dir) == 1L,
            is.character(model_backend), length(model_backend) == 1L,
            !is.na(model_backend), nzchar(model_backend),
            is.character(source_commit), length(source_commit) == 1L)
  if (!is.na(source_commit) && !grepl("^[0-9a-fA-F]{40}$", source_commit)) {
    stop("source_commit must be a full Git commit SHA, or NA for an unpublished copy.")
  }
  if (file.exists(output_dir)) stop("Use a new output directory; existing records are preserved.")
  if (any(!file.exists(input_files))) stop("Every specified input file must exist.")
  package_path <- find.package("PortfolioTesteR")
  identity <- data.frame(
    package = "PortfolioTesteR",
    version = as.character(utils::packageVersion("PortfolioTesteR")),
    installed_path = package_path,
    source_commit = source_commit,
    model_backend = model_backend,
    recorded_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    stringsAsFactors = FALSE
  )
  inputs <- data.frame(path = normalizePath(input_files, winslash = "/", mustWork = TRUE),
                       md5 = unname(tools::md5sum(input_files)), stringsAsFactors = FALSE)
  if (!dir.create(output_dir, recursive = TRUE)) stop("Could not create output directory.")
  utils::write.csv(identity, file.path(output_dir, "run-identity.csv"), row.names = FALSE)
  utils::write.csv(inputs, file.path(output_dir, "input-files.csv"), row.names = FALSE)
  utils::write.csv(utils::installed.packages(), file.path(output_dir, "installed-packages.csv"), row.names = FALSE)
  capture.output(utils::sessionInfo(), file = file.path(output_dir, "session-info.txt"))
  invisible(identity)
}
