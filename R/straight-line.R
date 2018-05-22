.macos_core_audit <- function(quiet = TRUE) {

  perl <- find_perl()

  utils::read.csv(
    system.file("tests", "macos", "core-macos-tests.csv", package="ropsec"),
    stringsAsFactors=FALSE
  ) -> macos_core_rules

  td <- tempdir()
  dir.create(file.path(td, "lynis"), showWarnings = FALSE)
  on.exit(unlink(file.path(td, "lynis", "lynis"), recursive = TRUE), add=TRUE)

  file.copy(
    system.file("tools", "lynis", package="ropsec"),
    file.path(td),
    overwrite = TRUE,
    recursive = TRUE
  )

  tf <- tempfile(fileext = ".dat")
  on.exit(unlink(tf), add=TRUE)

  pwd <- getwd()
  setwd(file.path(td, "lynis"))
  on.exit(setwd(pwd), add=TRUE)

  c(
    file.path(td, "lynis", "lynis"),
    "--no-log"
  ) -> args

  if (quiet) args <- c(args, "--quiet")

  c(
    args,
    # "--profile",
    # system.file("conf", "macos", "personal.prf", package="ropsec"),
    "--tests",
    paste0(macos_core_rules$TestID, collapse=","),
    "--report-file",
    tf
  ) -> args

  err <- sys::exec_wait(perl, args = args, std_out = FALSE, std_err = FALSE)

  process_lynis_audit(readLines(tf, warn=FALSE))

}


#' Walk a straight line
#'
#' NOTE: This can take several minutes to run
#'
#' @md
#' @note Seriously, this can take several minutes to run
#' @param quiet if `TRUE` show no output during audit
#' @return eventually a standardized audit object but for now a lynis audit
#'         object which is a list with warnings, suggestions and full
#'         audit results (data frame)
#' @export
full_on_audit <- function(quiet = TRUE) {

  if (is_windows()) {
    message("Not yet!")
  } else if (is_linux()) {
    message("Not yet!")
  } else { # hopefully macOS
    .macos_core_audit(quiet)
  }

}


