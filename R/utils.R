is_interactive <- function() {
  base::interactive() && !is_testing()
}

is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}

os_type <- function() {
  .Platform$OS.type
}

is_windows <- function() {
  .Platform$OS.type == "windows"
}

is_linux <- function() {
  identical(tolower(Sys.info()[["sysname"]]), "linux")
}

find_perl <- function() {
  perl <- Sys.which("perl")

  if (perl == "") {
    stop(
      "Cannot find 'perl'. cloc requires perl to be installed and on the PATH.",
      call. = FALSE
    )
  }

  return(perl)
}
