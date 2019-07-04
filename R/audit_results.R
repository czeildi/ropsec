#' testthat style system checks
#'
#' Test system settings like ssh key, password requirement etc.
#'
#' @return testthat report
#' @export
#'
#' @examples \dontrun{summarize_system_checks()}
summarize_system_checks <- function() {
  if (is_windows()) {
    system.file("tests", "simple", "windows-simple-test.R", package = "ropsec", mustWork = TRUE) %>%
      testthat::test_file()
  } else if (is_linux()) {
    system.file("tests", "simple", "linux-simple-test.R", package = "ropsec", mustWork = TRUE) %>%
      testthat::test_file()
  } else {
    system.file("tests", "simple", "macos-simple-test.R", package = "ropsec", mustWork = TRUE) %>%
      testthat::test_file()
  }
}
