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
    message("System check summary is not yet available for Windows!")
  } else if (is_linux()) {
    message("System check summary is not yet available for Linux!")
  } else { # hopefully macOS
    system.file("tests", "simple", "macos-simple-test.R", package = "ropsec", mustWork = TRUE) %>%
      testthat::test_file()
  }
}
