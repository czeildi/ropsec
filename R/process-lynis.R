process_lynis_audit <- function(res) {

  suggestions <- res[grepl("^sugg", res)]
  warnings <- res[grepl("^warn", res)]

  clean_res <- res[!grepl("^#|^sugg|^warn", res)]

  clean_res <- stri_split_fixed(clean_res, "=", 2, simplify = TRUE)
  clean_res <- as.data.frame(clean_res, stringsAsFactors = FALSE)
  class(clean_res) <- c("tbl_df", "tbl", "data.frame")

  suggestions <- res[grepl("^sugg", res)]
  suggestions <- stri_split_fixed(suggestions, "=", 2, simplify = TRUE)
  suggestions <- as.data.frame(suggestions, stringsAsFactors = FALSE)
  class(suggestions) <- c("tbl_df", "tbl", "data.frame")

  warnings <- res[grepl("^warn", res)]
  warnings <- stri_split_fixed(warnings, "=", 2, simplify = TRUE)
  warnings <- as.data.frame(warnings, stringsAsFactors = FALSE)
  class(warnings) <- c("tbl_df", "tbl", "data.frame")

  if (nrow(warnings) > 0) {
    tmp <- utils::read.csv(text=paste0(warnings$V2, collapse="\n"), sep="|", stringsAsFactors=FALSE, header=FALSE)
    warnings <- tmp$V2
  } else {
    warnings <- c()
  }

  if (nrow(suggestions) > 0) {
    tmp <- utils::read.csv(text=paste0(suggestions$V2, collapse="\n"), sep="|", stringsAsFactors=FALSE, header=FALSE)
    suggestions <- tmp$V2
  } else {
    suggestions <- c()
  }

  list(
    warnings = warnings,
    suggestions = suggestions,
    audit_results = stats::setNames(clean_res, c("field", "value"))
  ) -> out

  class(out) <- c("lynis_obj", "list")

  out

}

#' Print lynis audit results
#'
#' @md
#' @param x lynis_obj
#' @param ... unused
#' @export
#' @keywords internal
print.lynis_obj <- function(x, ...) {

  cat("System audit results:\n\n", sep="")

  if (length(x$warnings) > 0) {
    cat("Warnings:\n", sprintf("  - %s\n", x$warnings), "\n", sep="")
  } else {
    cat("No warnings.\n")
  }

  cat("\n")

  if (length(x$suggestions) > 0) {
    cat("Suggestions:\n", sprintf("  - %s\n", x$suggestions), "\n", sep="")
  } else {
    cat("No suggestions\n")
  }

  cat("\n")

  print(x$audit_results)

}
