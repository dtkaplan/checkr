#' Print a checkr_result
#'
#' @rdname print.checkr_result
#' @param x a checkr_result object to be printed
#' @param ... additional args (not used here)
#'
#'
#' @export
print.checkr_result <- function(x, ...) {
  has_code <- "code" %in% names(x)
  if (x$message == "")
    oneline <- paste0("checkr result: *", x$action, "*\n")
  else
    oneline <- paste0("checkr result *", x$action, "* with message:\n", x$message)

  cat(oneline)
  if (has_code) {
    cat("\nCODE:\n")
    print(x$code)
  }
}
