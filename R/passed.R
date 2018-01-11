#' Check whether a test passed or failed
#'
#' @rdname passed
#' @aliases passed failed ok
#'
#' @param t a checkr_test_result such as produced by `line_where()``
#'
#' @export
passed <- function(t) {
  stopifnot(inherits(t, "checkr_result"))
  t$action == "pass"
}
#' @export
failed <- function(t) {
  stopifnot(inherits(t, "checkr_result"))
  t$action == "fail"
}
#' @export
ok <- function(t) {
  stopifnot(inherits(t, "checkr_result"))
  t$action %in% c("ok", "pass")
}
