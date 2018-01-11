#' Tests on expressions
#'
#' These functions can be applied to EX bindings as produced by `line_where()`.
#' They calculate something about the EX expression.
#'
#' @param ex A straight expression, for instance as bound to `EX` in `line_where().`
#'
#' @rdname test_ex
#' @export
n_args <- function(ex) {
  length(rlang::lang_args_names(ex))
}
#' @rdname test_ex
#' @export
func_of <- function(ex) {
  rlang::lang_head(ex)
}
