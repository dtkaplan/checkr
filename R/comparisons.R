#' Comparison operators for bindings
#'
#' @aliases %same_as% %not_same_as%
#'
#' @importFrom rlang eval_tidy quo_expr child_env quos new_quosure
#' @importFrom rlang enexpr is_formula expr_text quo enquo is_quosure
#' @importFrom rlang is_lang lang_args lang_args_names lang lang_tail lang_head
#' @param e1 an expression,
#' @param e2 another expression
#' @rdname comparisons
#' @export
`%same_as%` <- function(e1, e2) {
  # Handle numbers specially so we don't have to worry about integers and floating points.
  if (is.numeric(e1) && is.numeric(e2)) e1 == e2
  # handle names specially
  else if (is.name(e1) && !is.name(e2)) identical(rlang::eval_tidy(e1), e2)
  else if (is.name(e2) && !is.name(e1)) identical(e1, rlang::eval_tidy(e2))
  # handle quos
  else if (rlang::is_quosure(e1) && rlang::is_quosure(e2))
    identical(rlang::quo_expr(e1), rlang::quo_expr(e2))
  else identical(e1, e2)
}
#' @rdname comparisons
#' @export
`%not_same_as%` <- function(e1, e2) {
  ! e1 %same_as% e2
}

argument_calls <- function(ex, funs) {
  # is it a call at the highest level?
  f1 <- redpen::node_match(ex, .(f)(...) ~ f)
  if (is.null(f1)) { # it isn't
    return(FALSE)
  } else { # it is a call
    if (c(f1) %in% c(funs)) { # there's a match
      return(TRUE)
    } else { # check each of the arguments to f1
      the_args <- rlang::lang_args(ex)
      for (k in seq_along(the_args)) {
        res <- argument_calls(the_args[[k]], funs)
        if (res) return(TRUE)
      }
      return(FALSE)
    }
  }
}
