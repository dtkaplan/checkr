#' Comparison operators for bindings
#'
#' @aliases %same_as% %not_same_as% same_val same_name
#'
#' `same_val` checks whether its two arguments have the same value. One or both of the arguments
#' can be a name, in which case the name is evaluated to produce the value.
#' `same_name` checks whether its two arguments correspond to the same name. Either or both arguments can be
#' a name or a character string corresponding to a name.
#' `%same_as%`

#' @param e1 an expression,
#' @param e2 another expression
#' @rdname comparisons
#' @export
`%same_as%` <- function(e1, e2) {
  # Handle numbers specially so we don't have to worry about integers and floating points.
  if (is.numeric(e1) && is.numeric(e2)) e1 == e2
  # handle names specially
  else if (is.name(e1) && !is.name(e2)) identical(eval_tidy(e1), e2)
  else if (is.name(e2) && !is.name(e1)) identical(e1, eval_tidy(e2))
  # handle quos
  else if (is_quosure(e1) && is_quosure(e2))
    identical(quo_expr(e1), quo_expr(e2))
  else identical(e1, e2)
}
#' @rdname comparisons
#' @export
`%not_same_as%` <- function(e1, e2) {
  ! e1 %same_as% e2
}

#' @rdname comparisons
#' @export
same_name <- function(e1, e2) {
  # both expressions should be names or quoted strings containing a name
  stopifnot(is.name(e1) || is.character(e1))
  stopifnot(is.name(e2) || is.character(e2))

  identical(as.name(e1), as.name(e2))
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
      the_args <- call_args(ex)
      for (k in seq_along(the_args)) {
        res <- argument_calls(the_args[[k]], funs)
        if (res) return(TRUE)
      }
      return(FALSE)
    }
  }
}
