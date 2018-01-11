#' Find a line calling a function
#'
#' Searches throughly through each line in a checkr_result to find a
#' call to any of the functions listed in the dots. ...
#'
#' @param ex a checkr_result from a previous application of a checkr function, e.g. `for_checkr()`
#' @param ... unquoted functions to search for. A match to any one will trigger success.
#' @param n an integer specifying which successful line to return. (Default: 1, the first.)
#' @param message a character string to include in the checkr_result if the search is a failure.
#' @param just_the_fun If TRUE, don't return the whole line, just the call involving the function
#' @return A checkr_result with a pass, fail, or ok result. If a pass, the code component of the
#' returned value will have the line found by the search. Otherwise it will contain the input code
#' unaltered.
#'
#' @details The functions in `...` should be unquoted, e.g. `sin` or `lm` rather than "sin".
#' Note that infix functions, e.g. `+`, `-`, `||`, *must* be in backquotes: `` `+` ``.
#'
#' Like all checkr functions, if the input `ex` is marked as a fail the output will be a short
#' circuit, that is, immediately returning the input unchanged.
#'
#' @examples
#' code <- for_checkr("x <- 1; y <- x^2; z <- (y^2 + 7) / 2")
#' line_calling(code, `^`, message="Didn't find any line using exponentiation.")
#' line_calling(code, `^`, message="Didn't find any line using exponentiation.",
#'     just_the_fun = TRUE)
#' line_calling(code, `^`, n = 2L, message = "Didn't find a second line using exponentiation.")
#'
#' @export
line_calling <- function(ex, ..., n=1L, message = "Didn't find matching function.", just_the_fun = FALSE) {
  qfuns <- quos(...)
  qfuns <- lapply(qfuns, FUN = quo_expr)
  success_count <- 0L
  check_qfuns(qfuns)

  stopifnot(inherits(ex, "checkr_result"))
  if (failed(ex)) return(ex) # short circuit on failure
  not_yet <- new_checkr_result(action = "fail")

  # Loop over the lines
  for (m in seq_along(ex$code)) {
    this_line <- skip_assign(ex$code[[m]])
    if ( ! inherits(rlang::quo_expr(this_line), "call")) {
      next;
    }
    top_level <- redpen::node_match(this_line, .(fn)(...) ~ fn )
    res <-
      if (is.null(top_level)) { # the expression isn't a call
        not_yet
      } else { # it is a call
        if (c(top_level) %in% c(qfuns)) {
          success_count <- success_count + 1
          if (n == success_count) {
            ex$code <- list(ex$code[[m]])
            return(ex)
          } else {
            not_yet
          }
        } else {
          tmp <- new_checkr_result(action = "ok", message = message, code = ex$code[m])
          arg_calling(tmp, ..., n=1L, message = message)
        }
      }
    if (ok(res)) {
      success_count <- success_count + 1L
      if (n == success_count) {
        if (!just_the_fun) {
          res$code <- list(ex$code[[m]])
        }
        return(res)
      }
    }
  }

  # If we got here, there wasn't a match
  new_checkr_result(action = "fail", message = message, code = ex$code)
}
