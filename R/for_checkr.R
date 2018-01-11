#' Translate a sequence of commands into a sequence of quosures
#'
#' This function is the first step in any checkr sequence. It scans the
#' quoted commands for parse errors, evaluates the lines one at a time, and
#' packages up the lines into quosures with an environment in which the line
#' should be executed. (That way the lines can be evaluated in a stand-alone
#' manner.)
#'
#' @param exprs an expression or set of expressions in a curly brace
#'
#' @return a quosure for each expression in exs, with an associated
#' environment that reflects the state when that expression was evaluated.
#'
#' @examples
#' code <- for_checkr(quote({x <- 2; y <- x^2; z <- x + y}))
#' class(code)
#' passed(code)
#' line_where(code, Z == "z")
#' line_where(code, V == 4, Z == "y")

#' @export
for_checkr <- function(exprs) {
  if (is.character(exprs)) { # turn text into parsed code
    exprs <- parse(text = exprs)
  }
  exprs <- as_bracketed_expressions(exprs)
  # The environment for the first expression
  prev_env <- rlang::caller_env()
  code <- list()
  values <- list()
  for (m in 2:length(exprs)) {
    next_env <- new.env(parent = prev_env)
    so_far <- try(rlang::eval_bare(exprs[[m]], env = next_env), silent = TRUE)
    if (inherits(so_far, "try-error")) {
      return(checkr_result_on_error(so_far, exprs[[m]]))
    } else {
      values[[m - 1]] <- if(!is.null(so_far)) so_far else NA
      # The if deals with the possibility that the value is NULL.
      # Assigned directly to values[[m-1]], this would delete the element.
      # So NA is substituted.
    }
    code[[m - 1]] <- rlang::new_quosure(exprs[[m]], env = prev_env)
    prev_env <- next_env
  }
  # return a checkr result augmented with the enquosured code and values
  res <- new_checkr_result()
  res$code <- code
  res$values <- values

  res
}

checkr_result_on_error <- function(v, ex) {
  message <- attr(v, "condition")
  message <- gsub("^.*\\): ", "", message )
  new_checkr_result(action = "fail",
                    message = paste(rlang::expr_text(ex),
                                    "is an invalid command because", message))
}

