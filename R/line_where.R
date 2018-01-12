#' Identify an individual line in a sequence of commands
#'
#' The tests are written in terms of pronouns
#' - F the function at the highest level (but with assignment removed)
#' - Z the name being bound to the line's value by assignment. ("" if no assignment.)
#' - V the value produced by the line.
#' - EX the expression itself (but with assignment removed)
#'
#' @aliases line_where lines_after
#'
#' @param ex expressions as made by for_checkr()
#' @param ... tests specifying the kind of line we want
#' @param message A character string to be included as the message in the result. This
#' can have moustaches written in terms of F, Z, V, or EX
#'
#'
#' @return A checkr test result. By default, if the line is found, the result
#' is an "OK", setting the stage for further testing. If no matching line is found, the result is a fail.
#'
#' @examples
#' ex <- for_checkr(quote({x <- 2; y <- x^3; z <- y + x}))
#' line_where(ex, insist(F == "^", "Didn't find exponentiation"))
#'
#' @export
line_where <- function(ex, ..., message = "") {
  stopifnot(inherits(ex, "checkr_result"))
  if (failed(ex)) return(ex) # short circuit on failure
  tests <- rlang::quos(...)
  res <- matching_line(ex, tests, message = message)

  res
  # if (res$n == 0) {
  #   the_message <-
  #     if (nchar(message)) res$message
  #     else "Didn't find a line passing tests."
  #     new_checkr_result(action = "fail", message = the_message, code = ex$code)
  # }
  # else {
  #   # note: assignment will be included in the returned code.
  #   new_checkr_result(action = "pass",
  #                     message = "",
  #                     code = ex$code[res$n])
  #   }
}
#' Grab the lines after a specified line (which is included)
#'
#' @rdname line_where
#' @export
lines_after <- function(ex, ..., message = "") {
  stopifnot(inherits(ex, "checkr_result"))
  if (failed(ex)) return(ex) # short circuit on failure
  tests <- rlang::quos(...)
  res <- matching_line(ex, tests, message = "")
  if (res$n == 0) {
    if (nchar(message)) new_checkr_result(action = "fail",
                                       message = res$message)
    else NULL
  }
  else ex$code[res$n:length(ex$code)]
}


# internal function to run the tests to find a matching line
matching_line <- function(ex, tests, message = "", type = NULL, type_text="") {
  stopifnot(inherits(ex, "checkr_result"))
  type_failure <- "" # a flag
  for (k in 1:length(ex$code)) {
    # Create the bindings
    V <- if ("values" %in% names(ex)) {
      ex$values[[k]] # the value was pre-computed
    } else {
      rlang::eval_tidy(ex$code[[k]])
    }
    F <- get_function(simplify_ex(ex$code[[k]]))
    Z <- get_assignment_name(ex$code[[k]])
    EX <- skip_assign(ex$code[[k]])
    # other attributes to identify a line?

    # run the tests in an environment where the pronouns V, F, Z, EX are defined
    # run the tests with these bindings
    bindings <- list(V = V, F = F, Z = Z, EX = EX)
    simp_ex <- simplify_ex(ex$code[[k]])
    res <- run_tests(tests, bindings, simp_ex)
    if ( ! failed(res)) {
      res$code <- list(ex$code[[k]])
      attr(res, "line_number") <- k  # include line number
      return(res)
    }
  }
  #   # are defined. The tests should return a logical
  #   passed_all <- TRUE
  #   for (t in seq_along(tests)) {
  #     bindings <- list(V = V, F = F, Z = Z, EX = EX, `==` = `%same_as%`, `!=` = `%not_same_as%` )
  #
  #
  #     pass_this_test <-
  #       try(rlang::eval_tidy(tests[[t]], data = bindings), silent = TRUE)
  #     if (inherits(pass_this_test, "try-error")) {
  #       warning("Error in checkr test statement.")
  #       passed_all <- FALSE
  #       break
  #     }
  #     else if ( ! pass_this_test) {
  #       passed_all <- FALSE
  #       break
  #     }
  #   }
  #   if (passed_all) break
  # }

  # return the input expression, marked as a failure
  ex$action <- "fail"
  ex$message <- message

  ex
  # return the line number: zero if no line was found

  # list(n = ifelse(passed_all, k, 0),
  #      message = moustache(message, bindings = bindings))
}

