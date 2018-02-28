#' Identify an individual line in a sequence of commands
#'
#' The tests are written in terms of pronouns
#' - F the function at the highest level (but with assignment removed)
#' - Z the name being bound to the line's value by assignment. ("" if no assignment.)
#' - V the value produced by the line.
#' - E the expression itself (but with assignment removed)
#'
#'
#' @aliases line_where lines_after
#'
#' @param ex a `"checkr_test"` object for instance as made by for_checkr()
#' @param ... passif/failif/insist tests specifying the kind of line we want. The messages associated with
#' each test can have moustaches written in terms of F, Z, V, or E.
#' @param message A character string message to give if no acceptable line is found.
#'
#' @details Testing starts with the first test in `...`. Each test, in turn, can produce a definitive pass or fail result, at which point
#' testing is complete. But tests can also produce indefinite results, in which case testing moves on to the next test (if any).
#' If there is not a definitive result from the tests, the return
#'
#' If `passif()` or `failif()` are used and the test is `TRUE`, no further testing is performed. Similarly,
#' if `insist()` is used and the test is `FALSE`, no further testing is performed. If none of these conditions
#' applies, `line_where()` progresses to the next of the tests in `...`. If none of the tests produce a definitive
#' result, `line_where()` will return an OK result.
#'
#' The `ex` argument is a `"checkr_test"` object. If that input object is a fail, `line_where()` immediately returns
#' that input: none of the tests are performed. This allows test results to be cascaded.
#'
#' Some important details about what types of objects F, Z, V, and E will be. V and E are straightforward: V will
#' always be the kinds of thing computed by the line, e.g. a vector, dataframe, and so on. E will always be
#' a language expression (minus the assignment, if any), and F will always be a character string. But Z is a name and character strings respectively.
#'
#'
#'
#' @return A `"checkr_test"` result which is either a pass, fail, or OK.
#'
#' @examples
#' ex <- for_checkr(quote({x <- 2; y <- x^3; z <- y + x}))
#' line_where(ex, insist(F == "^"), message = "Didn't find exponentiation")
#'
#' @export
line_where <- function(ex, ..., message = "No such line found.") {
  stopifnot(inherits(ex, "checkr_result"))
  if (failed(ex)) return(ex) # short circuit on failure
  tests <- rlang::quos(...)
  res <- matching_line(ex, tests, message = message)

  res
}


# internal function to run the tests to find a matching line
matching_line <- function(ex, tests, message = "", type = NULL, type_text="") {
  stopifnot(inherits(ex, "checkr_result"))
  if (failed(ex)) return(ex) # short circuit on failure
  type_failure <- "" # a flag
  res <- new_checkr_result(action = "fail", message = "No content to submission.")
  for (k in seq_along(ex$code)) {
    # Create the bindings
    V <- if ("values" %in% names(ex)) {
      ex$values[[k]] # the value was pre-computed
    } else {
      rlang::eval_tidy(ex$code[[k]])
    }
    F <- get_function(simplify_ex(ex$code[[k]]))
    Z <- get_assignment_name(ex$code[[k]])
    E <- skip_assign(ex$code[[k]])
    # other attributes to identify a line?

    # run the tests in an environment where the pronouns V, F, Z, EX are defined
    # run the tests with these bindings
    bindings <- list(V = V, F = F, Z = Z, E = E)
    simp_ex <- simplify_ex(ex$code[[k]])
    res <- run_tests(tests, bindings, simp_ex)
    if ( ! failed(res)) {
      res$code <- list(ex$code[[k]])
      attr(res, "line_number") <- k  # include line number
      return(res)
    }
  }


  if (failed(res)) {
    res$code <- ex$code
    if(res$message == "") res$message <- message
  } else {
    stop("You should never get here. If we got here, the test must have failed!")
  }

  res
}

