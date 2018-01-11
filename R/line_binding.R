#' Locates a line matching a redpen pattern and conducts tests on the resulting bindings.
#'
#' Looks for a line matching *all* of the redpen patterns. If the match is found, the
#' tests (see `...`) are evaluated in order using the bindings established in the redpen match.
#' A pass or a fail causes an immediate
#' termination of the testing and returns that result.
#' If no pass or fail occurs, a neutral result ("ok")
#' is created so that evaluation can proceed to subsequent statements.
#'
#' If any of the redpen patterns fails to match, a checkr_result "fail" is returned with
#' the message specified in the `message` argument, and with code being the same as the input code.
#'
#' In additon to the passif/failif/noteif tests, you can use `line_` functions as a test.
#' Of course, the the functions
#' used in the tests will have an input that is only one line, so the name "line_" may be
#' misleading. `check_binding()` is just an alias for line_binding. Not really needed, but
#' it seems nicer to use `check_binding()`  within a "line_binding"
#'
#' @aliases line_binding check_binding
#'
#' @return A checkr_test object with an action ("pass", "fail", or "ok")
#'
#' @param ex a checkr_result object produced by some previous checkr function, e.g. `for_checkr()`
#' @param keys an R statement used for pattern matching and binding, based
#' on the redpen package. This can also be a {}-bracketed set of patterns. If the expression
#' involves assignment, the keys will be matched only to the RHS of assignment, not the whole
#' expression. See details for what becomes of assignment.
#' @param ... tests to apply to expressions in `ex`. These are typically made
#' with `passif()`, `failif()`, `noteif()`, `check_binding()`, and so on. In addition to the bindings
#' defined in `keys`, for each line the name of the object assigned to will be bound to the pronoun `Z`.  (`Z` will be `""`
#' when there's no assignment in the line.)
#' @param message a character string message. If the patterns don't match, the message to
#' give with the failed checkr_result.
#' @param qkeys (for internal use only) a quoted expression containing the keys.
#'
#' @details Remember that the `keys` should be designed around statements *not* involving assignment.
#' If you want to check assignment, use the `Z` pronoun.

#' @return a checkr_result object.
#'
#' @details The pattern or patterns in `keys` are applied to each of the expressions in `ex`.
#' The tests are only considered for the first expression in `ex` that matches
#' the pattern.  `passif()` and `failif()` tests, when satisfied, lead to immediate
#' return: no later tests are performed. `noteif()` just adds a note, without
#' terminating the testing. The redpen patterns are compared just to the RHS of any assignment.
#' The Z pronoun will store the name of any assignment (and will be `""` if there's no assignment.)
#'
#'
#' @examples
#' ex <- for_checkr(quote(z <- 2+2))
#' line_binding(ex, 2 + 2, passif(TRUE, "The pattern matched."), message = "I was looking for 2+2.")
#' line_binding(ex, 3 + 3, message = "I was looking for 3 + 3")
#' # The Z pronoun for assignment is added by default
#' line_binding(ex, 2 + 2, passif(Z == "z", "Assignment to {{Z}}."))
#' line_binding(ex, 2 + ..(y),
#'     failif(y != 2,
#'          "{{expression_string}} wasn't right. Second argument should be 2,  not {{y}}"))
#' line_binding(ex, `+`(.(a), .(b)), passif(TRUE, "Found a match."))
#' line_binding(ex, `+`(.(a), .(b)),
#'   passif(a==b, message = "Yes, the arguments to + are equal. They are both {{a}}."))
#' wrong1 <- for_checkr(quote(2 - 2))
#' wrong2 <- for_checkr(quote(2*2))
#' line_binding(wrong1, {.(expr); .(f)(.(a), .(b))},
#'   passif(f == `+`, "Right! Addition means {{f}}."),
#'   failif(f != `+`, "In {{expr}}, you used {{f}} instead of +."))
#' line_binding(wrong2, {.(fn)(.(a), .(b)); ..(val)},
#'   noteif(val == 4, "Right overall answer: {{val}}."),
#'   failif(fn != `+`, "You need to use the `+` function, not {{fn}}."),
#'   noteif(val != 4, "The result should be 4, not {{val}}."),
#'   passif(fn == `+` && val == 4 && a == b))
#' code2 <- for_checkr(quote({data(mtcars); plot(mpg ~ hp, data = mtcars)}))
#' line_binding(code2,
#'   # note, single . with .(fn)
#'   {..(val); .(fn)(.(formula), data = mtcars);},
#'   passif(fn == plot, "You made the plot!"))


#' @export
line_binding <- function(ex, keys, ..., message = "No match found to specified patterns.", qkeys = NULL) {
  stopifnot(inherits(ex, "checkr_result"))
  if (failed(ex)) return(ex) # short circuit on failed input

  code <- ex$code
  keys <- if (is.null(qkeys)) rlang::enexpr(keys)
          else qkeys
  # make sure the patterns, even if from parse(),
  # are put into the form of a set of bracketed expressions
  keys <- as_bracketed_expressions(keys)
  tests <- rlang::quos(...)

  # We'll be indexing <keys> from 2, since slot 1 has
  # the bracket `{`

  if (length(keys) <= 1) stop("No expressions given for argument 'keys'.")

  for (m in seq_along(code)) {
    this_env <- environment(code[[m]]) # get the line's environment
    bindings <- list(Z = get_assignment_name(code[[m]])) # pronoun for assignment name
    patterns_matched <- rep(FALSE, length(keys)-1)
    for (k in 2:length(keys)) {
      # a formula whose RHS copies the environment
      # that node_match will put in .data
      pattern <- LHS ~ copy_env(.data)
      rlang::f_lhs(pattern) <- rlang::expr( !! keys[[k]])

      # Grab the list of bindings
      # Handle either a simple list of quosures or the output of
      # for_checkr()
      Z <- get_assignment_name(code[[m]])
      simp_ex <- simplify_ex(code[[m]])
      new_bindings <-
        try(redpen::node_match(simp_ex, !!pattern),
            silent = TRUE)

      # If command throws error, special fail on error
      if (inherits(new_bindings, "try-error")) {
        return(
          new_checkr_result(
            action = "fail",
            message = as.character(new_bindings) # holds error message
            )
          )
      }

      if (is.null(new_bindings)) {
        break;
      } else {
        patterns_matched[k-1] <- TRUE
        new_bindings$. <- NULL
        bindings <- c(bindings, new_bindings)
      }

    }
    if (all(patterns_matched)) {
      # we found a match to expression[[m]] for all the keys
      break;
    }

  }

  # If none of the expressions matched all of the patterns,
  # return now.
  if ( ! all(patterns_matched)) {
    res <- if (message == "") new_checkr_result("ok", code = code)
    else new_checkr_result("fail", message, code = code)
    return(res)
  }

  # run the tests with these bindings
  res <- run_tests(tests, bindings, simp_ex)
  res$code <- list(code[[m]])

  res
}


#' @export
check_binding <- line_binding



