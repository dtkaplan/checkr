#' Run test statements in the context of the bindings
#'
#' @param test_list a list containing a set of tests
#' @param bindings the bindings resulting from a pattern match to a submission
#' @param ex the expression being tested (used for failure message)
#'
run_tests <- function(test_list, bindings, ex) {
  # run the tests with these bindings
  res <- new_checkr_result()
  notes <- NULL
  # These additions to bindings overwrite == and !=
  bindings[["=="]] <- function(x,y) x %same_as% y
  bindings[["!="]] <- function(x,y) ! x %same_as% y
  for (k in seq_along(test_list)) {
    test <- rlang::eval_tidy(test_list[[k]], data = bindings)
    # will be a function if passif(), failif(), etc. but
    # will be a checkr_result if it's something else
    if (inherits(test, "checkr_result")) {
      # a recursive call to if_matches()
      action  <- test$action
      message <- test$message
    } else {
      # it's a passif(), noteif(), failif()
      the_test <- test("test")
      bindings[["test_string"]] <- rlang::expr_text(rlang::quo_expr(the_test))
      bindings[["expression_string"]] <- rlang::expr_text(rlang::quo_expr(ex))
      # Evaluate the test in the context of the bindings.
      test_result <- rlang::eval_tidy(the_test, data = bindings)
      message <- moustache(test("message", test_result), bindings)
      action <- test("action", test_result)
    }
    # Short circuit on pass or fail.
    if (action == "ok") {
      if (is.null(notes) && message != "") notes <- paste("*", message)
      else if (message != "")
        notes <- paste(notes,"\n*", message) # save notes for later pass or fail

    }

    if (action %in% c("pass", "fail")) {
      res$action <- action
      res$message <- message
      if ( ! is.null(notes))
        res$message <- paste0(message, "\nNOTES:\n", notes)
      return(res)
    }
  }
  # we got here without a pass or failure. So it's an OK.
  if ( ! is.null(notes))
    res$message <- paste0("NOTES:\n", notes)
  res$action = "ok"

  res
}

