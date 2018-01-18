#' Test for an empty submission
#'
#' Handles a special case when a submission is empty. This function
#' provides a way to through a graceful error, perhaps with a message telling
#' the student how to start.
#'
#' @param ex A checkr_result object, typically straight from `for_checkr`
#' @param message The message to give if `ex` has no code associated with it.
#'
#' @examples
#' code <- for_checkr("")
#' t1 <- if_empty_submission(code, message = "Start out with something. Say 2+2.")
#' @export
if_empty_submission <- function(ex, message = "Give me something to start with!") {
  stopifnot(inherits(ex, "checkr_result"))
  if (failed(ex)) return(ex) # short circuit on failure

  if (length(ex$code) == 0) { # nothing there!
    ex$action <- "fail"
    ex$message <- message
  }

  ex
}

