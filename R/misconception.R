#' Test for a misconceived statement
#'
#' Sometimes you expect a specific code pattern which might reflect a common misconception.
#' `misconception()` let's you mark a pattern as such, causing the test to fail if the pattern
#' is found.
#'
#' @param ex A checkr result to use as input
#' @param pattern A code pattern as found by any of the functions returning a checkr_result object.
#' @param message The message to give if the `pattern` is found.
#'
#' @examples
#' code <- for_checkr(quote({theta <- 53; x <- sin(theta)}))
#' misconception(code, line_where(code,V == 53), message = "Angle should be in degrees, not radians.")
#' misconception(code, line_calling(code, sin), message = "For the x-coordinate, use cos().")
#'
#' @export
misconception <- function(ex, pattern, message) {
  stopifnot(inherits(ex, "checkr_result"))
  if (failed(ex)) return(ex) # short circuit
  res <- pattern
  if (passed(res) || ok(res)) { # the pattern was found!
    res$action <- "fail"
    res$message <- message
  } else { # the pattern was not found
    # on failure, tests should return a test with code matching the input to the test
    # thus, the code for pattern is to be kept in the result
    return(ex)
  }

  res
}
