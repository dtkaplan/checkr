#' Apply passif/failif/okif tests to an identified code object
#'
#' A generic testing function for applying passif/failif tests with V and EX bindings
#' to a checkr_result object. Unlike `line_where()`, other line functions, no patterns
#' need to be provided. Just the passif/failif tests.
#'
#' @param ex the checkr_result object with a single line of code
#' @param ... passif/failif tests to be applied
#' @param message a character string to be used as the message for a failed result
#'
#' @return a checkr_result object reflecting the outcome of the tests
#'
#' @examples
#' code <- for_checkr(quote({x <- 3; y <- x^2 + 2}))
#' line2 <- line_where(code, Z == "y")
#' check(line2, passif(V == 11, "Right, eleven!"),
#'       message = "Sorry. The result should be 11.")
#' @export
check <- function(ex, ..., message = "Sorry!") {
  stopifnot(inherits(ex, "checkr_result"),
            length(ex$code) == 1)
  line_binding(ex, I, ..., message = message, qkeys = quote({.(EX); ..(V)}))
}
