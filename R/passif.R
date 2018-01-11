#' Collate a test expression with a message and a action.
#'
#' These functions are simply a way to associate a message with a test.
#' The test itself will be evaluated in `line_` functions, `arg_` functions, `check()`, etc.
#' For `passif()`, `failif()`, and `noteif()`, the corresponding result will be generated  if the `test`
#' passes. If the test fails, an "ok" result is generated. But for `insist()`,
#' a passing test will generate an "ok", and a failing test will generate a "fail". The point of `insist()` is
#' to avoid the need to use double negative with `failif()`. The double negative is confusing and error prone to
#' many people.
#'
#' @rdname passif
#' @aliases passif failif noteif
#'
#' @param test an expression written in terms of values found
#' in the pattern-matching bindings
#' @param message a character string containing the message to return.
#'
#' @details The `message` can include components calculated from the bindings.
#' Enclose these in moustaches, e.g. "The `{{F}}` function is not appropriate for adding."
#' Within a test, the operators `==` and `!=` in a test have been augmented to deal
#' with language objects such as names. They are translated to be equivalent to "\%same_as\%".
#' You can refer to the expression being tested with `{{expression_string}}` and to the `test` itself as `{{test_string}}`.
#'
#'
#' @examples
#' code <- for_checkr(quote({x <- 2; y <- x^2; z <- x + y}))
#' my_line <- line_where(code, F == `+`)
#' check_binding(my_line, `+`(..(x), ..(y)), insist(y == 4, "use 4 for the second argument to +"))
#' # or equivalently with a double negative ... fail and y != 4
#' check_binding(my_line, `+`(..(x), ..(y)), failif(y != 4, "use 4 for the second argument to +"))
#'
#' @rdname passif
#' @export
passif <- generic_test(pass="pass", fail = "ok", "Good!")
#' @rdname passif
#' @export
failif <- generic_test(pass = "fail", fail = "ok", "Sorry.")
#' @rdname passif
#' @export
noteif <- generic_test(pass = "ok", fail = "ok", "Just a note ...")
#' @rdname passif
#' @export
insist <- function(test, message = "") {
  test <- rlang::enquo(test)
  function(task, res) {
    if (task == "test") test
    else if (task == "message") ifelse(res, "", message)
    else if (task == "action") ifelse(res, "ok", "fail")
  }
}
