#' Pull out an line, expression, or argument calling a specified function
#'
#' Find an argument
#'
#' @param ex An call to be checked
#' @param ... unquoted function names
#' @param n Look for the nth passing argument
#' @param message character string message on failure
#'
#' @examples
#' ex <- for_checkr(quote(15 * sin(53 * pi / 180)))
#' ex2 <- for_checkr(quote(yy <- sin(3))) # at the top level
#' line_calling(ex, sin)
#' # look only in arguments: the top-level function doesn't count
#' arg_calling(ex, sin)
#' arg_calling(ex2, sin)
#' # But line_calling() will find it at the top level.
#' line_calling(ex2, sin)
#'
#' @export
arg_calling <- function(ex, ..., n=1L, message = "call to function") {
  qfuns <- quos(...)
  arg_calling_(ex, qfuns, n=1L, message = "call to_function")
}
arg_calling_ <- function(ex, qfuns, n=1L, message = "call to function") {
  qfuns <- lapply(qfuns, FUN = quo_expr)
  # see whether the call itself is to the function
  this_fun <- get_function(ex$code[[1]])
  if (is.null(this_fun)) {
    ex$action <- "fail"
    ex$message <- "No function found."
    return(ex)
  } else if (c(this_fun) %in% c(qfuns)) {
    if (n == 1) return(ex)
    else n <- n - 1 # found 1 call, now continue on
  }

  test <- function(arg) {
    argument_calls(arg, qfuns)
  }
  res <- generic_arg(ex, "specified function", test, n = n,
              message = message,
              use_value = FALSE)
  if (failed(res)) return(res) # no such call found

  # is the desired function at the top of the tree?
  stopifnot(length(res$code) == 1)
  top_fun <-get_function(res$code[[1]])
  if (c(top_fun) %in% c(qfuns)) return(res)
  else arg_calling_(res, qfuns, n=n, message = message)
}

check_qfuns <- function(qfuns) {
  # are they really quoted functions
  what <- unlist(lapply(c(qfuns), FUN = function(x) is_function(eval(x))))
  if (! all(what)) stop("passing something other than a quoted function")
}
format_qfuns <- function(qfuns) {
  # make sure they are all functions
  check_qfuns(qfuns)
  fnames <- unlist(lapply(qfuns, expr_text))
  nfuns <- length(fnames)
  if (nfuns > 1) {
    paste( paste0(fnames[-nfuns], collapse = ", "),
           "or", fnames[nfuns])
  } else fnames
}

