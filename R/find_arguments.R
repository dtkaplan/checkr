#' Functions to extract an the argument from an expression
#'
#' Use these functions to find a particular argument in an expression.
#'
#' @rdname find_arguments
#' @aliases formula_arg data_arg matrix_arg vector_arg list_arg table_arg first_arg
#' @return the matching expression as a quosure that can be evaluated
#' with eval_tidy().
#'
#'
#' @details If the expression isn't a call, it still has a value. These functions
#' return that value if it's a match to the type sought. If ex directly from
#' for_checkr(), only the first expression is checked.
#'
#' @param ex the tidy expression to check
#' @param ... passif/failif/okif tests
#' @param nm the name of an argument as a character string (or a regex).
#' @param n an integer. If there's more than one matching argument, which one do
#' you want.
#' @param message a character string. If this is not empty (i.e. `""`) then a fail result
#' will be generated if the argument isn't found. Default: empty.
#'
#' @examples
#' code <- for_checkr(quote(lm(mpg ~ hp, data = mtcars)))
#' formula_arg(code)
#' data_arg(code,
#'   insist("hp" %in% names(V),
#'           "The data should have a column named 'hp'."))
#' matrix_arg(code)
#' named_arg(code, "data", failif(E == `mtcars`, "I didn't want mtcars."))
#' arg_number(code, 3)

#' @export
formula_arg <- function(ex, ..., n=1L, message = "") {
  res <- generic_arg(ex, "a formula e.g. a ~ b", is_formula, n = n, message = message)
  line_binding(res, I , ..., message = message, qkeys = quote({.(E); ..(V)}))
}
#' @rdname find_arguments
#' @export
data_arg <- function(ex, ..., n=1L, message = "") {
  res <- generic_arg(ex, "a data frame", is.data.frame, n = n, message = message)
  line_binding(res, I , ..., message = message, qkeys = quote({.(E); ..(V)}))
}
#' @rdname find_arguments
#' @export
matrix_arg <- function(ex, ..., n=1L, message = "") {
  res <- generic_arg(ex, "a matrix", is.matrix, n = n, message = message)
  line_binding(res, I , ..., message = message, qkeys = quote({.(E); ..(V)}))
}
#' @rdname find_arguments
#' @export
vector_arg <- function(ex, ...,  n=1L, message = "") {
  res <- generic_arg(ex, "a vector", is.vector, n = n, message = message)
  line_binding(res, I , ..., message = message, qkeys = quote({.(E); ..(V)}))
}
#' @rdname find_arguments
#' @export
character_arg <- function(ex, ...,  n=1L, message = "") {
  res <- generic_arg(ex, "a character string", is.character, n = n, message = message)
  line_binding(res, I , ..., message = message, qkeys = quote({.(E); ..(V)}))
}
#' @rdname find_arguments
#' @export
numeric_arg <- function(ex, ..., n=1L, message = "") {
  res <- generic_arg(ex, "numeric", is.numeric, n = n, message = message)
  line_binding(res, I , ..., message = message, qkeys = quote({.(E); ..(V)}))
}
#' @rdname find_arguments
#' @export
list_arg <- function(ex, ...,  n=1L, message = "") {
  res <- generic_arg(ex, "a list", is.list, n = n, message = message)
  line_binding(res, I , ..., message = message, qkeys = quote({.(E); ..(V)}))
}
#' @rdname find_arguments
#' @export
function_arg <- function(ex, ..., n=1L, message = "") {
  res <- generic_arg(ex, "a function", is.function, n = n, message = message)
  line_binding(res, I , ..., message = message, qkeys = quote({.(E); ..(V)}))
}
#' @rdname find_arguments
#' @export
table_arg <- function(ex, ..., n=1L, message = "") {
  res <- generic_arg(ex, "a table", is.table, n = n, message = message)
  line_binding(res, I , ..., message = message, qkeys = quote({.(E); ..(V)}))
}

generic_arg <- function(ex, type_description, type_test,
                        message = "", n = 1L, use_value = TRUE) {
  stopifnot(inherits(ex, "checkr_result"))
  if (failed(ex)) return(ex)
  if (length(ex$code) > 1) stop("Narrow down to a single line of code before calling.") # author
  code <- skip_assign(ex$code[[1]])

  if (message == "") {
    # Pre-form the failure message
    message <- paste(expr_text(quo_expr(code)),
                  "doesn't contain an argument that is",
                  type_description)
  }
  bad_return <- new_checkr_result(action = "fail", message = message, code = ex$code)

  # But usually will be a quo
  this_env <- environment(code)
  the_args <- call_args(code)
  found_target <- FALSE
  target <- NULL
  found_count <- 0

  for (k in 1:length(the_args)) {
    val <- the_args[[k]]
    if (use_value) val <- eval_tidy(val, data = this_env)

    if (type_test(val)) {
      found_count <- found_count + 1
      if (n == found_count) {
        target <- the_args[[k]]
        found_target <- TRUE
        break
      }
    }
  }
  if (found_target) {
    code = list(new_quosure(target, env = this_env))
    new_checkr_result("ok", code = code)
  } else {
    bad_return
  }
}

#' @rdname find_arguments
#' @export
arg_number <- function(ex, n = 1L, ..., message = "") {
  stopifnot(inherits(ex, "checkr_result"))
  if (failed(ex)) return(ex)
  code <- simplify_ex(ex$code[[1]])
  argv <- call_args(code)
  res <-
    if (length(argv) < n) {
      new_checkr_result(action = "fail",
                        message = paste(expr_text(quo_expr(code)),
                                        "does not have", n, "arguments"),
                        code = ex$code)

    } else {
      code <- list(new_quosure(argv[[n]], env = environment(code)))
      new_checkr_result("ok", code = code)
    }
  line_binding(res, I , ..., message = message, qkeys = quote({.(E); ..(V)}))
}

#' @rdname find_arguments
#' @export
first_arg <- function(ex, ..., message = "")
  arg_number(ex, ..., n=1L, message = message)

#' @rdname find_arguments
#' @export
named_arg <- function(ex, nm, ..., message = "") {
  if ( ! is.character(nm)) stop("Must specify argument name as a string.")
  stopifnot(inherits(ex, "checkr_result"))
  # pass along any input that is already failed.
  if (failed(ex)) return(ex)
  code <- simplify_ex(ex$code[[1]])

  arg_names <- call_args_names(code)
  argv <- call_args(code)
  the_arg <- grep(nm[1], arg_names)
  res <-
    if (length(the_arg) == 0) {
      new_checkr_result(action = "fail",
                        message = ifelse(nchar(message), message,
                                         paste0("could not find an argument named '", nm, "'")),
                        code = ex$code)
    } else {
      # we found a match, return it along with the environment
      code <- list(new_quosure(argv[[the_arg]], env = environment(code)))
      new_checkr_result("ok", code = code)
    }
  line_binding(res, I , ..., message = message, qkeys = quote({.(E); ..(V)}))
}
