#' Functions to extract an the argument from an expression
#'
#' Use these functions to find a particular argument in an expression.
#'
#' @rdname find_arguments
#' @aliases formula_arg data_arg matrix_arg vector_arg list_arg table_arg first_arg
#' @return the matching expression as a quosure that can be evaluated
#' with eval_tidy().
#'
#' @importFrom utils capture.output head tail
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
#'   failif( ! "hp" %in% names(V),
#'           "The data should have a column named 'hp'."))
#' matrix_arg(code)
#' named_arg(code, "data", failif(EX == `mtcars`, "I didn't want mtcars."))
#' arg_number(code, 3)

#' @export
formula_arg <- function(ex, ..., n=1L, message = "") {
  res <- generic_arg(ex, "a formula e.g. a ~ b", is_formula, n = n, message = message)
  line_binding(res, I , ..., message = message, qkeys = quote({.(EX); ..(V)}))
}
#' @rdname find_arguments
#' @export
data_arg <- function(ex, ..., n=1L, message = "") {
  res <- generic_arg(ex, "a data frame", is.data.frame, n = n, message = message)
  line_binding(res, I , ..., message = message, qkeys = quote({.(EX); ..(V)}))
}
#' @rdname find_arguments
#' @export
matrix_arg <- function(ex, ..., n=1L, message = "") {
  res <- generic_arg(ex, "a matrix", is.matrix, n = n, message = message)
  line_binding(res, I , ..., message = message, qkeys = quote({.(EX); ..(V)}))
}
#' @rdname find_arguments
#' @export
vector_arg <- function(ex, ...,  n=1L, message = "") {
  res <- generic_arg(ex, "a vector", is.vector, n = n, message = message)
  line_binding(res, I , ..., message = message, qkeys = quote({.(EX); ..(V)}))
}
#' @rdname find_arguments
#' @export
character_arg <- function(ex, ...,  n=1L, message = "") {
  res <- generic_arg(ex, "a character string", is.character, n = n, message = message)
  line_binding(res, I , ..., message = message, qkeys = quote({.(EX); ..(V)}))
}
#' @rdname find_arguments
#' @export
numeric_arg <- function(ex, ..., n=1L, message = "") {
  res <- generic_arg(ex, "numeric", is.numeric, n = n, message = message)
  line_binding(res, I , ..., message = message, qkeys = quote({.(EX); ..(V)}))
}
#' @rdname find_arguments
#' @export
list_arg <- function(ex, ...,  n=1L, message = "") {
  res <- generic_arg(ex, "a list", is.list, n = n, message = message)
  line_binding(res, I , ..., message = message, qkeys = quote({.(EX); ..(V)}))
}
#' @rdname find_arguments
#' @export
function_arg <- function(ex, ..., n=1L, message = "") {
  res <- generic_arg(ex, "a function", is.function, n = n, message = message)
  line_binding(res, I , ..., message = message, qkeys = quote({.(EX); ..(V)}))
}
#' @rdname find_arguments
#' @export
table_arg <- function(ex, ..., n=1L, message = "") {
  res <- generic_arg(ex, "a table", is.table, n = n, message = message)
  line_binding(res, I , ..., message = message, qkeys = quote({.(EX); ..(V)}))
}

generic_arg <- function(tidy_expr, type_description, type_test,
                        message = "", pass = "", n = 1L, use_value = TRUE) {
  # Can also take results straight from for_checkr()
  if (inherits(tidy_expr, "checkr_result")) {
    # pass along any input that is already failed.
    if (failed(tidy_expr)) return(tidy_expr)
    if (length(tidy_expr$code) > 1) stop("Narrow down to a single line of code before calling.")
    code <- skip_assign(tidy_expr$code[[1]])
  } else {
    code <- tidy_expr # it was a straight quosure, not a checkr_result
  }

  if (message == "") {
    # Pre-form the failure message
    fail <- paste(rlang::expr_text(rlang::quo_expr(code)),
                  "doesn't contain an argument that is",
                  type_description)
  }
  bad_return <- new_checkr_result(action = "fail", message = message, code = tidy_expr$code)

  if ( ! rlang::is_lang(code)) {
    # if it's the right kind of object, just return that
    if (type_test(code)) {
      # create a dummy expression whose value is the value of this thing
      # Q <- quo(v)
      # environment(Q) <- list(v = code)
      Q <- new_quosure(expr = as.name("v"), env = list(v = code))
      return(new_checkr_result("ok", code = list(Q)))
    }
    else return(bad_return)
  }

  # But usually will be a tidy expression containing a call
  this_env <- environment(code)
  the_args <- rlang::lang_args(code)
  found_target <- FALSE
  target <- NULL
  found_count <- 0

  for (k in 1:length(the_args)) {
    val <- if (use_value && (is.name(the_args[[k]]) || is.call(the_args[[k]]))) {
      rlang::eval_tidy(the_args[[k]], data = this_env)
    } else {
      the_args[[k]]
    }

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
    code = list(rlang::new_quosure(target, env = this_env))
    if (nchar(pass)) new_checkr_result("pass", message = pass, code = code)
    else new_checkr_result("ok", code = code)
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
  argv <- rlang::lang_args(code)
  res <-
    if (length(argv) < n) {
      new_checkr_result(action = "fail",
                        message = paste(rlang::expr_text(rlang::quo_expr(code)),
                                        "does not have", n, "arguments"),
                        code = ex$code)

    } else {
      code <- list(rlang::new_quosure(argv[[n]], env = environment(code)))
      new_checkr_result("ok", code = code)
    }
  line_binding(res, I , ..., message = message, qkeys = quote({.(EX); ..(V)}))
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

  arg_names <- rlang::lang_args_names(code)
  argv <- rlang::lang_args(code)
  the_arg <- grep(nm[1], arg_names)
  res <-
    if (length(the_arg) == 0) {
      new_checkr_result(action = "fail",
                        message = paste0("could not find an argument named '", nm, "'"),
                        code = ex$code)
    } else {
      # we found a match, return it along with the environment
      code <- list(rlang::new_quosure(argv[[the_arg]], env = environment(code)))
      new_checkr_result("ok", code = code)
    }
  line_binding(res, I , ..., message = message, qkeys = quote({.(EX); ..(V)}))
}
