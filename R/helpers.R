# Functions internal to the checkr package.

# utility for copying out the bindings defined by redpen pattern
copy_env <- function(E) {
  res <- list()
  nms <- names(E)
  for (nm in nms)
    res[[nm]] <- E[[nm]]

  res
}


# utility for turning the output of parse into a bracketed set of
# expressions
as_bracketed_expressions <- function(ex) {
  if (inherits(ex, "character")) ex <- parse(text = ex)
  if (inherits(ex, "expression")) {
    # ex came from the parser, not quote.
    # turn it into a bracketed set of expressions
    Res <- quote({})
    for (k in seq_along(ex))
      Res[[k+1]] <- ex[[k]]
  } else if ( ! inherits(ex, "{")) { # it's a single expression
    # put into the framework of a bracketed set of expressions
    Res <- quote({})
    Res[[2]] <- ex
  } else {
    Res <- ex
  }

  Res

}

# Utility for simplifying expressions that are gratuitously wrapped in
# parentheses and stripping off assignment.
# NOTE: Any expression like (2+2+2) doesn't need the parens. Expressions
# like (2 + 2)*4 need the parens, but the root of the parse tree will
# be * rather than (. So get rid of extraneous parens.
simplify_ex <- function(ex) {
  stopifnot(inherits(ex, "quosure"))
  ex <- skip_assign(ex)
  ex <- rlang::new_quosure(simplify_ex_helper(rlang::quo_expr(ex)),
                           env = environment(ex))

  ex
}
simplify_ex_helper <- function(raw_ex) { # recursive to remove nested parens.
  if (inherits(raw_ex, "(")) simplify_ex_helper(raw_ex[[2]])
  else raw_ex
}

new_checkr_result <- function(action = "ok", message = "", code = NULL) {
  res <- list(action = action, message = message)
  if ( ! is.null(code)) res$code <- code
  class(res) <- "checkr_result"

  res
}

generic_test <- function(pass = c("pass", "fail", "ok"),
                         fail = c("pass", "fail", "ok"),
                         default_message = "default test message",
                         fail_message = "") {
  pass <- match.arg(pass)
  fail <- match.arg(fail)
  function(test, message = default_message) {
    test <- rlang::enquo(test)
    function(task, res) {
      if (task == "test") test
      else if (task == "message") ifelse(res, message, "")
      else if (task == "action") ifelse(res, pass, fail)
    }
  }
}

# Get the lead function (ignoring any assignment)
get_function <- function(tidy_expr) {
  tidy_expr <- quo_expr(tidy_expr)
  # SPECIAL CASE: Formulas
  if (as.character(tidy_expr[[1]]) == "~") return("~")
  if (rlang::is_call(tidy_expr)) rlang::call_name(tidy_expr)
  else NULL
}
# Get the name being assigned to. "" if no assignment.
get_assignment_name <- function(tidy_expr){
  if ( ! rlang::is_lang(rlang::quo_expr(tidy_expr))) return("")
  res <- redpen::node_match(tidy_expr, `<-`(.(name), ...) ~ rlang::expr_text(name))

  if (is.null(res)) ""
  else res
}
# modify the expression to remove assignment.
skip_assign <- function(ex) {
  stopifnot(inherits(ex, "quosure"))
  if ( ! rlang::is_lang(rlang::quo_expr(ex))) {
    ex
  } else {
    if ("<-" == call_name(ex)) {
      skip_assign(
        rlang::new_quosure(rlang::quo_expr(rlang::call_args(ex)[[2]]),
                           environment(ex)))
    } else {
      rlang::new_quosure(rlang::quo_expr(ex), env = environment(ex))
    }
  }
}

