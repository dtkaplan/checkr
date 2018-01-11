#' Testing functions for formulas
#'
#' Provide facilities for checking the layout of formulas. Use these
#' with an input that is the *value* of a formula, that is, with a V binding rather
#' than an EX binding.
#'
#' @aliases formula_equals, two_sided, lhs_equals, rhs_equals, rhs_contains
#'
#' @param F a formula from a student submission.
#' @param target the desired attributes of the formula
#' @param ... expressions for the permissible parts of the formula
#'
#' @examples
#' CODE <- for_checkr(quote({
#'    data(mtcars, package = "datasets")
#'    mod <- lm(mpg ~ hp * wt + cyl, data = mtcars)
#'    }))
#' fa <- formula_arg(line_calling(CODE, lm),
#'   failif(! two_sided(V), "Modeling formulas should have two sides."),
#'   failif(! lhs_equals(V, mpg), "The response variable should be mileage."))
#' # or, another style for the same thing ...
#' check(formula_arg(line_calling(CODE, lm)),
#'   failif(! two_sided(V), "Modeling formulas should have two sides."),
#'   failif(! lhs_equals(V, mpg), "The response variable should be mileage."))
#' @rdname formula_tests
#' @export
formula_equals <- function(F, target) {
  target <- enexpr(target)
  F == target
}
#' @rdname formula_tests
#' @export
two_sided <- function(F) {
  length(F) == 3
}
#' @rdname formula_tests
#' @export
lhs_equals <- function(F, ...) {
  side_equal_helper(F, rlang::f_lhs, ...)
}
#' @rdname formula_tests
#' @export
rhs_equals <- function(F, ...) {
  side_equal_helper(F, rlang::f_rhs, ...)
}

side_equal_helper <- function(F, side_fun, ...) {
  targets <- targets_from_dots(...)
  F <- side_fun(F)
  test_fun <- function(x) {F == x}
  any(unlist(lapply(targets, test_fun)))
}

#' @rdname formula_tests
#' @export
rhs_contains <- function(F, ...) {
  targets <- targets_from_dots(...)
  # Need to figure out formula components
  stop("Not yet implemented.")
}

targets_from_dots <- function(...) {
  sapply(quos(...), quo_expr)
}

formula_components <- function(F) {
  # Give a unique number to each name in the formula, then do the calculation

}
