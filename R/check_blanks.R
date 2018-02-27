#' Checking fill-in-the-blanks problems
#'
#' Specify a binding pattern in terms of an expression where elements like `..one..` or `..x..`, etc
#' set the names to be bound to the corresponding part of the code.
#'
#' For formulas, the `~` should be encompassed by the named element. For instance, if the user is
#' shown a statement like `mean( ~ hp, data = mtcars)`, the blanks statement should be something like
#' `mean(..v.., data = mtcars)` and NOT `mean( ~ ..v.., data = mtcars).`
#'
#' @param ex the expression to check, e.g. USER_CODE
#' @param pat a quoted expression containing the anticipated correct form. Use elements such as `..one..` to
#' define a blank. If there are multiple expressions (say, to be matched against multiple
#' lines of a submission) `pat` should be a list of quoted expressions.
#' @param ... tests to apply to expressions in `ex`. These are typically made
#' with `passif()`, `failif()`, and so on.
#'
#' @details In `pat`, you can use `.` (for a name or call) and `...` (for remaining arguments) to stand for components of the pattern you don't care about.
#' @examples
#' submission <- for_checkr(quote({a <- 3; b <- 4; res <- sqrt(a^2 + b^2)}))
#' submission2 <- for_checkr(quote({a <- 3; b <- 4; res <- sin(a^2 + b^2)}))
#' # a template with a blank ..fn..
#' as_posted <- quote(res <- ..fn..(a^2 + b^2))
#' check_blanks(submission, as_posted,
#'    insist(fn == quote(sqrt), "{{fn}} is not the right function."),
#'    passif(TRUE, "Right! You're using the square-root function."))
#' check_blanks(submission2, as_posted,
#'    insist(fn == quote(sqrt), "{{fn}} is not the right function."),
#'    passif(TRUE, "Right! You're using the square-root function."))
#' as_posted2 <- quote(b <- ..bval..)
#' check_blanks(submission, list(as_posted, as_posted2),
#'    insist(fn == quote(sqrt), "Wrong function."),
#'    insist(bval == 5, "`b` is not supposed to be {{bval}}."))
#'
#' # Multiple blanks
#' as_posted <- for_checkr(quote({res <- ..fn..(`+`(`^`(a, ..exp1..), `^`(b, ..exp2..)))}))
#' @export
check_blanks <- function(ex, pat, ...) {
  stopifnot(inherits(ex, "checkr_result"))
  if (!is.call(pat)) {
    stopifnot(is.list(pat))
    stopifnot(is.call(pat[[1]]))
  } else {
    pat <- list(pat)
  }

  tests <- quos(...)


  # Construct a valid pattern to represent pat.
  # Replace names of blanks with properly formed pattern

  fixed_pat <- list()
  for (j in seq_along(pat)) {
      pat_str <- deparse(get_expr(pat[[j]]))
      blank_names <- unlist(
        stringr::str_extract_all(string = pat_str,
                                 "\\.{2}[\\._a-zA-Z0-9]+\\.{2}"))
      blank_alphanum <- gsub("^..|..$", "", blank_names)
      for (k in seq_along(blank_names)) {
        properly <- paste0(".(", blank_alphanum[k], ")")
        pat_str <- gsub(blank_names[k], properly, pat_str, fixed = TRUE )
      }
    fixed_pat[[j]] <- as_bracketed_expressions(parse(text = pat_str))[[2]]
  }
  bindings <- grab_bindings(ex, key_list = fixed_pat, all_for_one = FALSE, one_for_all = FALSE)




  if (is.null(bindings)) {
    # there was no match to the pattern
    return(new_checkr_result(action = "fail", message = "You changed something other than the blanks. Start over from the beginning."))
  }
  run_tests(tests, bindings, ex)
}


one <- quote(hp)
two <- quote(200)
S <- deparse(quote({x <- ..zero..
  mtcars %>% filter(..one.. < ..num..) %>%
  summarise(mpg = mean(mpg))}))

