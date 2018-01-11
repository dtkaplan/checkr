#' Checking fill-in-the-blanks problems
#'
#' @param ex the expression to check, e.g. USER_CODE
#' @param pat the anticipated correct form. Use elements such as `..one..` to
#' define a blank.
#' @param ... tests to apply to expressions in `ex`. These are typically made
#' with `passif()`, `failif()`, and so on.
#'
#' @examples
#' submission <- for_checkr(quote({a <- 3; b <- 4; res <- sqrt(a^2 + b^2)}))
#' submission2 <- for_checkr(quote({a <- 3; b <- 4; res <- sin(a^2 + b^2)}))
#' # a template with a blank ..fn..
#' as_posted <- quote(res <- ..fn..(a^2 + b^2))
#' check_blanks(submission, !!as_posted,
#'    insist(fn == quote(sqrt), "{{fn}} is not the right function."))
#' check_blanks(submission2, !!as_posted,
#'    insist(fn == quote(sqrt), "{{fn}} is not the right function."))
#' # Multiple blanks
#' as_posted <- for_checkr(quote({res <- ..fn..(`+`(`^`(a, ..exp1..), `^`(b, ..exp2..)))}))
#' @export
check_blanks <- function(ex, pat, ...) {
  stopifnot(inherits(ex, "checkr_result"))
  cmd <- rlang::enquo(pat)
  tests <- rlang::quos(...)
  pat_str <- deparse(rlang::get_expr(cmd))
  blank_names <- unlist(
    stringr::str_extract_all(string = pat_str,
                             "\\.{2}[\\._a-zA-Z0-9]+\\.{2}")
  )
  blank_alphanum <- gsub("^..|..$", "", blank_names)

  # Construct a valid pattern to represent pat.
  # Replace names of blanks with properly formed pattern
  for (k in seq_along(blank_names)) {
    properly <- paste0(".(", blank_alphanum[k], ")")
    pat_str <- gsub(blank_names[k], properly, pat_str, fixed = TRUE )
  }
  pat <- as_bracketed_expressions(parse(text = pat_str))[[2]]
  bindings <- grab_bindings(ex, pat)

  if (inherits(bindings, "checkr_result")) {
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

