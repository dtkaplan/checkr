#' Print a function with numbered lines.
#'
#' Many checkr statements will be packaged into functions with just one input: `USER_CODE`.
#' In narrative documentation for checkr, it can be nice to print these with lines numbered, so
#' that the narrative can refer to them. There's no reason to use this function in a learnr tutorial,
#' unless that tutorial is about checkr itself.
#'
#' Won't work for primitives, but then you'll never be writing primitives.
#'
#' @param fun Name of the function, e.g. `lm`.
#' @param just_the_body If `TRUE`, show just the body of the function. Otherwise, include
#' the function definition.
#' @param from_file name of a .R file. If the function resides in a .R file, giving `from_file`
#' will cause that .R file to be sourced.
#'
#' @export
print_function_contents <- function(fun, just_the_body = TRUE, from_file = NULL) {
  if (! is.null(from_file)) source(from_file)
  fun_name <- substitute(fun)
  body_text <- capture.output(body(fun))
  if (just_the_body) {
    body_text <- gsub("^ {4}", "", body_text[-c(1, length(body_text))])
    line_nums <- paste0("[", as.character(1:length(body_text)), "]")
    line_nums <- formatC(line_nums, width = max(nchar(line_nums)))
    cat(paste(line_nums, body_text, collapse = "\n"))
  } else {
    line_nums <- paste0("[", as.character(1:length(body_text[-1])), "]")
    line_nums <- formatC(line_nums, width = max(nchar(line_nums)))
    precursor <- paste(fun_name, "<- function(", paste(names(formals(fun)), collapse = ", "), ") {")
    cat(paste(precursor, paste(line_nums, body_text[-1], collapse = "\n"), sep = "\n"))
  }
}
