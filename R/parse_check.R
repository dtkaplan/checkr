#' Look for errors at parse time
#'
#' Parses the code (without evaluating it) to look for parse errors. If any are found,
#' a somewhat friendly message is returned.
#'
#' @param user_code a character string containing the code to parse. Note, the input
#' cannot be a checkr_result, since constructing one of those requires the code to be
#' executed.
#'
#'
#' @examples
#' code <- "lm(mpg ~ hp, \ndata == mtcars); \nplot(1:10)\n x <- f[b]\n y <- x^2\n\n z = cos(yy * xx^2)"
#' cat(parse_check(code)$message)
#' code2 <- "x <- 3\ny <- x^2\n__blank__ <- 2"
#' cat(parse_check(code2)$message)
#' code3 <- "x["
#' cat(parse_check(code3)$message)
#' @export
parse_check <- function(user_code) {
  line_no <- 0
  column_no <- 0
  message <- "no message"
  kind_message <- "Nice!"
  marked_code <- ""
  correct <- TRUE
  # parse without evaluating
  parsed <- evaluate::parse_all(user_code, allow_error = TRUE)
  # look for a problem
  if ("PARSE_ERROR" %in% names(attributes(parsed))) {
    # there's a problem
    correct = FALSE
    problem <- attr(parsed, "PARSE_ERROR")
    # get the line number
    position <- stringr::str_match(problem$message, "^<text>:([0-9]+):([0-9]+):")
    line_no <- position[1,2]
    column_no <- position[1,3]
    marked_code <- gsub("^[^\n]+\n", "", problem$message)
    # What kind of problem?
    if (grepl("unexpected INCOMPLETE_STRING", problem$message)) {
      kind_message <- "you seem to have unmatched quotation marks. The system is looking for a closing quotation mark at or before the point marked."
    } else if (grepl("unexpected end of input", problem$message)) {
      kind_message <- "your command<br> seems to have ended prematurely, before it was completed."
    } else if (grepl("unexpected input", problem$message)) {
      kind_message <- "you have a character that's not appropriate, e.g.\n- a name that starts with _\n- There must be some OTHERS..."
    } else if (grepl("unexpected '", problem$message)) {
      symbol <- stringr::str_match(problem$message, "unexpected ('[^']*')")
      kind_message <- paste("you have extra punctuation:", symbol[1, 2])
    } else if (grepl("unexpected symbol", problem$message)) {
      kind_message <- "you are missing something at or before the point marked: perhaps\n- a punctuation mark (such as a comma)\n- an arithmetic symbol (or something like that).\nOr you might have an extra opening parenthesis or bracket before the point marked."
    } else {
      kind_message <- "there is an unanticipated kind of parsing error. Tell Danny."
    }
  }

  final_message <-
    if (correct) "No parsing problems!"
    else sprintf("The R system can't figure out your command.\nNear line %s, %s\n%s",
                           line_no, kind_message, marked_code)

  list(correct = correct, message = final_message)

}
