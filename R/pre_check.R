#' Pre-check code
#'
#' Checks first whether the code parses. If so, funs the code looking for errors.
#' If any errors are found, a diagnostic message is returned.
#'
#' @param user_code A character string containing the code to be checked. Note that
#' this is a character string, not a checkr_result. Constructing a checkr_result requires
#' that the code be evaluated, while this function is for pre-evaluation checking.
#' @param soln_code Code, if any, containing a correct solution.
#'
#' @return A list containing fields `correct` and `message`. If `correct`
#' is `TRUE`
#'
#' @examples
#' code <- "lm(mpg ~ hp, data <- mtcars); plot(1:10); x <- 1\n y <- x^2\n\n z = cos(yy * xx^2)"
#' pre_check(code)
#'
#'
#' @export
pre_check <- function(user_code, soln_code = "hello") {
  # first, see if the user_code can be parsed
  parse_result <- parse_check(user_code)
  if ( ! parse_result$correct) return(parse_result)

  # an environment in which to check the code
  ex_env <- new.env(parent = rlang::caller_env())
  # evaluate the user code (presented as a string)
  # stop on first error
  parsed <- evaluate::evaluate(user_code, envir = ex_env, stop_on_error = 1L)
  is_error <- function(element) {
    any(class(element) %in% c("simpleError", "error", "condition"))
  }
  is_source <- function(element) {
    any(class(element) %in% c("source"))
  }
  is_output <- function(element) {
    any(class(element) %in% c("character", "recordedplot"))
  }
  eliminate_output_lines <- function(parsed) {
    output_lines <- which(unlist(lapply(parsed, is_output)))

    if (length(output_lines) == 0) parsed
    else parsed[-output_lines]
  }
  get_code_line <- function(element) {
    ifelse(any(class(element) %in% c("source")), element$src, NULL)
  }

  # Some lines contain character string representations of the output
  # produced. Get rid of these to simplify things.
  parsed <- eliminate_output_lines(parsed)
  problem_line_no <- which(unlist(lapply(parsed, is_error)))
  if (length(problem_line_no) == 0) # we're done
    return(list(correct = TRUE, message = "Pre-check passed!"))

  # What source line did the problem come up in?
  source_line <- lapply(parsed, is_source)
  line_no <- sum(unlist(source_line)) + 1

  # A plan for expansion???
  # Use these for later comparison
  # Or will this be too varied from perhaps correct answers
  valid_symbols <- all.vars(parse(text = soln_code))

  error_string <- parsed[[line_no]]$message
  error_call <- redpen::node_match(parsed[[line_no]]$call, .(fn)(...) ~ fn)

  # Look for undefined objects
  match <- find_error_name('object {{var}} not found', error_string)
  if ( ! is.na(match)) {
    # it was an undefined object. What kind?
    kind_of_object <- "variable"
    if (grepl("data", as.character(error_call))) {
        kind_of_object <- "data frame"
    } else {
      # "eval" -- variable
      kind_of_object <- paste("unknown with call", as.character(error_call))
    }
    return(list(correct = FALSE,
                message =
                  paste0("On line ", line_no - 1, " or ", line_no, ": '", match,
                         "' is not the name of an existing ", kind_of_object, ".")))
  }
  # Look for undefined functions
  match <- find_error_name('could not find function {{var}}', error_string)
  if ( ! is.na(match)) {
    # it was an undefined function.
    return(list(correct = FALSE,
                message =
                  paste0("On line ", line_no, ": '", match, "' is not the name of any function.")))
  }

  list(correct = TRUE,
       message = paste("Failure not yet included in pre_check(). Code was",
                       as.character(error_call), "with error: ", error_string))
}

# Pull out a problem name, if any, from an error message
# returns NA if there's no match
#
# MAYBE GIVE THE SYMBOL LIST SO THAT A SUGGESTION CAN BE MADE??
find_error_name <- function(str, message) {
  var_pattern <- '[\\\'\\"]([._a-zA-Z0-9]*)[\\\'\\"]'
  str <- gsub("{{var}}", var_pattern, str, fixed = TRUE)
  stringr::str_match(message, str)[1,2]
}

