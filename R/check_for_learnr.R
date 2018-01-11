#' Interface for checking learnr exercises.
#'
#' This is the interface from learnr to checkr. You don't call this function directly: that will be
#' done by learnr. Instead, you have to tell learnr to use this function. Do this with a directive
#' in the setup chunk of the learnr document: `tutorial_options(exercise.checker = checkr2::check_for_learnr)`
#'
#' @param label argument passed by learnr system
#' @param user_code ditto
#' @param solution_code ditto
#' @param check_code ditto
#' @param envir_result ditto
#' @param evaluate_result ditto
#' @param ... ditto
#' @param debug development flag to turn on logging of the information sent by learnr. This
#' must be set at compile time, it's not for users.
#'
#' @examples
#' # as it would be called from the learnr system ...
#' check_for_learnr(envir_result = 3, label = "first", user_code = "sin(pi)",
#'   check_code = 'ex <- for_checkr(USER_CODE)
#'                 t1 <- line_where(ex, F == sin, message="Please use the sin function.")
#'                 a1 <- arg_number(t1, 1); check(a1, passif(V == pi, "Right-oh!"))'
#' )
#'
#' #'
#' @export
check_for_learnr <-
  function(label=NULL,
         user_code = NULL,
         solution_code = NULL,
         check_code = NULL,
         envir_result = NULL,
         evaluate_result = NULL, ...,
         debug = FALSE) {
    # while debugging
    if(debug) {
      save_file_name <- sprintf("~/Downloads/CheckR/chunk-%s.rds", label)
      saveRDS(list(label = label,
                   user_code = user_code,
                   solution_code = solution_code,
                   check_code = check_code,
                   envir = envir_result,
                   evaluate_result = evaluate_result),
              file = save_file_name)
    }

    # Pre-evaluation checking
    # Only this part will be run for pre-evaluation. So a conclusive result must be returned.
    if (is.null(envir_result)) {
      res <- pre_check(user_code, solution_code)
      if ( ! res$correct) { # return a list in the right form for learnr
        return(list(correct = FALSE, type = "error", location = "prepend",
                    message = res$message))
      } else {
        return(TRUE)
      }
    }

    # Always check parsing if it wasn't checked before
    #
    # NOTE: This will only be relevant if the code is *not* evaluated by learnr.
    # That happens when there is a check-code chunk.
    # I want to turn it off more generally.
    if (! is.null(envir_result)) {
      res <- parse_check(user_code)
      if ( ! res$correct) { # return a list in the right form for learnr
        return(list(correct = FALSE, type = "error", location = "prepend",
                    message = res$message))
      }
    }

  # If we got here ...
  # The user code parsed successfully and, if there is a -check-code chunk,
  # it evaluated successfully. Now see if it passes the exercise author's tests.
  # res <- check(USER_CODE = user_code,
  #              tests = parse(text = check_code))
  # above is from original system
  parsed_check_code <- parse(text = check_code)
  res <- eval(parsed_check_code, envir = list(USER_CODE = user_code))

  if (res$action == "ok" && res$message == "") res$message = "Good"

  # turn the result into a value suitable for learnr
  feedback_type <- switch(res$action,
                          "pass" = "success",
                          "fail" = "error",
                          "ok" = "warning")
  final <- list(correct = (res$action == "pass"),
       message = res$message,
       type = feedback_type,
       location = "prepend"
  )

  final
}
