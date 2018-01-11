#' Defunct functions from checkr (v 0.1)
#'
#' These functions were in version 0.1 of checkr, but as of
#' version 0.3 they are not part of the system. If you are using them,
#' you are using an old version of checkr. If you want to continue using them,
#' install that old version: `devtools::install_github("dtkaplan/checkr-v-0.1")`
#'
#' @param ... whatever arguments the original functions took
#'
#' @rdname obsolete_funs
#' @export
capture.code <- function(...)
.Defunct("none available", "",
         paste("For the original version of checkr which contained this function,",
               "see github.com/dtkaplan/checkr-v-0.1",
               "But don't start anew with that defunction version.",
               "Switch to the current version of checkr"))

#' @rdname obsolete_funs
#' @export
find_assignment <- capture.code
#' @rdname obsolete_funs
#' @export
find_call <- capture.code
#' @rdname obsolete_funs
#' @export
find_constants <- capture.code
#' @rdname obsolete_funs
#' @export
find_formula <-capture.code
#' @rdname obsolete_funs
#' @export
find_names <- capture.code
#' @rdname obsolete_funs
#' @export
all_tests <- capture.code
#' @rdname obsolete_funs
#' @export
any_test <- capture.code
#' @rdname obsolete_funs
#' @export
arg_is <- capture.code
#' @rdname obsolete_funs
#' @export
checkr_tutor <- capture.code
#' @rdname obsolete_funs
#' @export
check_argument <- capture.code
#' @rdname obsolete_funs
#' @export
check_function_calls <- capture.code
