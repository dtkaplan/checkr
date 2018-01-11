#' Combine or negate checkr_results
#'
#' The outcome of a checkr test is an object of class `checkr_result` that indicates
#' whether the test was passed or failed or was sufficient to move on to additional tests.
#'
#' @param res1 the first checkr result
#' @param res2 the second checkr result
#'
#' @examples
#' # normally these results are made by the checking functions
#' code <- for_checkr(quote(1))
#' pa <- check(code, passif(V==1, "Good!"))
#' pb <- check(code, passif(V==1, "Great!"))
#' fa <- check(code, failif(V==1, "too bad"))
#' fb <- check(code, failif(V==1, "sorry"))
#' oka <- check(code, noteif(V==1, "note a"))
#' okb <- check(code, noteif(V==1, "note b"))
#' pa %or% pb
#' pa %and% pb
#' fa %or% fb
#' fa %and% fb
#' oka %or% okb
#' oka %and% okb
#' oka %and% fa
#' oka %or% fa
#' pa %or% fa
#' pa %and% fa
#' pa %or% oka
#' pa %and% oka
#' checkr2::not(pa)
#' checkr2::not(fa)
#'
#' @rdname logicals
#' @export
`%or%` <- function(res1, res2) {
  UseMethod("%or%")
}
#' @rdname logicals
#' @export
`%and%` <- function(res1, res2) {
  UseMethod("%and%")
}
#' @rdname logicals
#' @export
`%or%.checkr_result` <- function(res1, res2) {
  stopifnot(inherits(res2, "checkr_result"))
  # Make sure they are both checkr_result and then combine in some sensible way
  # 1) req  || pass -> pass
  # 2) req  || fail -> req
  # 3) req  || req  -> req
  # 4) pass || pass -> pass
  # 5) pass || fail -> pass
  # 6) fail || fail -> fail

  one <- res1$action
  two <- res2$action
  # combine the notes
  mess <- combine_messages(res1, res2)

  # Now the logic
  if (one == "pass" || two == "pass") { # Cases 1, 4, 5
    new_checkr_result("pass", message = mess$pass_message)
  } else if (one == "fail" || two == "fail") { # Cases 2, 6
    new_checkr_result("fail", message = mess$fail_message)
  } else if (one == "ok" || two == "ok") { # Case 3
    new_checkr_result("ok", message = mess$ok_note)
  } else stop("illegal combination of checkr_results")
}
#' @rdname logicals
#' @export
`%and%.checkr_result` <- function(res1, res2) {
  stopifnot(inherits(res2, "checkr_result"))
  # 1) pass && pass -> pass
  # 2) any fail   -> fail
  # 3) ok && ok   -> ok
  # 4) pass && ok -> pass

  one <- res1$action
  two <- res2$action
  # combine the notes
  mess <- combine_messages(res1, res2)

  if (one == "fail" || two == "fail") {
    new_checkr_result("fail", mess$fail_message)
  } else if (one == "pass" || two == "pass") {
    new_checkr_result("pass", mess$pass_message)
  } else if (one == "ok" && two == "ok") {
    new_checkr_result("ok", mess$ok_note)
  } else {
    stop("Illegal combination of checkr_results")
  }

}

#' @rdname logicals
#' @export
not <- function(res1) {
  stopifnot(inherits(res1, "checkr_result"))
  if (res1$action %in% c("pass", "ok")) res1$action = "fail"
  else res1$action = "pass"

  res1
}


# combine messages from checkr_results
combine_messages <- function(res1, res2) {
  if (!(inherits(res1, "checkr_result") && inherits(res2, "checkr_result")))
    stop("arguments must both be checkr_result class")

  if (is.null(res1)) res2
  if (is.null(res2)) res1

  one <- res1$action
  two <- res2$action
  # combine notes and messages
  # show either or both notes
  ok_note <- ""
  if (one == "ok" && res1$message != "") ok_note <- res1$message
  if (two == "ok" && res2$message != "") ok_note <- paste(ok_note, res1$message)
  # Show either or both fail messages
  fail_message <- ""
  if (one == "fail") fail_message <- res1$message
  if (two == "fail") {
    if(fail_message == "") fail_message <- res2$message
    else if (res1$message != res2$message)
      fail_message <- paste(fail_message, "*also*", res2$message)
  }
  # We just need one of the two pass messages
  pass_message <- ""
  if (one == "pass") pass_message <- res1$message
  if (two == "pass" && pass_message == "") pass_message <- res2$message

  if ( ok_note != "") {
    fail_message <- paste(fail_message, "And note:", ok_note)
    pass_message <- paste(pass_message, "But note:", ok_note)
  }

  list(ok_note = ok_note, fail_message = fail_message, pass_message = pass_message)
}

