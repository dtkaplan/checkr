#' Find a line that is a magrittr chain
#'
#' Code submissions may consist of one or more lines. Each of those lines may be
#' written as a magrittr chain. `line_chaining()` will extract a single line that is a chain.
#'
#' @details A chain is itself equivalent to multiple lines. If you are interested in
#' exploring within a chain, you can expand the chain into individual lines. See `expand_chains()`.
#'
#' @param ex a checkr_result object such as produced by `for_checkr()`
#' @param n An integer indicating which line that is a chain is desired: first, second, ...
#' @seealso expand_chains, expand_all_chains
#'
#' @examples
#' code <- for_checkr(quote({x <- 3 %>% sin() %>% cos(); x %>% sqrt() %>% log()}))
#' line_chaining(code)
#' line_chaining(code, n = 2L)
#'
#' @export
line_chaining <- function(ex, n = 1L) {
  stopifnot(inherits(ex, "checkr_result"))
  if (failed(ex)) return(ex) # short circuit failure
  success_count <- 0
  for (m in seq_along(ex$code)) {
    if (is_chain(simplify_ex(ex$code[[m]]))) {
      success_count <- success_count + 1
      if (success_count == n) {
        res <- new_checkr_result(action = "ok", message = "",
                                 code = list(ex$code[[m]]))
        return(res)
      }
    }
  }
  ex$action = "fail"
  ex$message =
    if (success_count == 0 ) "Didn't find a chained command."
  else paste("Only", success_count, "chained command(s) found.")
  ex
}
