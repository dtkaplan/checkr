#' Grab any bindings from a pattern match
#'
#' UNLIKE the `check_` functions which return `checkr_result` objects,
#' `grab_bindings` returns a list with the values matched to the keys.
#'
#' @param ex the expression to be searched for the patterns
#' @param ... the set of patterns. Each pattern must be be enclosed in `quote()`. See details.
#' @param key a single pattern enclosed in `quote()`.
#' @param one_for_all Logical. If `TRUE`, then all patterns must match at least one
#' expression in ex.
#' @param all_for_one Logical. If `TRUE`, then there must be an expression
#' that matches all patterns.
#'
#' @details The patterns in `...` will often have internal commas. To distinquish these
#' from separate arguments to `grab_bindings()`, each pattern must be quoted, e.g. `quote(plot(.(one), .(two)))`.
#' Note that the patterns should include assignment if there is to be any.
#'
#' @examples
#' ex <- for_checkr(quote(plot(mpg ~ wt, data = subset(mtcars, hp < 150))))
#' grab_bindings(ex, quote(.(fn)(.(formula), data = .(the_data))))
#' ex2 <- for_checkr(quote({x <- 1; y <- x^2}))
#' grab_bindings(ex2, quote(`<-`(x, ..(first))),
#'      quote(`<-`(.(var), .(fn)(x, .(exponent)))), all_for_one = FALSE)
#' grab_bindings(ex2, quote(`<-`(x, ..(first))),
#'       quote(`<-`(.(var), .(fn)(x, .(exponent)))))
#' grab_binding_anywhere(for_checkr(quote({4 - 7;sin(3 + 2)})), quote(`+`(.(a), .(b))))
#' grab_binding_anywhere(for_checkr(quote({4 - 7;sin(3 + 2)})), quote(.(fn)(3 + 2)))
#' grab_binding_anywhere(for_checkr(quote({4 - 7;sin(3 + 2)})), quote(`-`(.(a), .(b))))
#' @export
grab_bindings <- function(ex, ...,
                          all_for_one = TRUE,
                          one_for_all = FALSE) {
  stopifnot(inherits(ex, "checkr_result"))
  keys <- list(...)
  bindings <- list()

  if (length(keys) == 0) stop("No expressions given for argument 'keys'.")
  expressions_matched <- rep(FALSE, length(ex$code))
  for (m in seq_along(ex$code)) {
    patterns_matched <- rep(FALSE, length(keys))

    for (k in seq_along(keys)) {
      # a formula whose RHS copies the environment
      # that node_match will put in .data
      pattern <- LHS ~ copy_env(.data)
      rlang::f_lhs(pattern) <- rlang::expr( !! keys[[k]])

      # Grab the list of bindings
      new_bindings <-
        try(redpen::node_match(ex$code[[m]], !!pattern),
            silent = TRUE)

      if (inherits(new_bindings, "try-error") || is.null(new_bindings)) {
        next
      } else {
        expressions_matched[m] <- TRUE
        patterns_matched[k] <- TRUE
        new_bindings$. <- NULL
        bindings <- c(bindings[! names(bindings) %in% names(new_bindings)], new_bindings)
      }
    }

  }
  if (all_for_one && ! all(patterns_matched)) return(NULL)

  if (one_for_all && ! all(expressions_matched)) return(NULL)

  # success!
  bindings
}

#' @rdname grab_bindings
#' @export
grab_binding_anywhere <- function(ex, key) {
  stopifnot(inherits(ex, "checkr_result"))
  if (failed(ex)) return(ex) # short circuit
  for (m in seq_along(ex$code)) {
    pattern <- LHS ~ copy_env(.data)
    rlang::f_lhs(pattern) <- rlang::expr( !! key)
    bindings <-redpen::node_match(ex$code[[m]], !!pattern)
    if ( ! is.null(bindings)) return(bindings)
    else {# no match, so try recursively with arguments
      if (is.call(ex$code[[m]])) {
        args <- rlang::lang_args(ex$code[[m]])
        tmp <- ex
        for (k in seq_along(args)) {
          tmp$code <- args[k]
          res <- grab_binding_anywhere(tmp, key)
          if (! is.null(res)) return(res)
        }
      }
    }
  }


  NULL
}
