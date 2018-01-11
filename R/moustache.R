#' evaluate expressions in a string using a particular environment
#'
#' @param string a character string which presumably contains some moustaches
#' referring to objects found in the bindings environment.
#' @param bindings an environment or list in which the objects moustached in `string` are defined.
#' @examples
#' checkr2:::moustache("hello")
#' checkr2:::moustache("Three plus five is {{3+5}}.")
moustache <- function(string, bindings = rlang::env_parent()) {
  # pull out all the instances of {{expr}} from the string
  the_moustache <- '\\{\\{.*?\\}\\}' # not greedy
  matches <- unlist(stringr::str_extract_all(string, the_moustache))
  if (length(matches) == 0) return(string)
  # evaluate the contents of the moustache
  expressions <- gsub("\\}\\}", "", gsub("\\{\\{", "", matches))
  for (j in seq_along(expressions)) {
    val <- try(eval(parse(text = expressions[j]), envir = bindings))
    if (inherits(val, "try-error")) {
      # it wasn't a valid expression
      val <- paste0("'{{", expressions[j], "}}' could not be evaluated.")
    }
    string <- gsub(matches[j], to_sensible_character(val), string, fixed = TRUE)
  }

  return(string)
}

to_sensible_character <- function(v) {
  if (is.vector(v)) {
    v <- if(is.numeric(v)) signif(v, 3) else v
    if (length(v) > 5) {
      S <- paste(paste(as.character(head(v,2)), collapse = ", "),
            "...",
            paste(as.character(tail(v,2)),collapse = ", "))

    } else {
      S <- paste(as.character(v), collapse = ",")
    }

    if (length(v) > 5) paste0("a vector of length ", length(v), ": ", S)
    else S
  } else if (is.matrix(v)) {
      paste(paste(dim(v), collapse = "by"), "matrix with vals", to_sensible_character(v[]))
  } else if (is.data.frame(v)) {
    paste("data frame with", nrow(v), "rows,",
          length(v), "columns, and names", to_sensible_character(names(v)))
  } else {
    deparse(v)
  }
}
