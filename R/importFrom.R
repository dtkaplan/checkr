#' @importFrom rlang eval_tidy quo_expr expr child_env quos new_quosure
#' @importFrom rlang enexpr is_formula expr_text quo enquo is_quosure
#' @importFrom rlang lang is_lang call_args call_args_names lang
#' @importFrom rlang is_call call_name call2
#' @importFrom rlang is_function caller_env eval_bare f_rhs f_lhs get_expr
#'
#' @importFrom magrittr %>%
#'
#' @importFrom utils capture.output head tail
#'
# The following is just to avoid an R CMD check note. I use dplyr in the vignette,
# but not otherwise in the package. Still, I need to import from it to avoid
# the CMD check message
#' @importFrom dplyr filter
NULL
