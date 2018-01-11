## ----setup, include = FALSE----------------------------------------------
library(checkr)
library(ggplot2)
#library(mosaic)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
s1 <- "lm(mpg ~ hp, data = mtcars)" # Right!
s1wrong <- "lm(mpg ~ hp, data = head(mtcars))"
s2 <- "mod <- lm(mpg ~ hp, data = mtcars); summary(mod)" # Right!
s2wrong <- "mod <- lm(hp ~ mpg, data = mtcars); summary(mod)"
s3wrong <- "for_me <- mtcars; mosaic::rsquared(lm(data = for_me, mpg ~ 1))"

## ----echo = FALSE--------------------------------------------------------
check_exer_1 <- function(USER_CODE) {
  code <- for_checkr(USER_CODE) # pre-processing
  lm_line <- line_calling(code, lm, message = "Use lm() to construct the model.")
  lm_call <- arg_calling(lm_line, lm)
  t1 <- data_arg(lm_call, 
                 insist(identical(V, mtcars), 
                        "Your data argument {{EX}} was not `mtcars`."),
                 message = "You didn't supply a `data = ` argument to `lm()`.")
  if (failed(t1)) return(t1)
  f <- formula_arg(lm_call,
                  message = "You didn't give a formula specifying the structure of the model.")
  t2 <- check(f, insist(two_sided(f), "There's no response variable in your formula."))
  t2 <- check(t2, insist(rlang::f_lhs(EX) == as.name("mpg"), 
                   paste("You need to have the miles-per-gallon variable",
                         "on the left side of the model formula.",
                         "You've got {{rlang::f_lhs(V)}} instead.")))
  if (failed(t2)) return(t2)
  check(lm_call, 
        insist(summary(V)$r.squared > 0.3, 
        "Your R-squared is {{summary(V)$r.squared}}. That's too small."),
        passif(TRUE, "Great job!"))
}

## ----echo = FALSE, comment = ""------------------------------------------
print_function_contents(check_exer_1, just_the_body = FALSE)

## ------------------------------------------------------------------------
check_exer_1(s1)
check_exer_1(s1wrong)
check_exer_1(s2)
check_exer_1(s2wrong)
check_exer_1(s3wrong)

## ------------------------------------------------------------------------
USER_CODE <- quote(y <- 15 * sin(53 * pi / 180))

## ------------------------------------------------------------------------
CODE <- for_checkr(USER_CODE)

## ------------------------------------------------------------------------
line_where(CODE, is.numeric(V), abs(V - 11.98) < 0.01, message = "Wrong numerical result.")

## ------------------------------------------------------------------------
CODE <- for_checkr(USER_CODE)
# CODE <- for_checkr(quote(11.98))
# CODE <- for_checkr(quote(sin(53)))
# CODE <- for_checkr(quote(15 * cos(53)))
t1 <- line_calling(CODE, sin, cos, tan, message = "You should be using a trigonometric function.")
t1 <- line_where(t1, F == quote(`*`),
              message = "Remember to multiply by the length of the hypotenuse")
line_where(t1, is.numeric(V), abs(V - 11.98) < 0.01, message = "{{V}} is a wrong numerical result. It should be about 11.98.")

## ------------------------------------------------------------------------
CODE <- for_checkr(quote(15 * cos(53)))
t1 <- line_calling(CODE, sin, cos, tan, message = "You should be using a trigonometric function.")
t1 <- misconception(t1, line_calling(t1, cos), message = "Are you sure cosine is the right choice?")
t1 <- line_where(t1, F == quote(`*`),
              message = "Remember to multiply by the length of the hypotenuse")
line_where(t1, is.numeric(V), abs(V - 11.98) < 0.01, message = "{{V}} is a wrong numerical result. It should be about 11.98.")

## ----echo = FALSE--------------------------------------------------------
CHECK <- function(submission) 
  if_matches(submission, .(fn)(..(ang)), 
             insist(fn == quote(sin), "{{fn}} is not the correct trig function."),
             failif(ang == 53, "You need to convert the 53 degrees into radians."),
             insist(ang == 53 * pi / 180, "Do you have the angle right?"),
             failif(TRUE, "Remember to take the length of the hypothenuse into account."))

## ----eval = FALSE--------------------------------------------------------
#  if_matches(submission, .(fn)(..(ang)),
#             insist(fn == quote(sin), "{{fn}} is not the correct trig function."),
#             failif(ang == 53, "You need to convert the 53 degrees into radians."),
#             insist(ang == 53 * pi / 180, "Do you have the angle right?"),
#             failif(TRUE, "Remember to take the length of the hypothenuse into account."))

## ----eval = FALSE, echo = FALSE------------------------------------------
#  CHECK(quote(15*sin(53 * pi/180)))

## ----eval = FALSE, echo = FALSE------------------------------------------
#  CHECK(quote(sin(53 * pi/180)))

## ----eval = FALSE, echo = FALSE------------------------------------------
#  CHECK(quote(cos(53 * pi / 180)))

## ----eval = FALSE, echo = FALSE------------------------------------------
#  CHECK(quote(sin(53)))

## ----eval = FALSE--------------------------------------------------------
#  if_matches(submission, .(fn)(..(arg)),
#             failif(TRUE, "Remember to take the length of the hypothenuse into account."))
#  if_matches(submission, 15 * .(fn)(..(arg)),
#             insist(fn == quote(sin), "{{fn}} is not the correct trig function."),
#             failif(ang == 53, "You need to convert the 53 degrees into radians."),
#             insist(ang == 53 * pi / 180, "Do you have the angle right?"),
#             passif(TRUE, "Good job!"))

## ----eval = FALSE--------------------------------------------------------
#  if_matches(submission, .(fn)(..(arg)),
#             failif(TRUE), "Remember to take the length of the hypothenuse into account.")
#  if_matches(submission, .(hyp) * .(fn)(..(ang)),
#             failif(hyp == 225, "Use the length, not the square length!"),
#             insist(hyp == 15, "What length are you using?"),
#             insist(fn == quote(sin), "{{fn}} is not the correct trig function."),
#             failif(ang == 53, "You need to convert the 53 degrees into radians."),
#             insist(ang == 53 * pi / 180, "Do you have the angle right?"),
#             passif(TRUE, "Good job!"))

## ------------------------------------------------------------------------
submission_2 <- "theta <- 53 * pi/180; r <- 15; r*sin(theta)"
submission_3 <- "ang <- pi * (53 / 180); sin(ang) * 15"

## ----echo = FALSE--------------------------------------------------------
library(ggplot2)
ggplot(mtcars, aes(x = mpg, y = hp, color = cyl)) +
  geom_point()

## ----eval = FALSE--------------------------------------------------------
#  library(ggplot2)
#  ggplot(mtcars, aes(x = ..x.., y = ..y.., color = ..c..)) +
#    ..geom..()

## ------------------------------------------------------------------------
submission <- "library(ggplot2); 
ggplot(mtcars, aes(x = hp, y = mpg, color = cyl)) +
  geom_point()"

## ----eval = FALSE--------------------------------------------------------
#  check_blanks(submission,
#               ggplot(mtcars, aes(x = ..x.., y = ..y.., color = ..c..)) + ..geom..(),
#               passif(x == quote(mpg) && y == quote(hp) &&
#                        c == quote(cyl) && geom == quote(geom_point),
#                      "Good job! {{x}}, {{y}}, {{c}}, and {{geom}}"),
#               noteif(x != quote(mpg), "{{x}} is not the variable on the horizontal axis."),
#               noteif(y != quote(hp), "{{y}} is not the right variable for the vertical axis"),
#               noteif(c != quote(cyl), "{{c}} is not the right variable to map to color."),
#               noteif(geom != quote(geom_point), "{{geom}} is not the correct geom to make a scatter plot."),
#               failif(TRUE, "Try again."))

## ----eval = FALSE--------------------------------------------------------
#  C <- .....(A^2 + B^2)

## ----echo = FALSE--------------------------------------------------------
CHECK2 <- function(submission) check_blanks(submission, C <- ........(A^2 + B^2),
             passif(.... == quote(sqrt), "Right!"),
             insist(.... == quote(sqrt), "Think again. {{....}} is not the right function to use."))

## ----eval = FALSE--------------------------------------------------------
#  check_blanks(submission, C <- ........(A^2 + B^2),
#               passif(.... == quote(sqrt), "Right!"),
#               insist(.... == quote(sqrt), "Think again. {{....}} is not the right function to use."))

## ----eval = FALSE, echo = FALSE------------------------------------------
#  CHECK2("C <- sqrt(A^2 + B^2)")

## ----eval = FALSE, echo = FALSE------------------------------------------
#  CHECK2("C <- log(A^2 + B^2)")

## ----eval = FALSE, echo = FALSE------------------------------------------
#  CHECK2("C <- ........(A^2 + B^2)")

## ----eval = FALSE, echo = FALSE------------------------------------------
#  CHECK2("C <- sqrt(A + B)")

## ----error = TRUE--------------------------------------------------------
a

## ----eval = FALSE--------------------------------------------------------
#  submission <- "x <- 3; sin(x)"
#  # This will work. `fn` will be a symbol, as will `quote(sin)`.
#  if_matches(submission, .(fn)(.(arg)), passif(fn == quote(sin), "Function is {{fn}}, argument is {{arg}}"))
#  # This won't work. `fn` will be a symbol, but `sin` is a function, not a symbol.
#  if_matches(submission, .(fn)(.(arg)), passif(fn == sin, "Function is {{fn}}, argument is {{arg}}"))
#  # Back to working again. With the double dots, `fn` will be the value that the name "sin" points to. In this case, that will be the function `sin()`.
#  if_matches(submission, ..(fn)(.(arg)), passif(fn == sin, "Function is {{fn}}, argument is {{arg}}"))
#  # The funny `.Primitive("sin")` reflects that the function `sin` is something called a "primitive," as opposed to the kind of thing like `function(x) x^2`. This will certainly be confusing as a message to students, so better to use comparison of the very first form, where the single-dot pattern is used and compared to `quote(sin)`.
#  

## ----eval = FALSE--------------------------------------------------------
#  if_matches(quote(x <- 3 + 2), `<-`(.(nm), ..(val)),
#             passif(nm == quote(X) || nm == quote(x), "{{nm}} was assigned the value {{val}}"))

## ----eval = FALSE--------------------------------------------------------
#  submission <- quote({x <- pi; cos(x)})
#  pattern <- quote(.(fn)(.(var))) # will have to unquote
#  if_matches(submission, !!pattern, passif(TRUE, "Found match"))
#  if_matches(submission, .(fn)(.(var)),
#             passif(fn == quote(cos) && var == quote(x), "Right. The function is {{fn}} on variable {{var}}."))

