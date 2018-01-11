## ----setup, include = FALSE----------------------------------------------
library(checkr)
library(ggplot2)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

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

