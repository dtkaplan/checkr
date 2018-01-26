context("pre-check")

test_that("pre-check handles both text and expressions", {
  code1 <- "lm(mpg ~ hp, data <- mtcars); plot(1:10); x <- 1\n y <- x^2\n\n z = cos(yy * xx^2)"
  code2 <- quote({lm(mpg ~ hp, data <- mtcars); plot(1:10); x <- 1; y <- x^2; z = cos(yy * xx^2)})
  pat <- "\\'yy\\' is not the name"
  expect_true(grepl(pat, pre_check(code1)$message))
  expect_true(grepl(pat, pre_check(code1)$message))
})
