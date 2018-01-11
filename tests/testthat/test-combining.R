context("combining")

CODE <- for_checkr(quote({x <- 2; y <- x^2; z <- x + y}))

test_that("Can combine results from tests.", {
  line1 <- line_where(CODE, Z == "z")
  line2 <- line_where(CODE, Z == "z", V == 4) # fail
  line3 <- line_where(CODE, Z == "z", V == 4, message = "z should be 4")
  expect_true(passed(line1 %or% line2))
  expect_false(passed(line1 %and% line2))
  expect_false(passed(line1 %and% line3))
})
