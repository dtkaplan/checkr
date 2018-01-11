context("High-level testing to expand coverage")

source(file = system.file("learnr_examples/internal-examples.R",
                          package = "checkr"),
       local = TRUE)

test_that("rep(1:4, each = 3) problem works", {

  # For testing ...
  ex1 <- quote(c(1,1,1,2,2,2,3,3,3,4,4,4))
  ex1a <- quote(Id <- c(1,1,1,2,2,2,3,3,3,4,4,4))
  ex2 <- "Id <- rep(1:3, each = 3)"
  ex2a <- "Id <- rep(1:3, each = 4)"
  ex2b <- "Id <- rep(3, each = 4)"
  ex3 <- "Id <- rep(1:4, each = 3)"

  r1 <- rep_1234(ex1)
  expect_true(failed(r1))
  expect_equal(r1$message, "Remember to store the result under the name `Id`.")
  r2 <- rep_1234(ex1a)
  expect_true(failed(r2))
  expect_equal(r2$message, "You're supposed to use `rep()`.")
  r3 <- rep_1234(ex2)
  expect_true(failed(r3))
  expect_true(grepl("The elements to be repeated", r3$message))
  r4 <- rep_1234(ex2a)
  expect_true(failed(r4))
  expect_true(grepl("Good use of", r4$message))
  r5 <- rep_1234(ex3)
  expect_true(ok(r5))
})
