context("Grabbing arguments ")

CODE <- for_checkr(quote({x <- 2; y <- x^3; z <- y + I(x)}))

test_that("arg_calling() grabs the sub-expression associated with calling a function", {
  lineA <- line_calling(CODE, `+`)
  r1 <- arg_calling(lineA, I)
  expect_equal(r1$code[[1]], quo(I(x)))
  r2 <- arg_calling(lineA, `+`)
  expect_equal(r2$code[[1]], quo(z <- y + I(x)))
  r3 <- arg_calling(lineA, sin, cos, `+`)
  expect_equal(r3$code[[1]], quo(z <- y + I(x)))
  r4 <- arg_calling(lineA, sin, cos)
  expect_true(failed(r4))
})

test_that("line_calling() can handle multiple lines", {
  r5 <- line_calling(CODE, sin, I, cos)
  expect_equal(r5$code[[1]], quo(z <- y + I(x)))
})

CODE2 <- for_checkr(quote({data(mtcars, package = "datasets"); lm(mpg ~ hp, data = mtcars)}))

test_that("checks for different kinds of arguments work", {
  lineA <- line_calling(CODE2, lm)
  res1 <- formula_arg(lineA)
  expect_equal(res1$code[[1]], quo(mpg ~ hp))
  res2 <- data_arg(lineA)
  expect_equal(res2$code[[1]], quo(mtcars))
  res3 <- named_arg(lineA, "data")
  expect_true(ok(res3))
  res4 <- arg_number(line_calling(lineA, lm), 2)
  expect_equal(res4$code[[1]], quo(mtcars))

  lineB <- line_calling(CODE2, data)
  res5 <- data_arg(lineB, passif(identical(V, mtcars), "Yes, you were using mtcars."))
  expect_true(passed(res5))
  res6 <- data_arg(lineB, passif(identical(EX, quote(`mtcars`)), "You used mtcars by name!"))
  expect_true(passed(res6))
  res7 <- character_arg(lineB, passif(V == "datasets", "Right, it's in the datasets package."))
  expect_true(passed(res7))
})

test_that("argument functions bind to EX and V", {
  lineA <- line_calling(CODE2, lm)
  res1 <- data_arg(lineA,
                   failif(nrow(V) != 32, "Wrong number of rows in data argument."),
                   passif(EX == `mtcars`, "Right! Use mtcars by name.")
                   )
  expect_true(passed(res1))
  res2 <- formula_arg(lineA, passif(EX == quote(mpg ~ hp), "Right order in formula {{V}}."))
  expect_equal(res2$message, "Right order in formula mpg ~ hp.")
})

test_that("On failure, the returned code is that of the original input.", {
  lineA <- line_calling(CODE2, lm)
  res1 <- named_arg(lineA, "hello") # a failure
  expect_equal(res1$code, lineA$code)
  res2 <- arg_number(lineA, 4)
  expect_equal(res2$code, lineA$code)
  res3 <- matrix_arg(lineA) # other, similar functions should be the same
  expect_equal(res3$code, lineA$code)
})

