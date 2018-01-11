context("Line locator functions")

CODE <- for_checkr(quote({x <- 2; y <- x^3; z <- y + I(x)}))

test_that("line_where() identifies assignment", {
  res1 <- line_where(CODE, Z == "y")
  expect_equal(res1$code[[1]], quo(y <- x^3))
})

test_that("line_where() identifies a function", {
  res1 <- line_where(CODE, F == `^`)
  expect_equal(res1$code[[1]], quo(y <- x^3))
})

test_that("line_where() identifies a line value", {
  res1 <- line_where(CODE, V == 8)
  expect_equal(res1$code[[1]], quo(y <- x^3))
})

test_that("line_where() identifies an expression", {
  res1 <- line_where(CODE, EX == quo(x^3))
  expect_equal(res1$code[[1]], quo(y <- x^3))
})

test_that("line_calling() works", {
  code <- for_checkr("x <- 1; y <- x^2; z <- (y^2 + 7) / 2")
  r1 <- line_calling(code, `^`, message="Didn't find any line using exponentiation.")
  expect_equal(r1$code[[1]], quo(y <- x^2))
  r2 <- line_calling(code, `^`, n = 2L, message = "Didn't find a second line using exponentiation.")
  expect_equal(r2$code[[1]], quo(z <- (y^2 + 7) / 2))
  r3 <- line_calling(code, `-`, message = "No subtraction line called.")
  expect_true(failed(r3))
  expect_equal(r3$message, "No subtraction line called.")
})

test_that("line_where() and line_binding() return code in the form of a list of quosures.", {
  res1 <- line_where(CODE, EX == quo(x^3))
  expect_true(is.list(res1$code))
  expect_true(rlang::is_quosure(res1$code[[1]]))
  res2 <- line_binding(CODE, {`^`(...);..(v)}, passif(v == 8, "The line producing 8."))
  expect_true(is.list(res2$code))
  expect_true(rlang::is_quosure(res2$code[[1]]))
})

test_that("line_binding() patterns ignore assignment", {
  res1 <- line_binding(CODE, .(f)(...), passif(f == `^`))
  expect_equal(res1$message, "Good!")
  res2 <- line_binding(CODE, .(f)(.,.,.))
  expect_true(failed(res2))
})

test_that("line_binding() tests know about assignment", {
  res1 <- line_binding(CODE, `^`(...), passif(TRUE, "Assignment to {{Z}}."))
  expect_equal(res1$message, "Assignment to y.")
})


test_that("line_binding() passes notes into message", {
  res1 <- line_binding(CODE, `^`(..(a), ..(b)), noteif(TRUE, "A note"), failif(FALSE, "failed"), passif(TRUE, "passed") )
  expect_true(passed(res1))
  expect_true(grepl("A note", res1$message))
  res2 <- line_binding(CODE, `^`(..(a), ..(b)), noteif(TRUE, "A note"), failif(TRUE, "failed"), passif(TRUE, "passed") )
  expect_true(failed(res2))
  expect_true(grepl("A note", res2$message))
  res3 <- line_binding(CODE, `^`(..(a), ..(b)), noteif(TRUE, "A note"), failif(FALSE, "failed"), noteif(TRUE, "Another note."), passif(TRUE, "passed") )
  expect_true(passed(res3))
  expect_true(grepl("A note", res3$message) && grepl("Another note", res3$message))
})

test_that("line_binding() tests terminate on first definitive pass or fail", {
  res1 <- line_binding(CODE, `^`(..(a), ..(b)), failif(TRUE, "failed"), passif(TRUE, "passed"))
  expect_true(failed(res1))
  res2 <- line_binding(CODE, `^`(..(a), ..(b)), failif(FALSE, "failed"), passif(TRUE, "passed"))
  expect_true(passed(res2))
  res3 <- line_binding(CODE, `^`(..(a), ..(b)), noteif(TRUE, "A note"), failif(FALSE, "failed"), passif(TRUE, "passed") )
  expect_true(passed(res3))
  expect_true(grepl("A note", res3$message))
  res4 <- line_binding(CODE, `^`(..(a), ..(b)), failif(FALSE, "failed"), passif(TRUE, "passed"), noteif(TRUE, "A note") )
  expect_true(passed(res4))
  expect_false(grepl("A note", res4$message))
})

test_that("line_binding() returns a checkr_result with code", {
  res1 <- line_binding(CODE, `^`(...), passif(TRUE, "Assignment to {{Z}}."))
  expect_true(inherits(res1, "checkr_result"))
  expect_equal(res1$code[[1]], quo(y <- x^3))
})

test_that("line_calling() works", {
  res1 <- line_calling(CODE, `^`)
  expect_equal(res1$code[[1]], quo(y <- x^3))
  res2 <- line_calling(CODE, I)
  expect_equal(res2$code[[1]], quo(z <- y + I(x)))
  res3 <- line_calling(CODE, sin, I, tan)
  expect_equal(res2$code[[1]], quo(z <- y + I(x)))
  res4 <- line_calling(CODE, sin, tan)
  expect_true(failed(res4))
})

test_that("On failure, the returned code is that of the original input.", {
  res1 <- line_calling(CODE, `*`, message = "No multiplication found.")
  expect_true(length(res1$code) == 3) # all the input lines
  res2 <- line_where(CODE, V == 100, message = "No line producing value 100.")
  expect_true(length(res2$code) == 3)
  res3 <- line_binding(CODE, exp(...), message = "Exponential wasn't used.")
  expect_true(length(res3$code) == 3)
})

