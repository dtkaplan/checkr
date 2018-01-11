context("check function applies tests to 1-code-line checkr_result")

CODE <- for_checkr(quote({data(mtcars, package = "datasets"); mod <- lm(mpg ~ hp, data = mtcars)}))

test_that("error if more than 1 code line is passed.", {
  expect_error(check(CODE, failif(TRUE)))
})

test_that("check() short circuits when a failed checkr_result is passed.", {
  lineA <- line_calling(CODE, `lm`)
  r1 <- check(lineA, failif(TRUE))
  expect_false(passed(check(r1, passif(TRUE))))
})

test_that("check() returns with the first definitive remark", {
  r1 <- check(line_calling(CODE, lm), failif(FALSE, "Shouldn't see this"), failif(TRUE, "Should see this"))
  expect_equal(r1$message, "Should see this")
  r2 <- check(line_calling(CODE, lm), failif(TRUE, "Shouldn't see this"), failif(TRUE, "Should see this"))
  expect_equal(r2$message, "Shouldn't see this")
})
