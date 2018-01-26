context("comparisons")

test_that("same_name works", {
  expect_true(same_name(quote(x), "x"))
  expect_true(same_name(quote(x), quote(x)))
  expect_true(same_name("x", quote(x)))
  expect_true(same_name("x", "x"))
  expect_false(same_name(quote(xx), "x"))
  expect_error(same_name(7, quote(x)))
  expect_error(same_name(quote(x), 7))
})

