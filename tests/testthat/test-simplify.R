context("simplify parens")

test_that("remove needless parens from code", {
  goo <- quo((((3 + 4))))
  quo_simplified <- quo(3+4)
  res <- checkr:::simplify_ex(goo)

  expect_equal(res, quo_simplified)
})
