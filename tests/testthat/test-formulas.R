context("test formulas")

CODE <- for_checkr(
  quote({ data(mtcars, package = "datasets")
          mod <- lm(mpg + carb ~ hp*cyl, data = mtcars)
        }))

test_that("testing for equality works", {
  the_formula <- mpg + carb ~ hp * cyl
  expect_true(formula_equals(the_formula, mpg + carb ~ hp * cyl))
  expect_false(formula_equals(the_formula, mpg ~ cyl * hp))
  expect_true(lhs_equals(the_formula, mpg + carb))
  expect_false(lhs_equals(the_formula, mpg))
  expect_true(rhs_equals(the_formula, hp * cyl))
  expect_false(rhs_equals(the_formula, cyl * hp))
})

