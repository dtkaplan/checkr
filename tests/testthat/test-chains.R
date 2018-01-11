context("chains")

test_that("Can identify an expression as a chain ", {
  expect_true(checkr:::is_chain(quo(3 %>% sqrt)))
  expect_false(checkr:::is_chain(quo(7)))
  expect_false(checkr:::is_chain(quo(sin(7))))
  expect_false(checkr:::is_chain(quo(mtcars)))
})

test_that("Can find a chain in a sequence of lines", {
  CODE <- for_checkr(quote({data(mtcars, package = "datasets"); mtcars %>% lm(mpg ~ cyl, data = .)}))
  r1 <- line_chaining(CODE)
  expect_false(failed(r1))
  expect_true(length(r1$code) == 1)
  expect_equal(rlang::quo_expr(r1$code[[1]]), quote(mtcars %>% lm(mpg ~ cyl, data = .)))
  r2 <- line_chaining(CODE, n = 2)
  expect_true(failed(r2))
  expect_true(length(r2$code) == 2) # for this example
})

test_that("Can expand single chains.", {
  CODE <- for_checkr(quote({data(mtcars, package = "datasets");
    mtcars %>% lm(mpg ~ cyl, data = .) %>% summary(.)}))
  r1 <- line_chaining(CODE)
  r1 <- expand_chain(r1)
  expect_equal(length(r1$code), 3)
  expect_equal(quo_expr(r1$code[[1]]), quote(mtcars))
  expect_equal(quo_expr(r1$code[[3]]), quote(summary(.)))
})

test_that("Can expand the chains in a sequence of lines", {
  CODE <- for_checkr(quote({data(mtcars, package = "datasets");
    mtcars %>% lm(mpg ~ cyl, data = .) %>% summary(.)}))
  r1 <- expand_all_chains(CODE)
  expect_equal(length(r1$code), 4)
  expect_equal(quo_expr(r1$code[[2]]), quote(mtcars))
  expect_equal(quo_expr(r1$code[[4]]), quote(summary(.)))
})

test_that("Can handle chains with just 1 link", {
  CODE <- for_checkr(quote({data(mtcars, package = "datasets");
    mtcars %>% lm(mpg ~ cyl, data = .) }))
  r1 <- line_chaining(CODE)
  r2 <- expand_chain(r1)
  r3 <- line_calling(r2, lm)
  r4 <- data_arg(r3, failif( !identical(V, mtcars), "Wasn't mtcars") )
  expect_false(failed(r4))
  expect_equal(eval_tidy(r4$code[[1]]), mtcars)
})

test_that("Chain expansion works even when functions don't have . as an explicit argument.", {
  code <- for_checkr(quote({x <- 3 %>% sin( ) %>% cos(); x %>% sqrt() %>% log()}))
  lineA <- line_chaining(code)
  r1 <- expand_chain(lineA)
  expect_equal(length(r1$code), 3)
  r2 <- expand_all_chains(code)
  expect_equal(length(r2$code), 6)
})
