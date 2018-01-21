context("combining")

CODE <- for_checkr(quote({x <- 2; y <- x^2; z <- x + y}))

test_that("Can combine results from tests.", {
  line1 <- line_where(CODE,
                      passif(Z == "z", "Found assignment to `z`."))
  line2 <- line_where(CODE,
                      insist(Z == "z", "No assignment to `z` found."),
                      insist(V == 4, "Should have been 6.")) # fail
  line3 <- line_where(CODE,
                      insist(Z == "z", "No assignment to `z` found."),
                      noteif(V == 6, "Should have been 6."), message = "z should be 4")
  expect_true(passed(line1 %or% line2))
  expect_false(passed(line1 %and% line2))
  # line1 and line3 are "ok", so combining them with %and% produces an ok.
  expect_false(failed(line1 %and% line3))
  expect_false(passed(line1 %and% line3))
})
