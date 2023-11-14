test_that("fit_slr works", {
  x = rnorm(10)
  y = rnorm(10)
  expect_equal(fit_simple_model(y, x), lm(y~x)$coefficients)
})
