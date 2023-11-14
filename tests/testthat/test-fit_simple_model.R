test_that("fit_slr works", {
  #Test Case 1:
  x = rnorm(20)
  y = rnorm(20)
  expect_equal(fit_simple_model(y, x), lm(y~x)$coefficients)
  #Test Case 2:
  x = rnorm(100)
  y = rnorm(100)
  expect_equal(fit_simple_model(y, x), lm(y~x)$coefficients)
  #Test Case 3 (xy length not equal):
  x = rnorm(10)
  y = rnorm(9)
  expect_equal(fit_simple_model(y, x), NULL)
})
