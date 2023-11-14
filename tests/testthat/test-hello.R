test_that("fit_mlr works", {
  y = rnorm(10)
  x1 = rnorm(10)
  x2 = rnorm(10)
  X_matrix <- cbind(x1, x2)
  expect_equal(fit_multi_model(y, X_matrix) , lm(y~x)$coefficients)
})
