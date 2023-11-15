library(testthat)
test_that("fit_mlr works", {
  #Test Case 1:
  y = rnorm(10)
  x1 = rnorm(10)
  x2 = rnorm(10)
  X_matrix <- cbind(x1, x2)
  expect_equal(fit_multi_model(y, X_matrix) , lm(y~x1+x2)$coefficients)
  #Test Case 2:
  y = rnorm(100)
  A = rnorm(100)
  B = rnorm(100)
  C = rnorm(100)
  X_matrix <- cbind(A, B, C)
  expect_equal(fit_multi_model(y, X_matrix) , lm(y~A+B+C)$coefficients)
  #Test Case 3:
  y = rnorm(100)
  x1 = rnorm(99)
  x2 = rnorm(99)
  x3 = rnorm(99)
  X_matrix <- cbind(x1, x2, x3)
  expect_equal(fit_multi_model(y, X_matrix) , NULL)
})
