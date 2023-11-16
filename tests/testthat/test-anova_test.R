library(testthat)
test_that("fit_mlr works", {
  #Test Case 1:
  y = rnorm(10)
  x = rnorm(10)
  X_matrix <- cbind(x)
  expect_equal(round(anova_test(y, X_matrix)$`Sum Sq`, 4), round(anova(lm(y~ X_matrix))$`Sum Sq`, 4))
  expect_equal(round(anova_test(y, X_matrix)$Df, 4), round(anova(lm(y~ X_matrix))$Df, 4))
  expect_equal(round(anova_test(y, X_matrix)$`Mean Sq`, 4), round(anova(lm(y~ X_matrix))$`Mean Sq`, 4))
  expect_equal(round(anova_test(y, X_matrix)$`F value`, 4), round(anova(lm(y~ X_matrix))$`F value`, 4))
  expect_equal(round(anova_test(y, X_matrix)$`Pr(>F)`, 4), round(anova(lm(y~ X_matrix))$`Pr(>F)`, 4))

  #Test Case 2:
  y = rnorm(20)
  x1 = rnorm(20)
  x2 = rnorm(20)
  C = rnorm(20)
  X_matrix <- cbind(x1, x2, C)
  expect_equal(round(anova_test(y, X_matrix)$`Sum Sq`, 4), round(anova(lm(y~ X_matrix))$`Sum Sq`, 4))
  expect_equal(round(anova_test(y, X_matrix)$Df, 4), round(anova(lm(y~ X_matrix))$Df, 4))
  expect_equal(round(anova_test(y, X_matrix)$`Mean Sq`, 4), round(anova(lm(y~ X_matrix))$`Mean Sq`, 4))
  expect_equal(round(anova_test(y, X_matrix)$`F value`, 4), round(anova(lm(y~ X_matrix))$`F value`, 4))
  expect_equal(round(anova_test(y, X_matrix)$`Pr(>F)`, 4), round(anova(lm(y~ X_matrix))$`Pr(>F)`, 4))

  #Test Case 3:
  y = rnorm(100)
  x1 = rnorm(99)
  x2 = rnorm(99)
  x3 = rnorm(99)
  X_matrix <- cbind(x1, x2, x3)
  expect_error(anova_test(y, X_matrix), "Lengths of 'y' and 'x' must be the same.")
})
