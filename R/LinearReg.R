library(Rcpp)
sourceCpp("src/fit_slr.cpp")
sourceCpp("src/fit_mlr.cpp")
sourceCpp("src/fit_mlr_null.cpp")


#' fit_simple_model
#'
#' @param y a vector of values as response variable
#' @param x a vector of values as predict variable
#' @return The two estimator coefficients (intercept beta0 and slope beta1)
#' @examples
#' y = rnorm(100)
#' x = rnorm(100)
#' fit_simple_model(y, x)
#' @export
fit_simple_model <- function(y, x) {
  if (length(y) != length(x)) {
    stop("Lengths of 'y' and 'x' must be the same.")
  }
  return(c("(Intercept)" = fit_slr(y, x)[1], x = fit_slr(y, x)[2]))
}


#' fit_multi_model
#'
#' @param y a vector of values as response variable
#' @param x_matrix a matrix of values as predict variable(s)
#' @param null whether this is a null model or not, default as FALSE
#' @return The vector of all the estimator coefficients
#' @examples
#' y = rnorm(100)
#' A = rnorm(100)
#' B = rnorm(100)
#' fit_multi_model(y, x_matrix, null = FALSE)
#' fit_multi_model(y, x_matrix, null = TRUE)
#' @export
fit_multi_model <- function(y, x_matrix, null = F) {
  if (length(y) != nrow(x_matrix)) {
    stop("Lengths of 'y' and 'x' must be the same.")
  }
  if (null) {
    coefficients = fit_mlr_null(y, x_matrix)
    names(coefficients) = c(colnames(x_matrix))
    return(coefficients)
  }
  coefficients = fit_mlr(y, x_matrix)
  names(coefficients) = c("(Intercept)", colnames(x_matrix))
  return(coefficients)
}


#' residual
#'
#' @param y a vector of values as response variable
#' @param x_matrix a matrix of values as predict variable(s)
#' @param null whether this is a null model or not, default as FALSE
#' @return The vector of the corresponding residuals values
#' @examples
#' y = rnorm(100)
#' A = rnorm(100)
#' B = rnorm(100)
#' residual(y, x_matrix, null = FALSE)
#' residual(y, x_matrix, null = TRUE)
#' @export
residual <- function(y, x_matrix, null = F) {
  if (length(y) != nrow(x_matrix)) {
    stop("Lengths of 'y' and 'x' must be the same.")
  }
  if (null) {
    mod = fit_mlr_null(y, x_matrix)
    y_pred = x_matrix %*% mod
    residuals = y - y_pred
  }
  else{
    mod = fit_mlr(y, x_matrix)
    y_pred = cbind(1, x_matrix) %*% mod
    residuals = y - y_pred
  }
  residuals_vector = as.vector(residuals)
  names(residuals_vector) = 1:length(residuals_vector)
  return(residuals_vector)
}


#' anova_test
#'
#' @param y a vector of values as response variable
#' @param x_matrix a matrix of values as predict variable(s)
#' @return Anova table as a dataframe
#' #' \itemize{
#' \item degree of freedom for model, degree of freedom for error
#' \item SSE, SSR
#' \item MSE, MSR
#' \item F-statistics
#' \item p-value
#' }
#' @examples
#' y = rnorm(100)
#' A = rnorm(100)
#' B = rnorm(100)
#' anova_test(y, x_matrix)
#' @export
anova_test <- function(y, x_matrix) {
  full_model = fit_multi_model(y, x_matrix, F)
  X_full = cbind(1, x_matrix)
  fitted_full = X_full %*% full_model
  residuals_full = residual(y, x_matrix, F)
  ssr = sum((fitted_full - mean(y)) ^ 2)
  sse = sum(residuals_full ^ 2)
  n = length(y)
  p = ncol(x_matrix) + 1
  df_model = p - 1
  df_error = n - p
  msr = ssr / df_model
  mse = sse / df_error
  f_statistic = msr / mse
  p_value = pf(f_statistic, df1 = df_model, df2 = df_error, lower.tail = FALSE)
  anova_table = data.frame(
    Model = c("Model", "Residuals"),
    DF = round(c(df_model, df_error), 4),
    SumOfSquares = round(c(ssr, sse), 4),
    MeanSquare = round(c(msr, mse), 4),
    F = c(round(f_statistic, 4), NA),
    PValue = c(round(p_value, 4), NA)
  )
  rownames(anova_table) = NULL
  colnames(anova_table) = c("", "Df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)")
  return(anova_table)
}
