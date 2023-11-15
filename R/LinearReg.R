library(Rcpp)

sourceCpp("src/fit_slr.cpp")
sourceCpp("src/fit_mlr.cpp")
sourceCpp("src/fit_mlr_null.cpp")

fit_simple_model <- function(y, x) {
  if(length(y)!=length(x)) {
    stop("Lengths of 'y' and 'x' must be the same.")
  }
  return(c("(Intercept)" = fit_slr(y, x)[1], x = fit_slr(y, x)[2]))
}

fit_multi_model <- function(y, x_matrix, null = F) {
  if(length(y)!=nrow(x_matrix)) {
    stop("Lengths of 'y' and 'x' must be the same.")
  }
  if(null){
    coefficients = fit_mlr_null(y, x_matrix)
    names(coefficients) = c(colnames(x_matrix))
    return(coefficients)
  }
  coefficients = fit_mlr(y, x_matrix)
  names(coefficients) = c("(Intercept)", colnames(x_matrix))
  return(coefficients)
}
