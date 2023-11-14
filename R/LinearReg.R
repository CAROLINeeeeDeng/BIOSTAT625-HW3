library(Rcpp)
sourceCpp("src/fit_slr.cpp")
fit_simple_model <- function(y, x) {
  return(c("(Intercept)" = fit_slr(y, x)[1], x = fit_slr(y, x)[2]))
}
sourceCpp("src/fit_mlr.cpp")
fit_multi_model <- function(x, y) {
  fit_mlr_cpp(x, y)
}



