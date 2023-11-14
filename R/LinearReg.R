library(Rcpp)
sourceCpp("src/fit_slr.cpp")
fit_simple_model <- function(y, x) {
  if(length(y)!=length(x)){return(NULL)}
  return(c("(Intercept)" = fit_slr(y, x)[1], x = fit_slr(y, x)[2]))
}
sourceCpp("src/fit_mlr.cpp")
fit_multi_model <- function(y, X) {
  if(length(y)!=nrow(X)){return(NULL)}
  fit_mlr_cpp(y, X)
  return(fit_mlr_cpp(y, X))
}
