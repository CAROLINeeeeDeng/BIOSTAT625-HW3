library(Rcpp)

sourceCpp("src/fit_slr.cpp")
sourceCpp("src/fit_mlr.cpp")


fit_simple_model <- function(y, x) {
  if(length(y)!=length(x)) {return(NULL)}
  return(c("(Intercept)" = fit_slr(y, x)[1], x = fit_slr(y, x)[2]))
}


fit_multi_model <- function(y, x_matrix) {
  if(length(y)!=nrow(x_matrix)) {return(NULL)}
  fit_mlr_cpp(y, x_matrix)
  return(fit_mlr_cpp(y, x_matrix))
}
