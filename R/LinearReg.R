library(Rcpp)
sourceCpp("src/fit_slr.cpp")
fit_slr <- function(y, x) {
  fit_slr(y, x)
}
sourceCpp("src/fit_mlr.cpp")
fit_mlr <- function(x, y) {
  fit_mlr_cpp(x, y)
}

