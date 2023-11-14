library(Rcpp)
sourceCpp("src/fit_slr.cpp")
fit_slr <- function(x, y) {
  fit_slr_cpp(x, y)
}
