#include <Rcpp.h>
#include <vector>
// [[Rcpp::export]]
Rcpp::NumericVector fit_mlr_cpp(Rcpp::NumericVector y, Rcpp::NumericMatrix x) {
  if (x.nrow() == y.size()) {
    Rcpp::NumericMatrix X(x.nrow(), x.ncol() + 1);
    for (size_t i=0; i<x.nrow(); ++i) {
      X(i, 0) = 1;
      for (int j=1; j<x.ncol()+1; ++j) {
        X(i, j) = x(i, j-1);
      }
    }

    Rcpp::NumericMatrix XTX = Rcpp::transpose(X) * X;
    Rcpp::NumericVector XTY = Rcpp::transpose(X) * y;
    std::vector<double> estimators(x.ncol() + 1, 0);
    for (int i=0; i<x.ncol()+1; ++i) {
      for (int j=0; j<x.ncol()+1; ++j) {
        estimators[i] += XTX(i,j)*XTY(j);
      }
    }
    Rcpp::NumericVector estimators_list(estimators.begin(), estimators.end());
    return estimators_list;
  }
  else {
    std::cout<<"length are not the same"<<std::endl;
    return 0;
  }
}
