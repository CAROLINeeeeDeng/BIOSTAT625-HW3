#include <Rcpp.h>
#include <vector>
// [[Rcpp::export]]
Rcpp::NumericVector fit_mlr_cpp(Rcpp::NumericVector y, Rcpp::NumericMatrix x) {
  if ((x[0].size() == y.size()) {
    Rcpp::NumericMatrix X(x.nrow(), x.ncol() + 1);
    for (size_t i=0; i<x.nrow(); ++i) {
      X(i, 0) = 1;
      for (int j=1; j<x.ncol()+1; ++j){
        X(i, j) = x(i, j-1);
      }
    }
    Rcpp::NumericMatrix XTX = Rcpp::tranpose(X) * X;
    Rcpp::NumericVector XTY = Rcpp::tranpose(X) * y;
    Rcpp::NumericVector estimators = Rcpp::solve(XTX, XTY);
    return estimators;
  }
  else {
    std::cout<<"length are not the same"<<std::endl;
}
