#include <Rcpp.h>

Rcpp::NumericVector fit_mlr_cpp(Rcpp::NumericVector y, Rcpp::NumericVector x1, Rcpp::NumericVector x2) {
  if ((x1.size() == x2.size()) && (x2.size() == y.size())) {
    Rcpp::NumericMatrix X(y.size(), 3);
    for (int i = 0; i < y.size(); ++i) {
      X(i, 0) = 1;
      X(i, 1) = x1[i];
      X(i, 2) = x2[i];
    }
    Rcpp::NumericMatrix XTX = Rcpp::tranpose(X) * X;
    Rcpp::NumericVector XTY = Rcpp::tranpose(X) * y;
    Rcpp::NumericVector coefficients = Rcpp::solve(XTX, XTY);
    return coefficients;
  }
  else {
    std::cout<<"length are not the same"<<std::endl;
}
