#include <Rcpp.h>
Rcpp::NumericVector fit_slr(Rcpp::NumericVector y, Rcpp::NumericVector x) {
  if (x.size() == y.size) {
    double mean_x = Rcpp::mean(x);
    double mean_y = Rcpp::mean(y);
    double SSXY = Rcpp::sum((x - mean_x)*(y - mean_y));
    double SSX = Rcpp::sum((x - mean_x)*(x - mean_x));
    double b1 = SSXY/SSX;
    double b0 = mean_y - (estimated_beta1 * mean_x);
    return Rcpp::NumericVector::create(Rcpp::Named("intercept") = b0, Rcpp::Named("slope") = b1);
  }
  else {
    printf("length are not the same");
  }
}


