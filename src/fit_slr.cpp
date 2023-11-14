// [[Rcpp::depends(Rcpp)]]
#include <Rcpp.h>
// [[Rcpp::export]]
Rcpp::NumericVector fit_slr(Rcpp::NumericVector y, Rcpp::NumericVector x) {
  if (x.size() == y.size()) {
    double mean_x = Rcpp::mean(x);
    double mean_y = Rcpp::mean(y);
    double SSXY = Rcpp::sum((x - mean_x)*(y - mean_y));
    double SSX = Rcpp::sum((x - mean_x)*(x - mean_x));
    double b1 = SSXY/SSX;
    double b0 = mean_y - (b1 * mean_x);
    return Rcpp::NumericVector{b0, b1};
  }
  else {
    std::cout<<"length are not the same"<<std::endl;
    return 0;
  }
}
