#include <Rcpp.h>
// [[Rcpp::export]]
Rcpp::NumericVector predict_slr_cpp(const Rcpp::NumericVector& predict_var, const Rcpp::NumericVector& estimators) {
  Rcpp::NumericVector predicted_values = estimators[0] + estimators[1] * predict_var;
  return predicted_values;
}
