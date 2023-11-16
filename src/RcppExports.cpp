// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// fit_mlr
Rcpp::NumericVector fit_mlr(Rcpp::NumericVector y, Rcpp::NumericMatrix x);
RcppExport SEXP _LinearReg_fit_mlr(SEXP ySEXP, SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(fit_mlr(y, x));
    return rcpp_result_gen;
END_RCPP
}
// fit_slr
Rcpp::NumericVector fit_slr(Rcpp::NumericVector y, Rcpp::NumericVector x);
RcppExport SEXP _LinearReg_fit_slr(SEXP ySEXP, SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(fit_slr(y, x));
    return rcpp_result_gen;
END_RCPP
}
// predict_slr_cpp
Rcpp::NumericVector predict_slr_cpp(const Rcpp::NumericVector& predict_var, const Rcpp::NumericVector& estimators);
RcppExport SEXP _LinearReg_predict_slr_cpp(SEXP predict_varSEXP, SEXP estimatorsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type predict_var(predict_varSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type estimators(estimatorsSEXP);
    rcpp_result_gen = Rcpp::wrap(predict_slr_cpp(predict_var, estimators));
    return rcpp_result_gen;
END_RCPP
}
// predict_slr_cpp
Rcpp::NumericVector predict_slr_cpp(const Rcpp::NumericVector& predict_var, const Rcpp::NumericVector& estimators);
RcppExport SEXP _LinearReg_predict_slr_cpp(SEXP predict_varSEXP, SEXP estimatorsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type predict_var(predict_varSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type estimators(estimatorsSEXP);
    rcpp_result_gen = Rcpp::wrap(predict_slr_cpp(predict_var, estimators));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_LinearReg_fit_mlr", (DL_FUNC) &_LinearReg_fit_mlr, 2},
    {"_LinearReg_fit_slr", (DL_FUNC) &_LinearReg_fit_slr, 2},
    {"_LinearReg_predict_slr_cpp", (DL_FUNC) &_LinearReg_predict_slr_cpp, 2},
    {"_LinearReg_predict_slr_cpp", (DL_FUNC) &_LinearReg_predict_slr_cpp, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_LinearReg(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}