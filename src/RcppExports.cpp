// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// hist_exp_cpp
NumericVector hist_exp_cpp(NumericVector y, double lambda, int lag);
RcppExport SEXP _stAirPol_hist_exp_cpp(SEXP ySEXP, SEXP lambdaSEXP, SEXP lagSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< double >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< int >::type lag(lagSEXP);
    rcpp_result_gen = Rcpp::wrap(hist_exp_cpp(y, lambda, lag));
    return rcpp_result_gen;
END_RCPP
}
// hist_exp_anti_cpp
NumericVector hist_exp_anti_cpp(NumericVector y, double lambda, int lag, double scale, double border);
RcppExport SEXP _stAirPol_hist_exp_anti_cpp(SEXP ySEXP, SEXP lambdaSEXP, SEXP lagSEXP, SEXP scaleSEXP, SEXP borderSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< double >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< int >::type lag(lagSEXP);
    Rcpp::traits::input_parameter< double >::type scale(scaleSEXP);
    Rcpp::traits::input_parameter< double >::type border(borderSEXP);
    rcpp_result_gen = Rcpp::wrap(hist_exp_anti_cpp(y, lambda, lag, scale, border));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_stAirPol_hist_exp_cpp", (DL_FUNC) &_stAirPol_hist_exp_cpp, 3},
    {"_stAirPol_hist_exp_anti_cpp", (DL_FUNC) &_stAirPol_hist_exp_anti_cpp, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_stAirPol(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
