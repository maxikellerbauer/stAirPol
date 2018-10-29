#include <Rcpp.h>
using namespace Rcpp;

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar)

// For more on using Rcpp click the Help button on the editor toolbar

//' Hist CPP
//'
//' @export
//' @importFrom Rcpp evalCpp
//' @useDynLib spAirPol
//' @return bounded numeric vector
// [[Rcpp::export]]
NumericVector hist_exp_cpp(NumericVector y, double lambda, int lag) {
  int n = y.size();
  NumericVector out(n);
  for(int i = lag; i < n; ++i) {
    out[i] = 0;
    for (int j = 0; j < lag; ++j) {
      out[i] = out[i] + y[i-j] * exp(-lambda * j);
    }
  }
  return out;
}
