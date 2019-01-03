#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector hist_exp_anti_cpp(NumericVector y, double lambda, int lag, double scale, double border) {
  int n = y.size();
  NumericVector out(n);
  for(int i = lag; i < n; ++i) {
    out[i] = 0;
    for (int j = 0; j < lag; ++j) {
      if(y[i-j] <= border) {
        out[i] = out[i] + (1 / (y[i-j]+scale)) * exp(-lambda * j);
      }
    }
  }
  return out;
}
