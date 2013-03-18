#include <Rcpp.h>
#include <time.h>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector bin(NumericVector x, double width, double origin = 0) {
  int n = x.size();
  NumericVector times(n);

  for(int i = 0; i < n; ++i) {
    times[i] = (x[i] - origin) / width + 1;
  }

  return times;
}


/*** R 
x <- runif(1e6)

library(microbenchmark)

m <- microbenchmark(bin(x, 1/100))

one <- mean(m$time) / length(x) / 1e9
cycle_length <- 1 / 2.6e9

*/