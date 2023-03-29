#include <Rcpp.h>
using namespace Rcpp;

//'  Find column with same non-positive values
//'
//' @param invmatrix Inverse of the hypothesis matrix
//'
// [[Rcpp::export]]
int find_same_col(NumericMatrix invmatrix) {
  int nrow = invmatrix.nrow();
  int ncol = invmatrix.ncol();

  // Remove the first row
  NumericMatrix B = invmatrix(Range(1, nrow - 1), _);

  for (int j = 0; j < ncol; j++) {
    NumericVector col = B(_, j);

    if ((std::abs(min(col) - max(col)) < 1e-15) &&
        std::all_of(col.begin(), col.end(), [](double x){return x <= 0;})) {
      return j + 1;
    }
  }

  return NA_INTEGER;
}
