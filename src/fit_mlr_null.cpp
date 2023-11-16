// [[Rcpp::depends(Rcpp)]]
#include <Rcpp.h>
#include <vector>
#include <iostream>
//#include "fit_mlr_null.h"

std::vector<std::vector<double>> inverseMatrixNull(const std::vector<std::vector<double>>& M) {
  int n = M.size();
  std::vector<std::vector<double>> matrix(n, std::vector<double>(2*n, 0.0));
  for (int i = 0; i < n; ++i) {
    matrix[i][i + n] = 1;
    for (int j = 0; j < n; ++j) {
      matrix[i][j] = M[i][j];
    }
  }
  for (int i = 0; i < n; ++i) {
    double pivot = matrix[i][i];
    for (int j = 0; j < 2 * n; ++j) {
      matrix[i][j] /= pivot;
    }

    for (int k = 0; k < n; ++k) {
      if (k != i) {
        double factor = matrix[k][i];
        for (int j = 0; j < 2 * n; ++j) {
          matrix[k][j] -= factor * matrix[i][j];
        }
      }
    }
  }
  std::vector<std::vector<double>> inverse(n, std::vector<double>(n, 0.0));
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < n; ++j) {
      inverse[i][j] = matrix[i][j + n];
    }
  }
  return inverse;
}

// [[Rcpp::export]]
Rcpp::NumericVector fit_mlr_null(Rcpp::NumericVector y, Rcpp::NumericMatrix x) {
  if (x.nrow() == y.size()) {
    Rcpp::NumericMatrix X(x.nrow(), x.ncol());
    for (size_t i = 0; i < x.nrow(); ++i) {
      for (int j = 0; j < x.ncol(); ++j) {
        X(i, j) = x(i, j);
      }
    }

    std::vector<std::vector<double>> XTX(x.ncol(), std::vector<double>(x.ncol(), 0.0));
    for (int i = 0; i < x.ncol(); ++i) {
      for (int j = 0; j < x.ncol(); ++j) {
        for (int k = 0; k < x.nrow(); ++k) {
          XTX[i][j] += X(k, i) * X(k, j);
        }
      }
    }

    std::vector<double> XTY(x.ncol(), 0.0);
    for (int i = 0; i < x.ncol(); ++i) {
      for (int j = 0; j < x.nrow(); ++j) {
        XTY[i] += X(j, i) * y(j);
      }
    }

    std::vector<std::vector<double>> inverse_XTX = inverseMatrixNull(XTX);
    std::vector<double> estimators(x.ncol(), 0.0);
    for (int i = 0; i < x.ncol(); ++i) {
      for (int j = 0; j < x.ncol(); ++j) {
        estimators[i] += inverse_XTX[i][j] * XTY[j];
      }
    }

    Rcpp::NumericVector estimators_list(estimators.begin(), estimators.end());
    return estimators_list;
  } else {
    std::cout << "lengths are not the same" << std::endl;
    return NULL;
  }
}
