// [[Rcpp::depends(Rcpp)]]
#include <Rcpp.h>
#include <vector>
#include <iostream>
#include "fit_mlr.h"

std::vector<std::vector<double>> inverseMatrix(const std::vector<std::vector<double>>& M) {
  int n = M.size();
  std::vector<std::vector<double>> matrix(n, std::vector<double>(2*n, 0.0));
  for (int i=0; i < n; ++i) {
    matrix[i][i+n] = 1;
    for (int j = 0; j < n; ++j) {
      matrix[i][j] = M[i][j];
    }
  }
  for (int i=0; i < n; ++i) {
    double pivot = matrix[i][i];
    for (int j=0; j < 2*n; ++j) {
      matrix[i][j] /= pivot;
    }

    for (int k=0; k < n; ++k) {
      if (k!=i) {
        double factor = matrix[k][i];
        for (int j=0; j < 2*n; ++j) {
          matrix[k][j] -= factor*matrix[i][j];
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
Rcpp::NumericVector fit_mlr(Rcpp::NumericVector y, Rcpp::NumericMatrix x) {
  if (x.nrow() == y.size()) {
    size_t var = x.ncol() + 1;
    Rcpp::NumericMatrix X(x.nrow(), var);
    for (size_t i=0; i<x.nrow(); ++i) {
      X(i, 0) = 1;
      for (int j=1; j<var; ++j) {
        X(i, j) = x(i, j-1);
      }
    }
    std::vector<std::vector<double>> XTX(var, std::vector<double>(var, 0.0));
    for (int i = 0; i < var; ++i) {
      for (int j = 0; j < var; ++j) {
        for (int k = 0; k < x.nrow(); ++k) {
          XTX[i][j] += X(k, i) * X(k, j);
        }
      }
    }
    std::vector<double> XTY(var, 0.0);
    for (int i = 0; i < var; ++i) {
      for (int j = 0; j < x.nrow(); ++j) {
        XTY[i] += X(j, i) * y(j);
      }
    }
    std::vector<std::vector<double>> inverse_XTX = inverseMatrix(XTX);
    std::vector<double> estimators(var, 0.0);
    for (int i=0; i<var; ++i) {
      for (int j=0; j<var; ++j) {
        estimators[i] += inverse_XTX[i][j]*XTY[j];
      }
    }
    Rcpp::NumericVector estimators_list(estimators.begin(), estimators.end());
    return estimators_list;
  }
  else {
    std::cout<<"length are not the same"<<std::endl;
    return NULL;
  }
}
