# BIOSTAT625-HW3
## LinearReg - R package for linear regression, residual calculation, and Anova

### Description
The LinearReg Package can fit SLR, MLR, calculate the residuals, and do Anova test with detailed values


### Based on Package
roxygen2, devtools, Rcpp, testthat


### Functions and How to Use
#### · fit_simple_model(y, x): 
Perform simple linear regression (with one response variable and ONLY one independent variable) using OLS, return the estimator coefficients, using the fit_slr function generated by fit_slr.cpp
- input: 'y' as the response variable in vector; 'x' as the independent variable in vector
- return: the estimator coefficients: Intercept(Beta0) and Beta1

#### · fit_multi_model(y, x_matrix, null = F): 
Perform multiple linear regression (with one response variable and one or MORE independent variable) using OLS, both full model and null model, return the estimator coefficients, using the fit_mlr function generated by fit_mlr.cpp and fit_mlr_null function generated by fit_mlr_null.cpp
- input: 'y' as the response variable in vector; 'x_matrix' as the independent variables in matrix; null as a boolean value which stands for whether a full modle or a null model(no intercept) is wanted, default as False
- return: the estimator coefficients

#### · residual(y, x_matrix, null = F): 
Calculate the residuals returned as a vector after fitting the correct linear model
- input: 'y' as the response variable in vector; 'x_matrix' as the independent variables in matrix; null as a boolean value which stands for whether a full modle or a null model(no intercept) is wanted, default as False
- return: the residuals values in a vector

#### · anova_test(y, x_matrix): 
Calculate SSR, SSE, corresponding degree of freedom, MSR, MSE, F-statistics, P-value, and generate a anova table
- input: 'y' as the response variable in vector; 'x_matrix' as the independent variables in matrix
- return: the table of anova as a dataframe

  
