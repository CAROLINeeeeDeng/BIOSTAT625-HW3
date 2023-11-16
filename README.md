# BIOSTAT625-HW3
## LinearReg - R package for linear regression, residual calculation, and Anova

### Description
The LinearReg Package can fit SLR, MLR, calculate the residuals, and do Anova test with detailed values

### Function
- fit_simple_model(): perform simple linear regression (with one response variable and ONLY one independent variable) using OLS, return the estimator coefficients
- fit_multi_model(): perform multiple linear regression (with one response variable and MORE than one independent variable) using OLS, both full model and null model, return the estimator coefficients
- residual(): calculate the residuals returned as a vector after fitting the correct linear model
- anova_test(): calculate SSR, SSE, corresponding degree of freedom, MSR, MSE, F-statistics, P-value, and generate a anova table

### Based on Package
roxygen2, devtools, Rcpp, testthat
