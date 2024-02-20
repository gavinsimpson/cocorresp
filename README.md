## Co-correspondence analysis with R

[![CRAN version](https://www.r-pkg.org/badges/version/cocorresp)](https://cran.r-project.org/package=cocorresp)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/cocorresp)](https://cran.r-project.org/package=cocorresp)
[![R-CMD-check](https://github.com/gavinsimpson/cocorresp/workflows/R-CMD-check/badge.svg)](https://github.com/gavinsimpson/cocorresp/actions)
[![codecov.io](https://app.codecov.io/github/gavinsimpson/cocorresp/coverage.svg?branch=master)](https://app.codecov.io/github/gavinsimpson/cocorresp?branch=master)

**cocorresp** fits symmetric and predictive co-correspondence analysis (CoCA) models in R. CoCA relates two community matrices together in the same way that CCA relates a community matrix and a matrix of environmental or other predictor variables.

## Summary

Fits predictive and symmetric co-correspondence analysis (CoCA) models to relate one data matrix to another data matrix. More specifically, CoCA maximises the weighted covariance between the weighted averaged species scores of one community and the weighted averaged species scores of another community. CoCA attempts to find patterns that are common to both communities.

The main interface function is `coca` which accepts a 
formula or two community data matrices. An appropriate formula is `Y ~ ., data = X` and the associated `data` object from which `.` will be looked up. The `method` argument is used to select from the two forms of CoCA:

1. `method = "predictive"` for predictive CoCA (the default), and
2. `method = "symmetric"` for symmetric CoCA.

**cocorresp** is based on original Matlab routines by C.J.F. ter Braak and A.P. Schaffers. The R port was by Gavin L. Simpson. Function `cocorresp::simpls()` is largely based on `simpls.fit()` from the **pls** package of Ron Wehrens and Bjorn-Helge Mevik.

## Installation

**cocorresp** is available from CRAN; install the latest release using

```r
install.packages("cocorresp")
```

To install the development version, use the **remotes** package (you may need to install **remotes** first)

```r
remotes::install_github("gavinsimpson/cocorresp")
```
