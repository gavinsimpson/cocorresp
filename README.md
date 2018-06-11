## Co-correspondence analysis with R

**cocorresp** fits symmetric and predictive co-correspondence (CoCA) models in R.

#### Released version
[![CRAN version](https://www.r-pkg.org/badges/version/cocorresp)](https://cran.r-project.org/package=cocorresp) [![](https://cranlogs.r-pkg.org/badges/grand-total/cocorresp)](https://cran.r-project.org/package=cocorresp)

#### Build status
[![Build Status](https://travis-ci.org/gavinsimpson/cocorresp.svg?branch=master)](https://travis-ci.org/gavinsimpson/cocorresp)  [![Build status](https://ci.appveyor.com/api/projects/status/u1e24ck7a61eonxr/branch/master?svg=true)](https://ci.appveyor.com/project/gavinsimpson/cocorresp/branch/master)  [![codecov.io](https://codecov.io/github/gavinsimpson/cocorresp/coverage.svg?branch=master)](https://codecov.io/github/gavinsimpson/cocorresp?branch=master)

## Summary

Fits predictive and symmetric co-correspondence analysis (CoCA) models to relate one data matrix to another data matrix. More specifically, CoCA maximises the weighted covariance between the weighted averaged species scores of one community and the weighted averaged species scores of another community. CoCA attempts to find patterns that are common to both communitities.

The main interface function is `coca` which accepts a 
formula or two community data matrices. An appropriate formula is `Y ~ ., data = X` and the associated `data` object from which `.` will be looked up. The `method` argument is used to select from the two forms of CoCA:

1. `method = "predictive"` for predictive CoCA (the default), and
1. `method = "symmetric"` for symmetric CoCA.

The cocorresp package is based on original Matlab routines by C.J.F. ter Braak and A.P. Schaffers. The R port was by Gavin L. Simpson. Function `cocorresp::simpls()` is largely based on `simpls.fit()` from the **pls** package of Ron Wehrens and Bjorn-Helge Mevik.

## Installtion

**cocorresp** is available from CRAN; install the latest release using

```r
install.packages("cocorresp")
```

To install the development version, use the **devtools** package (you may need to install **devtools** first)

```r
devtools::install_github("gavinsimpson/cocorresp")
```
