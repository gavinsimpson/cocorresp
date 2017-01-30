\name{cocorresp-package}
\alias{cocorresp-package}
\alias{cocorresp}
\docType{package}
\title{
Co-correspondence analysis ordination methods for community ecology
}
\description{
  Fits predictive and symmetric co-correspondence analysis (CoCA) models
  to relate one data matrix to another data matrix. More specifically,
  CoCA maximises the weighted covariance between the weighted averaged
  species scores of one community and the weighted averaged species
  scores of another community. CoCA attempts to find patterns that are
  common to both communitities.
}
\details{
  The main interface function is \code{\link{coca}} which accepts a
  formula or two community data matrices. An appropriate formula is
  \code{Y ~ ., data = X} and the associated \code{data} object from
  which \code{.} will be looked up. The \code{method} argument is used
  to select from the two forms of CoCA: \code{method = "predictive"} for
  predictive CoCA (the default), and \code{method = "symmetric"} for
  symmetric CoCA.
}
\author{
  Original Matlab routines by C.J.F. ter Braak and A.P. Schaffers. R port
  by Gavin L. Simpson. Function simpls based on \code{simpls.fit} (package
  pls) by Ron Wehrens and Bjorn-Helge Mevik.
  
  Maintainer: Gavin L. Simpson <ucfagls@gmail.com>
}
\keyword{ package }
