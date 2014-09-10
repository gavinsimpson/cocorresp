\name{scaleChi}
\alias{scaleChi}

\title{Standardised chi-square residuals }
\description{
  Scales a matrix, Y, to is standardised chi-square residuals \eqn{(o -
    e) / \sqrt{e}}{(o - e) / sqrt(e)} (given \eqn{K_n} and \eqn{R_0}
  metrics derived from an external matrix \eqn{Y_0}) so that further
  analysis can be unweighted.
}
\usage{
scaleChi(Y, Kn, R0, eps = 1e-06)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{ a matrix for which standardised chi-square residuals are to
    be calculated.}
  \item{Kn}{ the column sums (K) of Y divided by sum(K).}
  \item{R0}{ row weights.}
  \item{eps}{ a tolerance.}
}
%\details{}
\value{
  \item{Yr }{the matrix of standardised chi-squared residuals of Y.}
}

\references{
  
  Ter Braak, C.J.F and Schaffers, A.P. (2004) Co-Correspondence
  Analysis: a new ordination method to relate two community
  compositions. \emph{Ecology} \bold{85(3)}, 834--846

}

\author{Gavin L. Simpson, based on Matlab code by C.J.F. ter Braak and
  A.P. Schaffers.}

\note{
  This function is not intended for casual use by users.
}

\keyword{multivariate}
