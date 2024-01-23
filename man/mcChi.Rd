\name{mcChi}
\alias{mcChi}
\title{ Standardised chi-square residuals }
\description{
  Scales a matrix, Y, to its standardised chi-square residuals \eqn{(o -
    e) / \sqrt{e}}{(o - e) / sqrt(e)} (if \eqn{R_0 = R}, where R contains the row
  sums of matrix Y) so that further analysis can be unweighted
}
\usage{
mcChi(Y, R0, eps = 1e-06)
}
\arguments{
  \item{Y}{ a matrix for which standardised chi-square residuals are to
    be calculated.}
  \item{R0}{ row weights.}
  \item{eps}{ tolerance - leave as default.}
}
\details{
  This function implements equation 8 of ter Braak and Schaffers (2004)
  by firstly applying equation 7 to form matrix Q using row and column
  sums of Y as weights, and, secondly, by applying equation 8 to form a
  matrix of standardised chi-square residuals from Q by
  pre-multiplication of Q by \eqn{\sqrt{R_0}}{sqrt(R0)} and
  post-multiplication of Q by \eqn{\sqrt{K}}{sqrt(K)}, where K is the
  column sums of Y.
}
\value{
  A list with the following components:
  \item{Ychi }{the matrix of standardised chi-squared residuals of Y}
  \item{Kn }{the column sums (K) of Y divided by sum(K)}
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

% \examples{
% data(beetles)
% ## log transform the bettle data
% beetles <- as.matrix(log(beetles + 1))
% ## row sums for R0
% rsum <- rowSums(beetles)
% ## calculate chi-square residuals
% res <- mcChi(beetles, rsum)
% res$Ychi
%}
\keyword{multivariate}
