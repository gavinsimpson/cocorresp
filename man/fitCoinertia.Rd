\name{fitCoinertia}
\alias{fitCoinertia}
\alias{print.fitCoinertia}
\alias{summary.fitCoinertia}
\alias{print.summary.fitCoinertia}

\title{Co-inertia analysis}
\description{
  Performs a co-inertia of the triplets \eqn{(Q_1,K_1,R_0)} and
  \eqn{(Q_2,K_2,R_0)}.
}
\usage{
fitCoinertia(X, Dp, Y, Dq, Dn, n.axes)

%\method{print}{coinertia}(x, axes = c(1:min(6, x$n.axes)),
%      digits = max(3, getOption("digits") - 3), \ldots)

\method{summary}{fitCoinertia}(object, \ldots)

%\method{print}{summary.coinertia}(x, digits = max(3,
%      getOption("digits") - 3), \ldots)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{ \eqn{Q_1}, matrix of expected abundances under row-column
    independence in the original \code{Y} species matrix when
    treated as a contingency table.}
  \item{Dp}{ \eqn{K_1}, species (column) weights for \code{X}.}
  \item{Y}{ \eqn{Q_2}, matrix of expected abundances under row-column
    independence in the original \code{X} species matrix when
    treated as a contingency table.}
  \item{Dq}{ \eqn{K_2}, species (column) weights for \code{Y}.}
  \item{Dn}{ site weights \eqn{R_0}.}
  \item{n.axes}{ number of axes to calculate the co-inertia analysis
    for.}
  \item{object}{an object of class \code{coinertia}.}
  \item{axes}{ the number of axes to display when printing.}
  %\item{digits}{ the number of significant digits to use in printing}
  \item{\ldots}{ arguments passed to other functions. Currently ignored.}
}
%\details{
%  ~~ If necessary, more details than the __description__  above ~~
%}
\value{
  A list with the following components:
  \item{U1 }{column weights of \code{X}.}
  \item{U2 }{column weights of \code{Y}.}
  \item{X1 }{rowscores of \code{X}.}
  \item{X2 }{rowscores of \code{Y}.}
  \item{lambda }{the Eigenvalues (squares of the singular values).}
  \item{n.axes }{number of axes requested.}
  \item{call }{the matched function call.}
}
\references{

  Ter Braak, C.J.F and Schaffers, A.P. (2004) Co-Correspondence
  Analysis: a new ordination method to relate two community
  compositions. \emph{Ecology} \bold{85(3)}, 834--846

  Doledec, S and Chessel, D. (1994) Co-inertia analysis: a method for
  studying species-environment relationships. \emph{Freshwater Biology}
  \bold{31}, 277--294
}
\author{ Original Matlab code by C.J.F. ter Braak and A.P. Schaffers. R
  port by Gavin L. Simpson.}

\note{ This function is not yet meant to be called directly by the
  user. If you wish to use it directly, see the function definition for
  \code{\link{symcoca}} which demonstrates how to prepare the
  relevant input matrices.

  Note that in this function, \code{X} corresponds to the input
  matrix \code{y} and \code{Y} corresponds to the
  input matrix \code{x} in \code{\link{symcoca}}.
  Confusing! This will be changed in a future release but for now the
  arguments follow those of the original Matlab code - perhaps a little
  too closely!
}

\seealso{ \code{\link{symcoca}} for the function that calls
  \code{fitCoinertia} and \code{\link{coinertiaI}} for co-inertia analysis
  using identity matrices for \eqn{K_1}, \eqn{K_2}, and \eqn{R_0}
}

\keyword{multivariate}
\keyword{models}
