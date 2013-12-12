\name{coinertia}
\alias{coinertia}
\alias{coinertia.default}
%\alias{print.coinertia}
%\alias{summary.coinertia}
%\alias{print.summary.coinertia}

\title{Co-inertia analysis}
\description{
  Performs a co-inertia of the triplets \eqn{(Q_1,K_1,R_0)} and
  \eqn{(Q_2,K_2,R_0)}.
}
\usage{
coinertia(y, ...)

\method{coinertia}{default}(y, x, n.axes = NULL, weights = NULL,
          symmetric = FALSE, ...)
}

\arguments{
  \item{y, x}{matrices or data frames of the two data sets for which
    axes of covariance are sought.}
  \item{n.axes}{numeric; the number of coinertia axes to retain.}
  \item{weights}{a vector of weights for the samples. If \code{NULL},
    weights are chosen from the row sums of \code{y} (\code{symmetric =
      FALSE}) or the average of the row sums of \code{y} and \code{x}
    (\code{symmetric = TRUE}).}
  \item{symmetric}{logical; should a symmetric set of weights be
    used. See Details.}
  \item{\ldots}{ arguments passed to other functions. Currently ignored.}
}
%\details{
%  ~~ If necessary, more details than the __description__  above ~~
%}
\value{
  An object of class \code{"coinertia"}, which is a list with the
  following components:
  
  \item{scores}{a list of ordination scores, with components
    \code{species} and \code{sites}, each of which is a list with
    components \code{Y} and \code{X} that refer to the scores for the
    input matrices \code{y} and \code{x} respectively.}
  \item{weights}{numeric vector of row weights used in the analysis.}
  \item{lambda}{numeric vector of Eigenvalues.}
  \item{n.axes}{numeric; the number of coinertia axes extracted}
  \item{symmetric}{logical; was a symmetric analysis performed?}
  \item{call}{the matched call.}
}
\references{

  Ter Braak, C.J.F and Schaffers, A.P. (2004) Co-Correspondence
  Analysis: a new ordination method to relate two community
  compositions. \emph{Ecology} \bold{85(3)}, 834--846

  Doledec, S and Chessel, D. (1994) Co-inertia analysis: a method for
  studying species-environment relationships. \emph{Freshwater Biology}
  \bold{31}, 277--294
}

\author{ Gavin L. Simpson, based on original Matlab code by C.J.F. ter
  Braak and A.P. Schaffers.}

\seealso{ \code{\link{symcoca}} for the function that calls
  \code{fitCoinertia} and \code{\link{coinertiaI}} for co-inertia analysis
  using identity matrices for \eqn{K_1}, \eqn{K_2}, and \eqn{R_0}.
}

\examples{
\testonly{od <- options(digits = 4)}
data(beetles, plants)
coin <- coinertia(beetles, plants)
coin
\testonly{options(od)}
}

\keyword{multivariate}
\keyword{models}
