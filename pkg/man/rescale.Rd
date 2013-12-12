\name{rescale}
\alias{rescale}
\alias{rescale.default}
\alias{rescale.symcoca}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Rescales CoCA species scores}
\description{
  Rescales CoCA species scores to the quarter root of the 
  eigenvalues.
}
\usage{
rescale(object, \ldots)

\method{rescale}{default}(object, \ldots)

\method{rescale}{symcoca}(object, axes = c(1:object$n.axes), \ldots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{an R object. Currently only objects of class 
    \code{"symcoca"} are supported.}
  \item{axes}{the number of axes to rescale}
  \item{\dots}{other arguments to be passed to \code{rescale}
    methods. Currently not used here.}
}
\details{
  Currently only implemented for objects of class \code{"symcoca"}.
}
\value{
  Returns a list with the following components:
  \item{U1}{rescaled species scores for the response}
  \item{U2}{rescaled species scores for the preditor}
}

\author{Matlab original by C.J.F. ter Braak and A.P. Schaffers. 
  R port by Gavin L. Simpson.}
\seealso{\code{\link{symcoca}}}
\examples{
\testonly{od <- options(digits = 4)}
data(bryophyte)
data(vascular)

bryo.sym <- coca(bryophyte ~ ., data = vascular, method = "symmetric")

\donttest{rescale(bryo.sym, axes = 1:2)}
\testonly{options(od)}
}
\keyword{multivariate}
