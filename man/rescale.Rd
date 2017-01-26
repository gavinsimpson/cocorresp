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
rescale(object, \dots)

\method{rescale}{default}(object, \dots)

\method{rescale}{symcoca}(object, choices = NULL,
        display = c("species", "sites"), \dots)
}

\arguments{
  \item{object}{an R object. Currently only objects of class 
    \code{"symcoca"} are supported.}
  \item{choices}{numeric; which axes to rescale and return.}
  \item{display}{character; which type of scores to rescale.}
  \item{\dots}{other arguments to be passed to \code{rescale}
    methods. Currently not used.}
}
\details{
  Currently only implemented for objects of class \code{"symcoca"}.
}
\value{
  Returns a list with one or both of the following components:
  \item{species}{rescaled species scores for the response}
  \item{sites}{rescaled species scores for the preditor}
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
