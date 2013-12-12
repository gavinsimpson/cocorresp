\name{weights.symcoca}
\alias{weights.symcoca}
\alias{weights.predcoca}
\title{Exctract CoCA model weights}
\description{
Extractor function to identify and select appropriate analysis weights
from Co-correspondence Analysis models.
}
\usage{
\method{weights}{symcoca}(object, ...)

\method{weights}{predcoca}(object, ...)
}
\arguments{
  \item{object}{an object of class \code{"symcoca"} or \code{"predcoca"}.}
  \item{\dots}{arguments passed to other methods. Not used.}
}
% \details{
% }
\value{
  A numeric vector of common site weights is currently returned. These
  correspond to \eqn{R_{0}}{R[0]} in ter Braak and Schaffers (2004).
}
\references{
  ter Braak, C.J.F and Schaffers, A.P. (2004) Co-Correspondence
  Analysis: a new ordination method to relate two community
  compositions. \emph{Ecology} \strong{85(3)}, 834--846. 
}
\author{Gavin L. Simpson}

%\note{}

\seealso{\code{\link{envfit.coca}} uses these weights to estimate
  weighted correlations in ordination space. See \code{\link{coca}} for
  details on creating CoCA models.}
\examples{
\testonly{od <- options(digits = 4)}
## symmetric CoCA
data(beetles)
data(plants)

## log transform the bettle data
beetles <- log(beetles + 1)

## fit the model
bp.sym <- coca(beetles ~ ., data = plants, method = "symmetric")

## weights == R[0]
weights(bp.sym)
\testonly{options(od)}
}
\keyword{methods}
\keyword{utilities}
