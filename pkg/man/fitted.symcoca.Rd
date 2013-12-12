\name{fitted.symcoca}
\alias{fitted.symcoca}
\alias{print.fitted.symcoca}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Fitted values of a Symmetric Co-Correpsondence analysis model.}
\description{
  Calculates and extracts the fitted values of a Symmetric
  Co-Correpsondence analysis model.
}
\usage{
\method{fitted}{symcoca}(object, \ldots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{an object of class \code{"symcoca"}}
  \item{\ldots}{arguments to be passed to other methods.}
}

\note{
  This function needs an update and to allow option to restrict
  fitted values to specified axes, and the names of the returned objects
  need making more obvious!}

\value{
  A list with the following components:
  \item{Yhat1 }{the fitted values for the \dQuote{response} matrix.}
  \item{Yhat2 }{the fitted values for the \dQuote{predictor} matrix.}
  \item{nam.dat }{a vector containing the names of the \dQuote{response}
    and \dQuote{predictor} matrices respectively. Used for printing the
    results.}
}

\author{Gavin L. Simpson, based on Matlab code by C.J.F. ter Braak and
  A.P. Schaffers.}

\references{

  Ter Braak, C.J.F and Schaffers, A.P. (2004) Co-Correspondence
  Analysis: a new ordination method to relate two community
  compositions. \emph{Ecology} \bold{85(3)}, 834--846
  
}

\seealso{The model fitting function \code{\link{coca}}}
\examples{
## symmetric CoCA
data(beetles)
data(plants)

## log transform the bettle data
beetles <- log(beetles + 1)

## fit the model
bp.sym <- coca(beetles ~ ., data = plants, method = "symmetric")

## fitted values
bp.fit <- fitted(bp.sym)
\donttest{bp.fit}
}
\keyword{multivariate}
