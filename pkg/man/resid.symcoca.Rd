\name{resid.symcoca}
\alias{resid.symcoca}
\title{Extract Model Residuals}
\description{
  Extracts the residuals of the fitted model of a symmetric CoCA
  to the response and the predictor. 
}
\usage{
\method{resid}{symcoca}(object, \ldots)
}
\arguments{
  \item{object}{an object of class \code{"symcoca"}.}
  \item{\ldots}{arguments to be passed to other methods.}
}
\value{
  A list containing the residuals for the response and the 
  predictor with the following components:
  \item{Y}{residuals of the fit to the repsonse.}
  \item{X}{residuals of the fit to the predictor.}
}
\author{Gavin L. Simpson}
\seealso{\code{\link{symcoca}}}
\examples{
\testonly{od <- options(digits = 4)}
data(bryophyte)
data(vascular)
bryo.sym <- coca(bryophyte ~ ., data = vascular, method = "symmetric")
eps <- resid(bryo.sym)
\testonly{options(od)}
}
\keyword{multivariate}
