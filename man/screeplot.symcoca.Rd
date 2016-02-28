\name{screeplot.symcoca}
\alias{screeplot.symcoca}
\title{Screeplot for symmetric co-correspondence analysis}
\description{
  Produces a screeplot of the eigenvalues of a symmetric co-correspondence analysis.
}
\usage{
\method{screeplot}{symcoca}(x, type = "b",
    xlab = NULL, ylab = NULL,
    \ldots)
}

\arguments{
  \item{x}{ an object of class \code{"symcoca"}, the result of
    a call to \code{\link{symcoca}}.}
  \item{type}{ the type of points to draw; see \code{\link{points}}.}
  \item{xlab, ylab}{ labels for the x and y axes. If none supplied
    suitable labels are formed from the result object.}
  \item{\ldots}{ other graphical parameters as in 'par' may also be passed as
    arguments.}
}

\author{Gavin L. Simpson.}

\seealso{\code{\link{coca}}, \code{\link{screeplot}}, \code{\link{plot.default}}}

\examples{
## symmetric CoCA
data(beetles)
data(plants)

## log transform the bettle data
beetles <- log(beetles + 1)

## fit the model
bp.sym <- coca(beetles ~ ., data = plants, method = "symmetric")

## draw a biplot of the response
screeplot(bp.sym)

}
\keyword{hplot}
\keyword{methods}
