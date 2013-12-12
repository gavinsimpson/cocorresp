\name{corAxis}
\alias{corAxis.symcoca}
\alias{corAxis}
\alias{corAxis.default}

\title{Correlation between ordination axes}
\description{
  Calculates the Pearson product-moment correlation coefficient for the
  site scores of ordination axes.
}
\usage{

corAxis(x, ...)

\method{corAxis}{default}(x, ...)

\method{corAxis}{symcoca}(x, axes = c(1:min(6, x$n.axes)), ...)
}

\arguments{
  \item{x}{an ordination object. Only methods for objects of class
    \code{\link{symcoca}} are currently available.}
  \item{axes}{the number of axes to calculate the correlation
    coefficients for.}
  \item{\dots}{arguments to be passed on to other methods.}
}
%\details{}
\value{
  A named vector containing the correlation coefficients for the
  requested axes.
}

\author{Gavin L. Simpson}

\note{The arguments for \code{cor} are hard coded at their
  defaults, see \code{\link{cor}} for details. A more flexible version
  is planned that will allow arguments to be passed to \code{cor}.}

\seealso{
  \code{\link{cor}}, for the main analysis function.
}
\examples{
\testonly{od <- options(digits = 4)}
## load some data
data(beetles)
data(plants)

## log transform the beetle data
beetles <- log(beetles + 1)

## symmetric Co-CA model
beetles.sym <- coca(beetles ~ ., data = plants, method = "symmetric")

## correlations between axes
corAxis(beetles.sym)
\testonly{options(od)}
}
\keyword{univar}
