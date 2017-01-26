\name{loadings}
\alias{loadings}
\alias{lodaings.default}
\alias{loadings.predcoca}
\alias{loadings.symcoca}

\title{CoCA species loadings}
\description{
  Extract CoCA species loadings from fitted objects.
}
\usage{
loadings(x, \ldots)

\method{loadings}{predcoca}(x, choices = c(1, 2),
         which = c("response", "predictor"), \ldots)

\method{loadings}{symcoca}(x, choices = c(1, 2),
         which = c("y1", "y2"), \ldots)

}

\arguments{
  \item{x}{an object resulting from a call to \code{\link{coca}}.}
  
  \item{choices}{numeric; vector of Co-CA axes to extract loadings for.}

  \item{which}{character; for which matrices should loadings be
    extracted?}

  \item{\dots}{additional arguments to be passed to lower level
    methods.}
}
\details{
  \code{loadings()} is an extractor function to access the loadings of a
  fitted CoCA model.

  This is a generic function, replacing the
  \code{\link[stats]{loadings}} function, which is preserved as the
  exported default S3 method. Methods are provided for both predictive
  and symmetric CoCA.
}
\value{
  A list of data frames or a single data frame depending on other arguments.
}
\author{
  Gavin L. Simpson
}

\seealso{

  \code{\link{coca}} for how to fit CoCA models.
}

\examples{
\testonly{
od <- options(digits = 4)
}
## symmetric CoCA
data(beetles)
## log transform the bettle data
beetles <- log(beetles + 1)
data(plants)
## fit the model
bp.sym <- coca(beetles ~ ., data = plants, method = "symmetric")

## extract the loadings
lds <- loadings(bp.sym)
head(lds[["Y"]])                        # loadings for the Y_1 matrix
str(lds)

\testonly{options(od)}
}
\keyword{methods}
\keyword{utilities}
