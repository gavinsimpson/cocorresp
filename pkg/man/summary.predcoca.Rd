\name{summary.predcoca}
\alias{summary.predcoca}
\alias{print.summary.predcoca}
\alias{summary.symcoca}
\alias{print.summary.symcoca}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Summarizing Co-CA Model Fits}
\description{
  \code{summary} methods for classes \code{"predcoca"} and
  \code{"symcoca"}. These provide a summary of the main results of a
  Co-Correspondence Analysis model.
}
\usage{
\method{summary}{predcoca}(object, axes = c(1:min(6, object$n.axes)),
         display = c("species", "sites"), \ldots)

\method{summary}{symcoca}(object, axes = c(1:min(6, object$n.axes)),
        display = c("species", "sites"), scaling = 1, \ldots)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{an object of class \code{"predcoca"} or
    \code{"symcoca"}. Generally the result of a call to \code{\link{coca}}.}
  \item{axes}{the number of CoCA axes to return in the result set.}
  \item{display}{one or both of \code{"species"} and/or \code{"sites"}}
  \item{scaling}{for objects of class \code{"symcoca"} only, the scaling
    to be applied to the results. One of \code{"1"} or \code{"2"}. See
    below for details of scalings used.}
  \item{\ldots}{arguments to be passed to other methods.}
}
%\details{}
\value{
  A list with the some of the following components:
  \item{cocaScores}{The site and or species scores for the axes requested.}
  \item{call}{The call used to fit the model.}
  \item{lambda}{The eigenvalues for the axes requested. Not for
    \code{\link{predcoca.simpls}}.}
  \item{namY, namX}{the names of the response and predictor either
    supplied by the user or derived from the original call.}
  \item{loadings}{a list with two components \code{loadings1} and
    \code{loadings2}, which refer to the response and the predictor
    matrices respectively. (Only for predictive CoCA models.)}
  \item{varianceExp}{a list with components \code{Yblock} and
    \code{Xblock} containing the amount of variance explained on each
    CoCA axis in the response and the predictor respectively. (Only for
    predictive CoCA models.)}
  \item{totalVar}{a list with components \code{Yblock} and \code{Xblock}
    containing the total variance in the response and the predictor data
    sets respectively}
  \item{inertia}{a list with components \code{total} and \code{residual}
    containing the total and residual inertia (variance) in the response
    and the predictor matrices of a symmetric CoCA model. (Only for
    symmetric CoCA models.)}
  \item{scaling}{the scaling used/requested. (Only for symmetric CoCA models.)}
}
\author{Gavin L. Simpson}

\seealso{The model fitting function \code{\link{coca}}}

\examples{
\testonly{od <- options(digits = 4)}
## symmetric CoCA
data(beetles)
data(plants)

## log transform the bettle data
beetles <- log(beetles + 1)

## fit the model
bp.sym <- coca(beetles ~ ., data = plants, method = "symmetric")
\donttest{summary(bp.sym, axes = 1:4)}

## Different scaling
\donttest{summary(bp.sym, axes = 1:4, scaling = 2)}

## Predictive CoCA
bp.pred <- coca(beetles ~ ., data = plants)
\donttest{summary(bp.pred, axes = 1:2)}
\testonly{options(od)}
}

\keyword{multivariate}
