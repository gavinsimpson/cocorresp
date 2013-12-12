\name{crossval}
\alias{crossval}
\alias{print.crossval}
\alias{summary.crossval}
\alias{print.summary.crossval}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Cross-validation for predictive Co-Correspondence
  Analysis models}

\description{
  Performs a leave-one-out cross-validation of a predictive
  Co-Correspondence Analysis model.
}
\usage{
crossval(y, x, n.axes = min(dim(x), dim(y)) - 1,
         centre = TRUE, verbose = TRUE)

%\method{print}{crossval}(x, digits = min(3, getOption("digits") - 3), \ldots)

\method{summary}{crossval}(object, axes = c(1:min(6, object$n.axes)), \dots)
}

\arguments{
  \item{y}{the response species matrix.}
  \item{x}{the predictor species matrix.}
  \item{n.axes}{the number of axes to calculate the leave-one-out
    cross-validation for. Default is to perform the CV for all
    extractable axes.}
  \item{centre}{centre \code{y} and \code{x} during analysis? Currently
    ignored as it may not be neccessary.}
  \item{verbose}{if \code{TRUE}, the default, print information on the
    progress of the cross-validation procedure.}
  \item{object}{an object of class \code{crossval} as returned by
    \code{crossval}.}
  \item{axes}{ the number of axes to summarise results for.}
  %\item{digits}{ the number of digits to print to the R console window.}
  \item{\dots}{ further arguments to \code{print} - currently ignored.}
}
\details{
   Performs a leave-one-out cross-validation of a predictive
  Co-Correspondence Analysis model. It can be slow depending on the
  number of columns in the matrices, and of course the number of sites.
}
\value{
  Returns a large list with the following components:
  \item{dimx, dimy }{the dimensions of the input matrices \code{x} and
    \code{y} respectively.}
  
  \item{press0 }{the \eqn{press_0} statistic.}
  
  \item{n.axes }{the number of axes tested.}
  
  \item{CVfit }{the cross-validatory fit.}
  
  \item{varianceExp}{list with components \code{Yblock} and
    \code{Xblock} containing the variances in the response and the
    predictor respectively, explained by each fitted PLS
    axis.}
 
  \item{totalVar}{list with components \code{Yblock} and \code{Xblock}
    containing the total variance in the response and the predictor
    respectively.}
  
  \item{nam.dat}{list with components \code{namY} and \code{namX}
    containing the names of the response and the predictor(s)
    respectively.}
  
  \item{call }{the R call used.}
}

\author{Gavin L. Simpson, based on Matlab code by C.J.F. ter Braak and
  A.P. Schaffers.}

\note{This function is not a bit out-of-date compared to some of the
  other functions. It should have a formular interface like
  \code{\link{coca}} or work on the results from \code{\link{coca}},
  although that will have to be altered to store a copy of the data? }

\seealso{The model fitting function \code{\link{coca}}}
\examples{
\testonly{od <- options(digits = 4)}
## load the data sets
data(beetles)
data(plants)

## log transform the bettle data
beetles <- log(beetles + 1)

## predictive CoCA using SIMPLS and formula interface
bp.pred <- coca(beetles ~ ., data = plants)
## should retain only the useful PLS components for a
## parsimonious model

## Leave-one-out crossvalidation - this takes a while
\dontrun{
crossval(beetles, plants)
}
## so 2 axes are sufficient
\testonly{options(od)}
}
\keyword{multivariate}
