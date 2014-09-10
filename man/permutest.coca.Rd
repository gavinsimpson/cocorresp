\name{permutest.coca}
\alias{permutest.coca}
\alias{print.permutest.coca}
\alias{summary.permutest.coca}
\alias{print.summary.permutest.coca}

\title{Permutation test for predictive co-correspondence analysis
  models}

\description{
  A permutation test for predictive co-correspondence analysis models to
  assess the significance of each CoCA ordination axes.
}
\usage{
\method{permutest}{coca}(x, R0 = NULL, permutations = 99,
               n.axes = x$n.axes, verbose = TRUE, \ldots)

%\method{print}{permutest.coca}(x, digits = max(3, getOption("digits") - 3), \ldots)

\method{summary}{permutest.coca}(object, \ldots)

%\method{print}{summary.permutest.coca}(x, digits = max(3, getOption("digits") - 3), \ldots)

}

\arguments{
  \item{x}{ an object of class \code{"predcoca"}.}
  \item{R0}{ row weights to use in the analysis. If missing, the
    default, these are determined from \code{x}.}
  \item{permutations}{ the number of permutations to perform.}
  \item{n.axes}{ The number of axes to test. Defaults to the number of
    axes stated in \code{x$n.axes}.}
  \item{verbose}{if \code{TRUE}, the default, print information on the
    progress of the permutation test procedure.}
  \item{object}{ an object of class \code{"permutest.coca"}.}
  \item{\ldots}{arguments to be passed to other methods.}
}
\details{
  An alternative approach to cross-validation (see
  \code{\link{crossval}}) to select the number of axes to retain in a
  predictive co-correspondence analysis is to test the statistical
  significance of each ordination axis using permutation tests.

  The test statistic used is the \emph{F}-ratio based on the fit of the
  first axis to the response data (ter Braak and Smilauer 2002). The
  second and subsequent axes are tested by treating previous axes as
  co-variables.

  To be precise, this approach does not test the significance of SIMPLS
  axes, but those of NIPALS-PLS axes (ter Braak and de Jong 1998).
}
\value{
  A list with the following components:
  \item{pval }{a vector of \emph{P}-values for each ordination axis.}
  \item{permstat }{a vector of values for the test statistic for each axis.}
  \item{total.inertia }{the total inertia in the response matrix.}
  \item{inertia }{a vector containing the inertia explained by each
    ordination axis.}
  \item{fitax }{a vector containing the fit of each axis to response.}
  \item{pcent.fit }{a vector containing the fit of each axis to the
    response as a percentage of the total inertia (variance).}
  \item{n.axes }{the number of axes in the ordination.}
  \item{call }{the matched call.}
}
\references{
  
  ter Braak, C.J.F. and de Jong, S. (1998) The objective function of
  partial least squares regression. \emph{Journal of Chemometrics}
  \bold{12}, 41--54.

  ter Braak, C.J.F and Schaffers, A.P. (2004) Co-Correspondence
  Analysis: a new ordination method to relate two community
  compositions. \emph{Ecology} \bold{85(3)}, 834--846.

  ter Braak, C.J.F. and Smilauer, P. (2002) \emph{Canoco reference manual
    and CanoDraw for Windows user's guide: software for canonical
    community ordination. Version 4.5}. New York: Microcomputer Power.
}

\author{Gavin L. Simpson, based on Matlab code by C.J.F. ter Braak and
  A.P. Schaffers.}

\note{
  Argument \code{R0} is provided for compatibility with the original
  MATLAB code. The R usage paradigm makes this argument redundant in the
  current code and it may be invalid to supply different row weights
  (\eqn{R_0}) as \code{R0}. This argument will likely be removed in future
  versions.
}

\section{Warning}{
  This function is \bold{slow}. Beware setting argument
  \code{permutations} higher than the default. Determine how long it
  takes for the default 99 permutations to complete before going crazy
  and asking for thousands of permutations - you've been warned, have a
  good book to hand.
}

\seealso{
  \code{\link{coca}}, for the model fitting function,
  \code{\link{crossval}}, for a leave-one-out cross-validation
  procedure, which is the preferred way to select axes in a predictive
  co-correspondence analysis.
}
\examples{
\testonly{od <- options(digits = 4)}
## load some data
data(beetles)
data(plants)

## log transform the bettle data
beetles <- log(beetles + 1)
## predictive CoCA using SIMPLS and formula interface
bp.pred <- coca(beetles ~ ., data = plants)

## should retain only the useful PLS components for a parsimonious model
\dontrun{
## Leave-one-out crossvalidation - this takes a while
crossval(beetles, plants)
## so 2 axes are sufficient

## permutation test to assess significant PLS components - takes a while
bp.perm <- permutest(bp.pred, permutations = 99)
bp.perm
summary(bp.perm)
}

## permutation test, this time only testing the first 2 axes
bp.perm <- permutest(bp.pred, permutations = 99, n.axes = 2)
bp.perm
\testonly{options(od)}
}
\keyword{multivariate}
