\name{envfit.coca}
\alias{envfit.coca}
\title{
  Fit an environmental vector or factor to a Co-CA ordination
}
\description{
  The function fits environmental vectors or factors to a Co-CA
  ordination. The projections of points onto vectors have maximum
  correlation with corresponding environmental variables, and the
  factors show the averages of factor levels.
}
\usage{
\method{envfit}{coca}(ord, env, which = c("response", "predictor"),
       choices = c(1, 2), scaling = FALSE, w, na.rm = FALSE,
       strata, permutations = 999, ...)
}
\arguments{
  \item{ord}{a Co-CA ordination object, the result of a call to
    \code{\link{coca}}.}
  \item{env}{a data frame, matrix or vector of environmental/external
    variable(s) to be fitted to the ordination. The variables may be
    of a mixed type (factors and continuous variables) in a data frame.}
  \item{which}{character; which of the response or predictor ordinations
    should be used during fitting of vectors and factors.}
  \item{choices}{numeric; the axes to which vectors and factors are
    fitted.}
  \item{scaling}{logical; should sacling be applied. See
    \code{\link{scores.symcoca}}.}
  \item{w}{weights used in fitting vectors and factors.}
  \item{na.rm}{Remove points with missing values in ordination scores or
    environmental variables. The operation is casewise; the whole row of
    data is removed if there is a missing value and \code{na.rm =
      TRUE}.}
  \item{strata}{An integer vector or factor specifying the strata for
    permutation. If supplied, observations are permuted only within the
    specified strata.}
  \item{permutations}{Number of permutations for assessing significance
    of vectors or factors. Set to \code{0} to skip permutations.}
  \item{\dots}{Arguments passed to \code{\link{vectorfit}} and
    \code{\link{factorfit}}.}
}
\details{
  See \code{\link{envfit}} for details of the method.
}
\value{
  Returns an object of class \code{\link{envfit}}. 
}
\author{
  Gavin L. Simpson. The code interfaces with and uses code from
  \code{\link{envfit}} for the main computations, which was written by
  Jari Oksanen.
}
% \section{Warning}{
%   This function is experimental. The vectors fitted are unweighted
%   whereas to be consistent with the \code{\link{cca}} method of
%   \code{\link{envfit}} they should be weighted. I just need to work out
%   the correct sets of weights to use.
% }
\seealso{
  \code{\link{coca}} for fitting models. \code{\link{envfit}} for
  details of the generic function and the computations performed.
}
\examples{
## symmetric CoCA
data(beetles)
data(plants)

## log transform the bettle data
beetles <- log(beetles + 1)

## fit the model
bp.sym <- coca(beetles ~ ., data = plants, method = "symmetric")

## load the environmental data
data(verges)

## fit vectors for the environmental data
sol <- envfit(bp.sym, verges, which = "response")
sol

## plot the response matrix and the fitted vectors
plot(bp.sym)
plot(sol)
}
\keyword{ multivariate }
\keyword{ htest }
