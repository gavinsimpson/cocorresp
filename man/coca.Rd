\name{coca}
\alias{coca}
\alias{coca.formula}
\alias{coca.default}
\alias{print.predcoca}
\alias{print.symcoca}
\concept{ordination}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Fit Co-Correspondence Analysis Ordination Models}
\description{
  \code{coca} is used to fit Co-Correspondence Analysis (CoCA)
  models. It can fit predictive or symmetric models to two community
  data matrices containing species abundance data.
}
\usage{
coca(y, \ldots)

\method{coca}{default}(y, x, method = c("predictive", "symmetric"),
     reg.method = c("simpls", "eigen"), weights = NULL,
     n.axes = NULL, symmetric = FALSE, \ldots)

\method{coca}{formula}(formula, data, method = c("predictive", "symmetric"),
     reg.method = c("simpls", "eigen"), weights = NULL,
     n.axes = NULL, symmetric = FALSE, \ldots)

}

\arguments{
  \item{y}{a data frame containing the response community data matrix.}

  \item{x}{a data frame containing the predictor community data matrix.}

  \item{formula}{a symbolic description of the model to be fit. The
    details of model specification are given below.}

  \item{data}{an optional data frame containing the variables in the model.
    If not found in \code{data}, the variables are taken from
    \code{environment(formula)}, typically the environment from which
    \code{coca} is called.}

  \item{method}{a character string indicating which co-correspondence
    analysis method to use. One of \code{"predictive"}(default), or
    \code{"symmetric"}, can be abbreviated.}

  \item{reg.method}{One of \code{"simpls"} (default) or
    \code{"eigen"}. If \code{method} is \code{"predictive"} then
    \code{reg.method} controls whether the co-correspondence analysis
    should be fitted using the SIMPLS algorithm or via an eigen
    analysis.}

  \item{weights}{a vector of length \code{nrow(y)} of user supplied
    weights for \eqn{R_0}. If \code{weights = NULL} (default) then the
    weights are determined from \code{y} (default) or \code{x} and \code{y}
    (\code{symmetric = TRUE} only).}

  \item{n.axes}{the number of CoCA axes to extract. If missing (default)
    the \code{n.axes} is \deqn{min(ncol(y), ncol(x), nrow(y), nrow(x)) -
      1}.}

  \item{symmetric}{if \code{method} is \code{"symmetric"} then
    \code{symmetric} determines whether weights for \eqn{R_0} are
    symmetric and taken as the average of the row sums of \code{x} and
    \code{y} (\code{symmetric = TRUE}). If \code{symmetric = FALSE}
    (default) then the weights \eqn{R_0} are taken as the row sums of \code{y}
    unless user defined weights are provided via argument
    \code{weights}. Ignored if \code{method} is \code{"predictive"}.}

  \item{\dots}{additional arguments to be passed to lower level methods.}
}
\details{
  \code{coca} is the main user-callable function.

  A typical model has the form \code{response ~ terms} where
  \code{response} is the (numeric) response data frame and \code{terms}
  is a series of terms which specifies a linear predictor for
  \code{response}. A typical form for \code{terms} is \code{.},
  which is shorthand for "all variables" in \code{data}. If \code{.} is
  used, \code{data} must also be provided. If specific species
  (variables) are required then \code{terms} should take the form
  \code{spp1 + spp2 + spp3}.

  The default is to fit a predictive CoCA model using SIMPLS via a
  modified version of \code{\link[pls]{simpls.fit}} from package
  \code{pls}. Alternatively, \code{reg.method = "eigen"} fits the model
  using an older, slower eigen analysis version of the SIMPLS
  algorithm. \code{reg.method = "eigen"} is about 100\% slower than
  \code{reg.method = "simpls"}.
}
\value{
  \code{coca} returns a list with \code{method} and \code{reg.method}
  determining the actual components returned.

  \item{nam.dat}{list with components \code{namY} and
    \code{namX} containing the names of the response and the
    predictor(s) respectively.}

  \item{call}{the matched call.}

  \item{method}{the CoCA method used, one of \code{"predictive"}
    or \code{"symmetric"}.}

  \item{scores}{the species and site scores of the fitted
    model.}

  \item{loadings}{the site loadings of the fitted model for the response
    and the predictor. (Predictive CoCA via SIMPLS only.)}

  \item{fitted}{the fitted values for the response. A list with 2
    components \code{Yhat} (the fitted values) and
    \code{Yhat1} (the transformed fitted values. (Predictive
    CoCA via SIMPLS only.)}

  \item{varianceExp}{list with components \code{Yblock} and
    \code{Xblock} containing the variances in the response and
    the predictor respectively, explained by each fitted PLS
    axis. (Predictive CoCA via SIMPLS only.)}

  \item{totalVar}{list with components \code{Yblock} and
    \code{Xblock} containing the total variance in the response
    and the predictor respectively. (Predictive CoCA via SIMPLS only.)}

  \item{lambda}{the Eigenvalues of the analysis.}

  \item{n.axes}{the number of fitted axes}

  \item{Ychi}{a list containing the mean-centered chi-square matrices
    for the response (\code{Ychi1}) and the predictor
    (\code{Ychi2}). (Predictive CoCA only.)}

  \item{R0}{the (possibly user-supplied) row weights used in the analysis.}

  \item{X}{X-Matrix (symmetric CoCA only).}

  \item{residuals}{Residuals of a symmetric model (symmetric CoCA
    only).}

  \item{inertia}{list with components \code{total} and
    \code{residual} containing the total and residual inertia
    for the response and the predictor (symmetric CoCA only).}

  \item{rowsum}{a list with the row sums for the response
    (\code{rsum1}) and the preditor (\code{rsum2})
    (symmetric CoCA only).}
  
  \item{colsum}{a list with the column sums for the response
    (\code{csum1})and the preditor (\code{csum2})
    (symmetric CoCA only).}
}
\references{

  ter Braak, C.J.F and Schaffers, A.P. (2004) Co-Correspondence
  Analysis: a new ordination method to relate two community
  compositions. \emph{Ecology} \bold{85(3)}, 834--846
  
}
\author{
  Original Matlab code by C.J.F. ter Braak and A.P. Schaffers. R
  port by Gavin L. Simpson. Formula method for \code{coca} uses a
  modified version of \code{\link[vegan]{ordiParseFormula}} by Jari
  Oksanen to handle formulea.
}

\seealso{

  \code{\link{crossval}} for cross-validation and
  \code{\link{permutest.coca}} for permutation test to determine the
  number of PLS axes to retain in for predictive CoCA.
  
  \code{\link{summary.predcoca}} and \code{\link{summary.symcoca}} for
  summary methods.}

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
bp.sym
\donttest{summary(bp.sym)}
plot(bp.sym)

## predictive CoCA using SIMPLS and formula interface
bp.pred <- coca(beetles ~ ., data = plants)
## should retain only the useful PLS components for a parsimonious model

## Leave-one-out crossvalidation - this takes a while

\dontrun{
crossval(beetles, plants)
## so 2 axes are sufficient
## permutation test to assess significant PLS components - takes a while
bp.perm <- permutest(bp.pred, permutations = 99)
bp.perm}

## agrees with the Leave-one-out cross-validation
## refit the model with only 2 PLS components
bp.pred <- coca(beetles ~ ., data = plants, n.axes = 2)
bp.pred
\donttest{summary(bp.pred)}
plot(bp.pred)

## predictive CoCA using Eigen-analysis
data(bryophyte)
data(vascular)
carp.pred <- coca(y = bryophyte, x = vascular, reg.method = "eigen")
carp.pred

## determine important PLS components - takes a while
\dontrun{
crossval(bryophyte, vascular)
(carp.perm <- permutest.coca(carp.pred, permutations = 99))}

## 2 components again, refit
carp.pred <- coca(y = bryophyte, x = vascular,
                  reg.method = "eigen", n.axes = 2)
carp.pred
## plot
plot(carp.pred)
\testonly{options(od)}
}
\keyword{multivariate}
\keyword{regression}
\keyword{models}
