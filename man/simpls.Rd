\encoding{latin1}
\name{simpls}
\alias{simpls}
\title{Modified version of Sijmen de Jong's SIMPLS}
\description{Fits a PLSR model with the SIMPLS algorithm, modified to
  allow a weighted analysis.}
\usage{simpls(X, Y, ncomp, stripped = FALSE, \dots)}
\arguments{
  \item{X}{a matrix of observations.  \code{NA}s and \code{Inf}s are not
    allowed.} 
  \item{Y}{a vector or matrix of responses.  \code{NA}s and \code{Inf}s
    are not allowed.} 
  \item{ncomp}{the number of components to be used in the
    modelling.}
  \item{stripped}{logical.  If \code{TRUE} the calculations are stripped
    as much as possible for speed; this is meant for use with
    cross-validation or simulations when only the coefficients are
    needed.  Defaults to \code{FALSE}.}
  \item{\dots}{other arguments.  Currently ignored.}
}
\details{This function is a modified version of
  \code{\link[pls]{simpls.fit}} from package \code{pls}. Four
  modification have been made:
  \enumerate{
    \item The input matrices \code{X} and \code{Y} are not centered,
    \item The scores (\code{tt} in the code) are not centered,
    \item Added code to calculate the total variance in the \code{Y}
      matrix, \code{Ytotvar}, and the variance in \code{Y} accounted for
      by each PLS axis, \code{Yvar} (See Value below), and
    \item Additional components are returned if argument \code{stripped}
    is \code{TRUE}.
  }

  This function should not be called directly, but through
  the generic function \code{\link{coca}}.

  SIMPLS is much faster than
  the NIPALS algorithm, especially when the number of X variables
  increases, but gives slightly different results in the case of
  multivariate Y. SIMPLS truly maximises the covariance criterion.
  According to de Jong, the standard PLS2 algorithms lie closer to
  ordinary least-squares regression where a precise fit is sought;
  SIMPLS lies closer to PCR with stable  predictions.}

\value{A list containing the following components is returned:
  \item{coefficients}{an array of regression coefficients for 1, \ldots,
    \code{ncomp} components.  The dimensions of \code{coefficients} are
    \code{c(nvar, npred, ncomp)} with \code{nvar} the number
    of \code{X} variables and \code{npred} the number of variables to be
    predicted in \code{Y}.}
  \item{scores}{a matrix of scores.}
  \item{loadings}{a matrix of loadings.}
  \item{Yscores}{a matrix of Y-scores.}
  \item{Yloadings}{a matrix of Y-loadings.}
  \item{projection}{the projection matrix used to convert X to scores.}
  \item{Xmeans}{a vector of means of the X variables.}
  \item{Ymeans}{a vector of means of the Y variables.}
  \item{fitted.values}{an array of fitted values.  The dimensions of
    \code{fitted.values} are \code{c(nobj, npred, ncomp)} with
    \code{nobj} the number samples and \code{npred} the number of
    Y variables.}
  \item{residuals}{an array of regression residuals.  It has the same
    dimensions as \code{fitted.values}.}
  \item{Xvar}{a vector with the amount of X-variance explained by each
    number of components.}
  \item{Yvar}{a vector with the amount of Y-variance explained by each
    number of components.}
  \item{Xtotvar}{Total variance in \code{X}.}
  \item{Ytotvar}{Total variance in \code{Y}.}

  If \code{stripped} is \code{TRUE}, only the components
  \code{coefficients}, \code{Xmeans} and \code{Ymeans}, \code{Xvar} and
  \code{Yvar}, and \code{Xtotvar} and \code{Ytotvar} are returned.
}
\references{
  de Jong, S. (1993) SIMPLS: an alternative approach to partial least
  squares regression.  \emph{Chemometrics and Intelligent Laboratory Systems},
  \bold{18}, 251--263.
}
\author{Based on \code{\link[pls]{simpls.fit}} by Ron Wehrens and
  Bjorn-Helge Mevik, with simple modifications by Gavin L. Simpson.}

\seealso{
  \code{\link{coca}}
}
\keyword{regression}
\keyword{multivariate}

