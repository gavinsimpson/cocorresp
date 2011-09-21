\name{cocorresp-internal}
\alias{scaleLin}
\alias{mcLin}
\alias{predcoca.simpls}
\alias{predcoca.eigen}
\alias{symcoca}
\title{Internal cocorresp Functions}
\description{
  Internal cocorresp functions.

  \code{predcoca.simpls}, \code{predcoca.eigen} and \code{symcoca}
  perform the actual fitting of Co-CA models.
}
\usage{
predcoca.eigen(y, x, R0 = NULL, n.axes = NULL, nam.dat = NULL)

predcoca.simpls(y, x, R0 = NULL, n.axes = NULL, nam.dat = NULL)

symcoca(y, x, n.axes = NULL, R0 = NULL,
        symmetric = FALSE, nam.dat = NULL)
}
\arguments{

  \item{y}{a data frame containing the response community data matrix.}

  \item{x}{a data frame containing the predictor community data matrix.}

  \item{R0}{a vector of length \code{nrow(y)} of user supplied
    weights for \eqn{R_0}. If \code{weights = NULL} (default) then the
    weights are determined from \code{y} (default) or \code{x} and \code{y}
    (\code{symmetric = TRUE} only).}

  \item{n.axes}{the number of CoCA axes to extract. If missing (default)
    the \code{n.axes} is \eqn{min(ncol(y), ncol(x), nrow(y), nrow(x)) -
      1}.}
  
  \item{symmetric}{if \code{method} is \code{"symmetric"} then
    \code{symmetric} determines whether weights for \eqn{R_0} are
    symmetric and taken as the average of the row sums of \code{x} and
    \code{y} (\code{symmetric = TRUE}). If \code{symmetric = FALSE}
    (default) then the weights \eqn{R_0} are taken as the row sums of \code{y}
    unless user defined weights are provided via argument
    \code{weights}. Ignored if \code{method} is \code{"predictive"}.}

  \item{nam.dat}{an optional list with elements \code{namY} and \code{namX}
    containing the names of \code{y} and \code{x} respectively. Used to
    label printed output. If missing the names of are deduced from
    \code{y} and \code{x}.}
  
  \item{Y}{ a matrix for which standardised chi-square residuals are to
    be calculated.}
  
  \item{eps}{ tolerance - leave as default.}

}
\details{
  These are not to be called by the user.

  \code{predcoca.simpls}, \code{predcoca.eigen} and \code{symcoca}
  perform the actual model fitting but are not meant to be called by the
  user as \code{coca} pre-processes the input data before calling these
  functions.
}
\keyword{ internal }
