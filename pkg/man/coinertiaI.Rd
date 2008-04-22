\name{coinertiaI}
\alias{coinertiaI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Coinertia analysis with identity matrices}
\description{
  Performs a co-inertia of the triplets \eqn{(Q_1,K_1,R_0)} and
  \eqn{(Q_2,K_2,R_0)} with identity matrices \eqn{K_1, K_2, R_0}.
}
\usage{
coinertiaI(X, Y, fast = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{Species matrix X.}
  \item{Y}{Species Matrix Y.}
  \item{fast}{If \code{"TRUE"} only return the row scores of
    \code{Y}.}
}
\details{
  Argument \code{fast} is used to return only the row scores of
  \code{Y} in function \code{\link{permutest.coca}}, which speeds the
  permutation test considerably.
}
\value{
  If \code{fast = TRUE}, a matrix of row scores for matrix
  \code{Y} (see \code{scores} below). If
  \code{fast = FALSE} a list with the following components:
  
  \item{weights }{A list with components \code{X} and
    \code{Y} containing the left and right singular vectors
    respectivley of the SVD on the triplets.}
  \item{scores }{A list with components \code{X} and
    \code{Y}, containing the row scores of the
    \code{X} and \code{Y} species matrices. These are
    the result of a matrix multiplication of \code{X} by the
    left singular vectors and \code{Y} by the right singular
    vectors.}
  \item{lambda }{the Eigenvalues of the analysis (the square of the
    singular values from the SVD.}
  \item{call }{the matched function call.}
}
\references{
  Doledec, S and Chessel, D. (1994) Co-inertia analysis: a method for
  studying species-environment relationships. \emph{Freshwater Biology}
  \bold{31}, 277--294.
}
\author{Original Matlab code by C.J.F. ter Braak and A.P. Schaffers. R
  port by Gavin L. Simpson.}
\note{This function is not meant to be called directly by the user. If
  you wish to use it study the code in
  \code{\link{permutest.coca}} to see how it should be called.}
\seealso{\code{\link{coinertia}}}
%\examples{}
\keyword{multivariate}
\keyword{models}
