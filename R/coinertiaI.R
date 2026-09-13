#' Coinertia analysis with identity matrices
#'
#' Performs a co-inertia of the triplets \eqn{(Q_1,K_1,R_0)} and
#' \eqn{(Q_2,K_2,R_0)} with identity matrices \eqn{K_1, K_2, R_0}.
#' @details Argument `fast` is used to return only the row scores of
#' `Y` in function [permutest.coca], which speeds the
#' permutation test considerably.
#' @param X Species matrix X.
#' @param Y Species Matrix Y.
#' @param fast If `"TRUE"` only return the row scores of
#' `Y`.
#' @returns If `fast = TRUE`, a matrix of row scores for matrix
#' `Y` (see `scores` below). If
#' `fast = FALSE` a list with the following components:
#'
#'
#' - **weights**: A list with components `X` and `Y` containing the left and right singular vectors respectively of the SVD on the triplets.
#'
#' - **scores**: A list with components `X` and `Y`, containing the row scores of the `X` and `Y` species matrices. These are the result of a matrix multiplication of `X` by the left singular vectors and `Y` by the right singular vectors.
#'
#' - **lambda**: the Eigenvalues of the analysis (the square of the singular values from the SVD.
#'
#' - **call**: the matched function call.
#' @references Doledec, S and Chessel, D. (1994) Co-inertia analysis: a method for
#' studying species-environment relationships. *Freshwater Biology*
#' **31**, 277--294.
#' @author Original Matlab code by C.J.F. ter Braak and A.P. Schaffers. R
#' port by Gavin L. Simpson.
#' @note This function is not meant to be called directly by the user. If
#' you wish to use it study the code in
#' [permutest.coca] to see how it should be called.
#' @seealso [coinertia]
#' @keywords models
#' @rdname coinertiaI
#' @name coinertiaI
coinertiaI <- function(X, Y, fast = TRUE) {
  A <- t(X) %*% Y
  retval <- if (fast) {
    svdA <- La.svd(A, nu = 0)
    Psi <- Y %*% t(svdA$vt)
  } else {
    svdA <- La.svd(A)
    Ksi <- X %*% svdA$u
    Psi <- Y %*% t(svdA$vt)
    L <- diag(svdA$d)^2
    retval <- list(
      weights = list(X = svdA$u, Y = t(svdA$vt)),
      scores = list(X = Ksi, Y = Psi),
      lambda = L,
      call = match.call()
    )
    class(retval) <- c("coinertiaI", "fitCoinertia")
    retval
  }
  retval
}
