#' Co-inertia analysis
#'
#' Performs a co-inertia of the triplets \eqn{(Q_1,K_1,R_0)} and
#' \eqn{(Q_2,K_2,R_0)}.
#' @param X \eqn{Q_1}, matrix of expected abundances under row-column
#' independence in the original `Y` species matrix when
#' treated as a contingency table.
#' @param Dp \eqn{K_1}, species (column) weights for `X`.
#' @param Y \eqn{Q_2}, matrix of expected abundances under row-column
#' independence in the original `X` species matrix when
#' treated as a contingency table.
#' @param Dq \eqn{K_2}, species (column) weights for `Y`.
#' @param Dn site weights \eqn{R_0}.
#' @param n.axes number of axes to calculate the co-inertia analysis
#' for.
#' @param object,x an object of class `coinertia`.
#' @param axes the number of axes to display when printing.
#' @param digits the number of significant digits to use in printing
#' @param ... arguments passed to other functions. Currently ignored.
#' @returns A list with the following components:
#'
#' - **U1**: column weights of `X`.
#'
#' - **U2**: column weights of `Y`.
#'
#' - **X1**: rowscores of `X`.
#'
#' - **X2**: rowscores of `Y`.
#'
#' - **lambda**: the Eigenvalues (squares of the singular values).
#'
#' - **n.axes**: number of axes requested.
#'
#' - **call**: the matched function call.
#' @references Ter Braak, C.J.F and Schaffers, A.P. (2004) Co-Correspondence
#' Analysis: a new ordination method to relate two community
#' compositions. *Ecology* **85(3)**, 834--846
#'
#' Doledec, S and Chessel, D. (1994) Co-inertia analysis: a method for
#' studying species-environment relationships. *Freshwater Biology*
#' **31**, 277--294
#' @author Original Matlab code by C.J.F. ter Braak and A.P. Schaffers. R
#' port by Gavin L. Simpson.
#' @note This function is not yet meant to be called directly by the
#' user. If you wish to use it directly, see the function definition for
#' [symcoca] which demonstrates how to prepare the
#' relevant input matrices.
#'
#' Note that in this function, `X` corresponds to the input
#' matrix `y` and `Y` corresponds to the
#' input matrix `x` in [symcoca].
#' Confusing! This will be changed in a future release but for now the
#' arguments follow those of the original Matlab code - perhaps a little
#' too closely!
#' @seealso [symcoca] for the function that calls
#' `fitCoinertia` and [coinertiaI] for co-inertia analysis
#' using identity matrices for \eqn{K_1}, \eqn{K_2}, and \eqn{R_0}
#' @keywords models
#' @rdname fitCoinertia
fitCoinertia <- function(X, Dp, Y, Dq, Dn, n.axes) {
  ax.names <- paste("COCA", 1:n.axes, sep = " ")
  Dp05 <- diag(sqrt(Dp))
  Dq05 <- diag(sqrt(Dq))
  A <- Dp05 %*% t(X) %*% diag(Dn) %*% Y %*% Dq05
  svdA <- La.svd(A)
  U <- diag(1 / sqrt(Dp)) %*% svdA$u
  V <- diag(1 / sqrt(Dq)) %*% t(svdA$vt)
  Ksi <- X %*% diag(Dp) %*% U
  Psi <- Y %*% diag(Dq) %*% V
  L <- diag(svdA$d)
  L <- L * L
  seqA <- seq_len(n.axes)
  U1 <- U[, seqA, drop = FALSE]
  U2 <- V[, seqA, drop = FALSE]
  colnames(U1) <- colnames(U2) <- ax.names
  rownames(U1) <- colnames(X)
  rownames(U2) <- colnames(Y)
  X1 <- Ksi[, seqA, drop = FALSE]
  X2 <- Psi[, seqA, drop = FALSE]
  colnames(X1) <- colnames(X2) <- ax.names
  lambda <- diag(L[seqA, seqA, drop = FALSE])
  names(lambda) <- ax.names
  retval <- list(
    scores = list(species = list(Y = U1, X = U2), site = list(Y = X1, X = X2)),
    lambda = lambda,
    n.axes = n.axes,
    call = match.call()
  )
  class(retval) <- "fitCoinertia"
  retval
}
