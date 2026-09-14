#' Co-inertia analysis
#'
#' Performs a co-inertia of the triplets \eqn{(Q_1,K_1,R_0)} and
#' \eqn{(Q_2,K_2,R_0)}.
#' @param y,x matrices or data frames of the two data sets for which
#' axes of covariance are sought.
#' @param n.axes numeric; the number of coinertia axes to retain.
#' @param weights a vector of weights for the samples. If `NULL`,
#' weights are chosen from the row sums of `y` (`symmetric =
#' FALSE`) or the average of the row sums of `y` and `x`
#' (`symmetric = TRUE`).
#' @param symmetric logical; should a symmetric set of weights be
#' used. See Details.
#' @param ... arguments passed to other functions. Currently ignored.
#' @returns An object of class `"coinertia"`, which is a list with the
#' following components:
#'
#'
#' - **scores**: a list of ordination scores, with components `species` and `sites`, each of which is a list with components `Y` and `X` that refer to the scores for the input matrices `y` and `x` respectively.
#'
#' - **weights**: numeric vector of row weights used in the analysis.
#'
#' - **lambda**: numeric vector of Eigenvalues.
#'
#' - **n.axes**: numeric; the number of coinertia axes extracted
#'
#' - **symmetric**: logical; was a symmetric analysis performed?
#'
#' - **call**: the matched call.
#' @references Ter Braak, C.J.F and Schaffers, A.P. (2004) Co-Correspondence
#' Analysis: a new ordination method to relate two community
#' compositions. *Ecology* **85(3)**, 834--846
#'
#' Doledec, S and Chessel, D. (1994) Co-inertia analysis: a method for
#' studying species-environment relationships. *Freshwater Biology*
#' **31**, 277--294
#' @author Gavin L. Simpson, based on original Matlab code by C.J.F. ter
#' Braak and A.P. Schaffers.
#' @seealso [symcoca] for the function that calls
#' `fitCoinertia` and [coinertiaI] for co-inertia analysis
#' using identity matrices for \eqn{K_1}, \eqn{K_2}, and \eqn{R_0}.
#' @keywords models
#' @rdname coinertia
#' @name coinertia
#' @aliases print.coinertia summary.coinertia print.summary.coinertia
#' @export
#' @examples
#' \dontshow{od <- options(digits = 4)}
#' data(beetles, plants)
#' coin <- coinertia(beetles, plants)
#' coin
#' \dontshow{options(od)}
coinertia <- function(y, ...) {
  UseMethod("coinertia")
}

#' @rdname coinertia
#' @export
coinertia.default <- function(
  y,
  x,
  n.axes = NULL,
  weights = NULL,
  symmetric = FALSE,
  ...
) {
  validate_pair(y, x, weights, n.axes)
  checkCommunityData(y)
  checkCommunityData(x)
  ## weights is R0
  rsumy <- rowSums(y)
  csumy <- colSums(y)
  toty <- sum(rsumy)
  rsumx <- rowSums(x)
  csumx <- colSums(x)
  totx <- sum(rsumx)
  ## some sanity checks
  if (any(rsumy <= 0)) {
    stop("all row sums must be >0 in data matrix y")
  }
  if (any(csumy <= 0)) {
    y <- y[, csumy > 0, drop = FALSE]
    message(
      "some species contain no data and were removed from data matrix y\n"
    )
    csumy <- csumy[csumy > 0] # colSums(y)
  }
  if (any(rsumx <= 0)) {
    stop("all row sums must be >0 in data matrix x")
  }
  if (any(csumx <= 0)) {
    x <- x[, csumx > 0, drop = FALSE]
    message(
      "some species contain no data and were removed from data matrix x\n"
    )
    csumx <- csumx[csumx > 0] # colSums(x)
  }
  sitesy <- rownames(y)
  sitesx <- rownames(x)
  sppy <- colnames(y)
  sppx <- colnames(x)
  y <- data.matrix(y)
  x <- data.matrix(x)
  nrx <- nrow(y)
  nry <- nrow(x)
  ncy <- ncol(y)
  ncx <- ncol(x)
  if (nrx != nry) {
    stop("Number of rows in y and x is not equal")
  }
  max.axes <- min(ncy, ncx, nry, nrx) - 1
  if (is.null(n.axes)) {
    n.axes <- max.axes
  } else {
    if (n.axes > max.axes) {
      n.axes <- max.axes
      warning("n.axes greater than min(n,p,q)-1,\nreset to min(n,p,q)-1")
    }
  }
  Axes <- seq_len(n.axes)
  ax.names <- paste0("COIN", Axes)
  if (is.null(weights)) {
    if (symmetric) {
      weights <- (rsumy + rsumx) / 2
    } else {
      weights <- rsumy
    }
  }
  .R0 <- weights / sum(weights)
  .csy <- csumy / toty
  .csx <- csumx / totx
  Q1 <- diag(toty / rsumy) %*% y %*% diag(1 / csumy) - 1
  Q2 <- diag(totx / rsumx) %*% x %*% diag(1 / csumx) - 1
  colnames(Q1) <- sppy
  colnames(Q2) <- sppx
  rownames(Q1) <- sitesx
  rownames(Q2) <- sitesy
  rooty <- sqrt(.csy)
  rootx <- sqrt(.csx)
  Dy <- diag(rooty)
  Dx <- diag(rootx)
  A <- Dy %*% t(Q1) %*% diag(.R0) %*% Q2 %*% Dx
  svdA <- La.svd(A)
  U <- diag(1 / rooty) %*% svdA$u
  V <- diag(1 / rootx) %*% t(svdA$vt)
  Ksi <- Q1 %*% diag(.csy) %*% U
  Psi <- Q2 %*% diag(.csx) %*% V
  L <- svdA$d^2
  U1 <- U[, Axes, drop = FALSE]
  U2 <- V[, Axes, drop = FALSE]
  colnames(U1) <- colnames(U2) <- ax.names
  rownames(U1) <- colnames(Q1)
  rownames(U2) <- colnames(Q2)
  X1 <- Ksi[, Axes, drop = FALSE]
  X2 <- Psi[, Axes, drop = FALSE]
  colnames(X1) <- colnames(X2) <- ax.names
  lambda <- L[Axes]
  names(lambda) <- ax.names
  res <- list(
    scores = list(species = list(Y = U1, X = U2), sites = list(Y = X1, X = X2)),
    weights = weights,
    lambda = lambda,
    n.axes = n.axes,
    symmetric = symmetric,
    call = match.call()
  )
  class(res) <- c("coinertia", "list")
  res
}

#' @noRd
#' @export
print.coinertia <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  writeLines("\nCoinertia Analysis\n")
  writeLines(strwrap(pasteCall(x$call)))
  cat("\nEigenvalues:\n")
  print(round(eigenvals(x), digits), ..., print.gap = 3)
  invisible(x)
}

#' @noRd
#' @export
eigenvals.coinertia <- function(x, ...) {
  x$lambda
}
