#' Standardised chi-square residuals
#'
#' Scales a matrix, Y, to is standardised chi-square residuals \eqn{(o -
#' e) / \sqrt{e}}{(o - e) / sqrt(e)} (given \eqn{K_n} and \eqn{R_0}
#' metrics derived from an external matrix \eqn{Y_0}) so that further
#' analysis can be unweighted.
#' @param Y a matrix for which standardised chi-square residuals are to
#' be calculated.
#' @param Kn the column sums (K) of Y divided by sum(K).
#' @param R0 row weights.
#' @param eps a tolerance.
#' @returns
#' - **Yr**: the matrix of standardised chi-squared residuals of Y.
#' @references Ter Braak, C.J.F and Schaffers, A.P. (2004) Co-Correspondence
#' Analysis: a new ordination method to relate two community
#' compositions. *Ecology* **85(3)**, 834--846
#' @author Gavin L. Simpson, based on Matlab code by C.J.F. ter Braak and
#' A.P. Schaffers.
#' @note This function is not intended for casual use by users.
#' @keywords multivariate
#' @rdname scaleChi
#' @name scaleChi
scaleChi <-
  function(Y, Kn, R0, eps = 0.000001) {
    R <- as.matrix(rowSums(Y))
    R0 <- as.matrix(R0)
    Keps <- pmax(Kn, eps)
    Yr <- sweep(Y / as.vector(R), 2L, Keps, "/") - 1
    Yr <- sweep(Yr * as.vector(sqrt(R0)), 2L, sqrt(Kn), "*")
    Yr
  }

#' @noRd
scaleLin <-
  function(X, mean, sd, r, eps = 0.00000001) {
    ## [Xs] = scale_lin(X, mean, sd, r)
    ## centers and standardizes wrt to a prefined mean and
    ## standard deviation used in crossval_chi_lin
    X <- sweep(X, 2, mean)
    X <- sweep(X, 2, sd + eps, "/")
    r <- as.matrix(r)
    Xs <- X * as.vector(sqrt(r))
    Xs
  }
