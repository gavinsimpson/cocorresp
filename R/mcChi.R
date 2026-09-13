#' Standardised chi-square residuals
#'
#' Scales a matrix, Y, to its standardised chi-square residuals \eqn{(o -
#' e) / \sqrt{e}}{(o - e) / sqrt(e)} (if \eqn{R_0 = R}, where R contains the row
#' sums of matrix Y) so that further analysis can be unweighted
#' @details This function implements equation 8 of ter Braak and Schaffers (2004)
#' by firstly applying equation 7 to form matrix Q using row and column
#' sums of Y as weights, and, secondly, by applying equation 8 to form a
#' matrix of standardised chi-square residuals from Q by
#' pre-multiplication of Q by \eqn{\sqrt{R_0}}{sqrt(R0)} and
#' post-multiplication of Q by \eqn{\sqrt{K}}{sqrt(K)}, where K is the
#' column sums of Y.
#' @param Y a matrix for which standardised chi-square residuals are to
#' be calculated.
#' @param R0 row weights.
#' @param eps tolerance - leave as default.
#' @returns A list with the following components:
#'
#' - **Ychi**: the matrix of standardised chi-squared residuals of Y
#'
#' - **Kn**: the column sums (K) of Y divided by sum(K)
#' @references Ter Braak, C.J.F and Schaffers, A.P. (2004) Co-Correspondence
#' Analysis: a new ordination method to relate two community
#' compositions. *Ecology* **85(3)**, 834--846
#' @author Gavin L. Simpson, based on Matlab code by C.J.F. ter Braak and
#' A.P. Schaffers.
#' @note This function is not intended for casual use by users.
#' @keywords multivariate
#' @rdname mcChi
#' @name mcChi
mcChi <-
  function(Y, R0, eps = 0.000001) {
    rsum <- rowSums(Y)
    csum <- colSums(Y)
    Kn <- csum / sum(csum)
    Keps <- pmax(Kn, eps)
    Q <- sweep(Y / rsum, 2L, Keps, "/") - 1
    .R0 <- R0 / sum(R0)
    Ychi <- sweep(Q * sqrt(.R0), 2L, sqrt(Kn), "*")
    rownames(Ychi) <- rownames(Y)
    colnames(Ychi) <- colnames(Y)
    retval <- list(Ychi = Ychi, Kn = Kn)
    class(retval) <- "mcChi"
    retval
  }

#' @noRd
mcLin <-
  function(X, R0, eps = 0.000000001) {
    ## calculates the weighted autoscaled Xs and the weighted mean
    ## and standard deviation of columns of matrix X using row weights W
    ## (a col vector) and multiplies with sqrt of W so that rXs can be
    ## put in an unweighted analysis
    Wn <- R0 / sum(R0)
    Wmeans <- colSums(diag(Wn) %*% X)
    rXs <- sweep(X, 2, Wmeans)
    sd <- sqrt(colSums(diag(Wn) %*% (rXs * rXs)))
    rXs <- sweep(rXs, 2, (sd + eps), "/")
    rXs <- diag(sqrt(Wn)) %*% rXs
    retval <- list(rXs = rXs, mean = Wmeans, sd = sd)
    class(retval) <- "mcLin"
    retval
  }
