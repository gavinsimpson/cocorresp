#' Permutation test for predictive co-correspondence analysis
#'   models
#'
#' A permutation test for predictive co-correspondence analysis models to
#'   assess the significance of each CoCA ordination axes.
#' @details An alternative approach to cross-validation (see
#'   [crossval]) to select the number of axes to retain in a
#'   predictive co-correspondence analysis is to test the statistical
#'   significance of each ordination axis using permutation tests.
#'
#'   The test statistic used is the *F*-ratio based on the fit of the
#'   first axis to the response data (ter Braak and Smilauer 2002). The
#'   second and subsequent axes are tested by treating previous axes as
#'   co-variables.
#'
#'   To be precise, this approach does not test the significance of SIMPLS
#'   axes, but those of NIPALS-PLS axes (ter Braak and de Jong 1998).
#' @param x an object of class `"predcoca"`.
#' @param R0 row weights to use in the analysis. If missing, the
#'     default, these are determined from `x`.
#' @param permutations the number of permutations to perform.
#' @param n.axes The number of axes to test. Defaults to the number of
#'     axes stated in `x$n.axes`.
#' @param verbose if `TRUE`, the default, print information on the
#'     progress of the permutation test procedure.
#' @param object an object of class `"permutest.coca"`.
#' @param \ldots arguments to be passed to other methods.
#' @returns A list with the following components:
#'   \item{pval }{a vector of *P*-values for each ordination axis.}
#'   \item{permstat }{a vector of values for the test statistic for each axis.}
#'   \item{total.inertia }{the total inertia in the response matrix.}
#'   \item{inertia }{a vector containing the *residualised*
#'     inertia. This is the total inertia in the response *after*
#'     removing the inertia explained by all previous axes. For the first
#'     CoCA axis this is, by definition, the total inertia in the
#'     response.}
#'   \item{fitax }{a vector containing the amount of inertia in the
#'     response matrix explained by each ordination axis.}
#'   \item{pcent.fit }{a vector containing the fit of each axis to the
#'     response as a percentage of the total inertia (variance).}
#'   \item{n.axes }{the number of axes in the ordination.}
#'   \item{call }{the matched call.}
#' @references ter Braak, C.J.F. and de Jong, S. (1998) The objective function of
#'   partial least squares regression. *Journal of Chemometrics*
#'   **12**, 41--54.
#'
#'   ter Braak, C.J.F and Schaffers, A.P. (2004) Co-Correspondence
#'   Analysis: a new ordination method to relate two community
#'   compositions. *Ecology* **85(3)**, 834--846.
#'
#'   ter Braak, C.J.F. and Smilauer, P. (2002) *Canoco reference manual
#'     and CanoDraw for Windows user's guide: software for canonical
#'     community ordination. Version 4.5*. New York: Microcomputer Power.
#' @author Gavin L. Simpson, based on Matlab code by C.J.F. ter Braak and
#'   A.P. Schaffers.
#' @note Argument `R0` is provided for compatibility with the original
#'   MATLAB code. The R usage paradigm makes this argument redundant in the
#'   current code and it may be invalid to supply different row weights
#'   (\eqn{R_0}) as `R0`. This argument will likely be removed in future
#'   versions.
#' @seealso [coca], for the model fitting function,
#'   [crossval], for a leave-one-out cross-validation
#'   procedure, which is the preferred way to select axes in a predictive
#'   co-correspondence analysis.
#' @examples
#' \dontshow{
#' suppressWarnings(RNGversion("3.5.0"))
#' od <- options(digits = 4)
#' }
#' ## load some data
#' data(beetles)
#' data(plants)
#'
#' ## log transform the bettle data
#' beetles <- log(beetles + 1)
#' ## predictive CoCA using SIMPLS and formula interface
#' bp.pred <- coca(beetles ~ ., data = plants)
#'
#' ## should retain only the useful PLS components for a parsimonious model
#' \donttest{
#' ## Leave-one-out crossvalidation - this takes a while
#' crossval(beetles, plants)
#' ## so 2 axes are sufficient
#' }
#'
#' ## permutation test
#' ## (Testing the first 2 axes & only 25 perms for speed.)
#' bp.perm <- permutest(bp.pred, permutations = 25, n.axes = 2)
#' bp.perm
#'
#' \dontshow{options(od)}
#' @keywords multivariate
#' @section Warning:
#' This function is **slow**. Beware setting argument
#'   `permutations` higher than the default. Determine how long it
#'   takes for the default 99 permutations to complete before going crazy
#'   and asking for thousands of permutations - you've been warned, have a
#'   good book to hand.
#' @rdname permutest.coca
#' @export
"permutest.coca" <- function(x, R0 = NULL, permutations = 99,
                             n.axes = x$n.axes, verbose = TRUE, ...) {
    permtest <- function(Y, X1, X0 = NULL, permutations, step) {
        Y.dim <- dim(Y)
        X1.dim <- dim(X1)
        if(!identical(Y.dim[1], X1.dim[1])) {
            stop("Matrix X1 must have the same number of rows as Y")
        }
        if (is.null(X0)) {
            mu <- matrix(0, nrow = Y.dim[1], ncol = Y.dim[2])
        } else {
            X0.dim <- dim(X0)
            if(!identical(Y.dim[1], X0.dim[1]))
                stop("Matrix X0 must have the same number of rows as Y")
            if((X1.dim[2] <= X0.dim[2]))
                stop("Matrix X1 must have more columns than X0")
            mu <- qr.coef(qr(X0), Y)
            mu <- X0 %*% mu
        }
        E <- Y - mu
        SS0 <- sum(sum(E^2))
        stati <- numeric(length = permutations + 1)
        stati[1] <- teststat(Y = E, X0 = X0, X1 = X1, step)
        YresPerm <- matrix(0, nrow = Y.dim[1], ncol = Y.dim[2])
        for(i in 2:(permutations + 1)) {
            YresPerm[sample(Y.dim[1]), ] <- E
            stati[i] <- teststat(Y = YresPerm, X0 = X0, X1 = X1, step)
        }
        pval <- sum(stati >= stati[1]) / (permutations + 1)
        retval <- list(pval = pval, stat = stati[1], stati = stati)
        class(retval) <- "permtest"
        return(retval)
    }
    teststat <- function(Y, X0, X1, step) {
        Psi <- coinertiaI(X = Y, Y = X1, fast = TRUE)
        if(is.null(X0)) {
            fit.X0 <- 0
            X0X1 <- Psi[ , 1, drop = FALSE]
        } else {
            fit.X0 <- residualMatrix(Y = Y, X = X0)$inertia$fitted
            X0X1 <- matrix(c(X0, Psi[ , 1, drop = FALSE]), ncol = step)
        }
        resid.res <- residualMatrix(Y = Y, X = X0X1)
        fit.lambda1 <- (fit.X0X1 <- resid.res$inertia$fitted) - fit.X0
        retval <- fit.lambda1 / (resid.res$inertia$total - fit.X0X1)
        return(retval)
    }
    residualMatrix <- function(Y, X) {
        Q <- qr.coef(qr(X), Y)
        Yf <- X %*% Q
        Yr <- Y - Yf
        tot.inertia <- sum(sum(Y^2))
        resid.inertia <- sum(sum(Yr^2))
        fit.inertia <- sum(sum(Yf^2))
        zerosum <- (fit.inertia + resid.inertia) - tot.inertia
        if (abs(zerosum) > 0.000001)
            warning("Residual inertia + fitted inertia did not equal total inertia.\n\t",
                    call. = FALSE)
        retval <- list(inertia = list(total = tot.inertia,
                       residual = resid.inertia,
                       fitted = fit.inertia),
                       Yr = Yr, zerosum = zerosum)
        class(retval) <- "residualMatrix"
        return(retval)
    }
    if(!inherits(x, "predcoca"))
        stop("x must be of class 'predcoca'")
    if(is.null(R0)) {
        .R0 <- x$R0
    } else {
        .R0 <- R0
    }
    Ychi1 <- x$Ychi$Ychi1
    Ychi2 <- x$Ychi$Ychi2
    if(n.axes > x$n.axes) {
        n.axes <- x$n.axes
        warning("n.axes too large, reset to x$n.axes.")
    }
    pval <- permstat <- inertia <- fitax <- numeric(n.axes)
    for(j in 1:n.axes) {
        if(verbose) {
            cat("Permutations for axis:", j)
            flush.console()
        }
        if(j == 1)
            covar <- NULL
        ptest <- permtest(Ychi1, Ychi2, X0 = covar, permutations, step = j)
        if(j == 1)
            stati.Ax1 <- ptest$stati
        permstat[j] <- ptest$stati[1]
        pval[j] <- ptest$pval
        Psi <- coinertiaI(X = Ychi1, Y = Ychi2, fast = TRUE)[, 1, drop = FALSE]
        res.mat1 <- residualMatrix(Ychi1, Psi)
        Ychi1 <- res.mat1$Yr
        if(j == 1) {
            total.inertia1 <- res.mat1$inertia$total
        }
        res.mat2 <- residualMatrix(Ychi2, Psi)
        Ychi2 <- res.mat2$Yr
        if(is.null(covar)) {
            covar <- cbind(NULL, Psi)
        } else {
            covar <- cbind(covar, Psi)
        }
        inertia[j] <- res.mat1$inertia$total
        fitax[j] <- res.mat1$inertia$fitted
        if(verbose) {
            cat(" - completed\n")
            flush.console()
        }
    }
    pcent.fit <- 100 * fitax / total.inertia1
    retval <- list(pval = pval, permstat = permstat,
                   total.inertia = total.inertia1,
                   inertia = inertia, fitax = fitax,
                   pcent.fit = pcent.fit, n.axes = n.axes,
                   ##Ychi1 = Ychi1, Ychi2 = Ychi2, stati.Ax1 = stati.Ax1
                   call = match.call())
    class(retval) <- "permutest.coca"
    retval
}

