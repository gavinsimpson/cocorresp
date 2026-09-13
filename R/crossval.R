#' Cross-validation for predictive Co-Correspondence
#'   Analysis models
#'
#' Performs a leave-one-out cross-validation of a predictive
#'   Co-Correspondence Analysis model.
#' @details Performs a leave-one-out cross-validation of a predictive
#'   Co-Correspondence Analysis model. It can be slow depending on the
#'   number of columns in the matrices, and of course the number of sites.
#' @param y the response species matrix.
#' @param x the predictor species matrix.
#' @param n.axes the number of axes to calculate the leave-one-out
#'     cross-validation for. Default is to perform the CV for all
#'     extractable axes.
#' @param centre centre `y` and `x` during analysis? Currently
#'     ignored as it may not be necessary.
#' @param verbose if `TRUE`, the default, print information on the
#'     progress of the cross-validation procedure.
#' @param object an object of class `crossval` as returned by
#'     `crossval`.
#' @param axes the number of axes to summarise results for.
#' @param digits the number of digits to print to the R console window.
#' @param ... further arguments to `print` - currently ignored.
#' @returns Returns a large list with the following components:
#'   \item{dimx, dimy }{the dimensions of the input matrices `x` and
#'     `y` respectively.}
#'   
#'   \item{press0 }{the \eqn{press_0} statistic.}
#'   
#'   \item{n.axes }{the number of axes tested.}
#'   
#'   \item{CVfit }{the cross-validatory fit.}
#'   
#'   \item{varianceExp}{list with components `Yblock` and
#'     `Xblock` containing the variances in the response and the
#'     predictor respectively, explained by each fitted PLS
#'     axis.}
#'  
#'   \item{totalVar}{list with components `Yblock` and `Xblock`
#'     containing the total variance in the response and the predictor
#'     respectively.}
#'   
#'   \item{nam.dat}{list with components `namY` and `namX`
#'     containing the names of the response and the predictor(s)
#'     respectively.}
#'   
#'   \item{call }{the R call used.}
#' @author Gavin L. Simpson, based on Matlab code by C.J.F. ter Braak and
#'   A.P. Schaffers.
#' @note This function is not a bit out-of-date compared to some of the
#'   other functions. It should have a formula interface like
#'   [coca] or work on the results from [coca],
#'   although that will have to be altered to store a copy of the data?
#' @seealso The model fitting function [coca]
#' @examples
#' \dontshow{od <- options(digits = 4)}
#' ## load the data sets
#' data(beetles)
#' data(plants)
#'
#' ## log transform the bettle data
#' beetles <- log(beetles + 1)
#'
#' ## predictive CoCA using SIMPLS and formula interface
#' bp.pred <- coca(beetles ~ ., data = plants)
#' ## should retain only the useful PLS components for a
#' ## parsimonious model
#'
#' ## Leave-one-out crossvalidation - this takes a while
#' \dontrun{
#' crossval(beetles, plants)
#' }
#' ## so 2 axes are sufficient
#' \dontshow{options(od)}
#' @keywords multivariate
#' @rdname crossval
#' @export
`crossval` <- function(y, x, n.axes = min(dim(x), dim(y)) - 1, centre = TRUE,
                       verbose = TRUE) {
    ## Y1 = X or predictor matrix
    ## Y2 = Y or the response matrix
    ## This is consistent with predcoca, and probably better to use
    ## X and Y in the predcoca case as it is a regression and also fits in
    ## with future formula interface
    namY <- deparse(substitute(y))
    namX <- deparse(substitute(x))
    if(any(rowSums(y) <= 0 ))
        stop("all row sums must be >0 in data matrix y")
    if(any((csum <- colSums(y)) <= 0 )) {
        y <- y[, csum > 0, drop = FALSE]
        message("some species contain no data and were removed from data matrix y\n")
    }
    if(any(rowSums(x) <= 0 ))
        stop("all row sums must be >0 in data matrix x")
    if(any((csum <- colSums(x)) <= 0 )) {
        x <- x[, csum > 0, drop = FALSE]
        message("some species contain no data and were removed from data matrix x\n")
    }
    x <- as.matrix(x)
    y <- as.matrix(y)
    dimx <- dim(x)
    dimy <- dim(y)
    if (n.axes > (min(dimx, dimy) - 1))
        stop("Number of PLS axes must be less than min(n, p)")
    cumpress <- matrix(0, dimy[2], n.axes)
    R0 <- rowSums(y)
    R0 <- R0 / sum(R0)
    ## Do Leave-one-out cv
    press <- matrix(0, dimx[1] * dimy[2], n.axes)
    press0 <-  0
    xChi <- mcChi(x, R0)
    yChi <- mcChi(y, R0)
    for (i in seq_len(dimx[1])) {
        if(verbose) {
            cat("LOO - Site:", i)
            flush.console()
        }
        xChi.loo <- mcChi(x[-i, ], R0[-i])
        testx <- scaleChi(x[i, , drop = FALSE], xChi.loo$Kn, R0[i])
        calx <- xChi.loo$Ychi
        yChi.loo <- mcChi(y[-i, ], R0[-i])
        testy <- scaleChi(y[i, , drop = FALSE], yChi.loo$Kn, R0[i])
        caly <- yChi.loo$Ychi
        press0 <- press0 + sum(testy^2)
        simpls.Xblock <- simpls(calx, caly, n.axes, stripped = TRUE)
        for (j in seq_len(n.axes)) {
            ypred <- testx %*% simpls.Xblock$coefficients[, , j]
            row.inds <- ((i-1) * dimy[2]+1):(i * dimy[2])
            press[row.inds, j] <- t(ypred - testy)^2
        }
        for (k in seq_len(dimy[2]))
            cumpress[k, ] <- colSums(press[seq(k, (dimx[1] * dimy[2]),
                                               by = dimy[2]) ,])
        if(verbose) {
            cat(" - Complete\n")
            flush.console()
        }
    }
    simpls.Yblock <- simpls(xChi$Ychi, yChi$Ychi, n.axes, stripped = TRUE)
    retval <- list(dimx = dimx, dimy = dimy, n.axes = n.axes,
                   press0 = press0, #rmsecv = rmsecv,
                   #rmsec = rmsec, rmsecs = rmsecs, CVfit = CVfit,
                   CVfit = 100 * (1 - colSums(cumpress) / press0),
                   varianceExp = list(Xblock = simpls.Xblock$Xvar,
                        Yblock = simpls.Yblock$Xvar),
                   totalVar = list(Xblock = simpls.Xblock$Xtotvar,
                     Yblock = simpls.Yblock$Xtotvar),
                   call = match.call(),
                   nam.dat = list(namY = namY, namX = namX))#,
                   #simpls.Xblock, simpls.Yblock)
    class(retval) <- c("crossval", "list")
    retval
}
