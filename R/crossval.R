#' Cross-validation for predictive Co-Correspondence
#' Analysis models
#'
#' Performs a leave-one-out cross-validation of a predictive
#' Co-Correspondence Analysis model.
#' @details Performs a leave-one-out cross-validation of a predictive
#' Co-Correspondence Analysis model. It can be slow depending on the
#' number of columns in the matrices, and of course the number of sites.
#'
#' Folds run independently; results are combined in site order.
#' `varianceExp` and `totalVar` describe the full-data SIMPLS fit. `centre`
#' is retained for compatibility and currently has no effect.
#' @param y the response species matrix.
#' @param x the predictor species matrix.
#' @param n.axes the number of axes to calculate the leave-one-out
#' cross-validation for. Default is to perform the CV for all
#' extractable axes.
#' @param centre centre `y` and `x` during analysis? Currently
#' ignored as it may not be necessary.
#' @param verbose if `TRUE`, the default, print information on the
#' progress of the cross-validation procedure.
#' @param object an object of class `crossval` as returned by
#' `crossval`.
#' @param axes the number of axes to summarise results for.
#' @param digits the number of digits to print to the R console window.
#' @param ... further arguments to `print` - currently ignored.
#' @returns Returns a large list with the following components:
#'
#' - **dimx, dimy**: the dimensions of the input matrices `x` and `y` respectively.
#'
#'
#' - **press0**: the \eqn{press_0} statistic.
#'
#'
#' - **n.axes**: the number of axes tested.
#'
#'
#' - **CVfit**: the cross-validatory fit.
#'
#'
#' - **varianceExp**: list with components `Yblock` and `Xblock` containing the variances in the response and the predictor respectively, explained by each fitted PLS axis.
#'
#'
#' - **totalVar**: list with components `Yblock` and `Xblock` containing the total variance in the response and the predictor respectively.
#'
#'
#' - **nam.dat**: list with components `namY` and `namX` containing the names of the response and the predictor(s) respectively.
#'
#'
#' - **call**: the R call used.
#' @author Gavin L. Simpson, based on Matlab code by C.J.F. ter Braak and
#' A.P. Schaffers.
#' @note This function is not a bit out-of-date compared to some of the
#' other functions. It should have a formula interface like
#' [coca] or work on the results from [coca],
#' although that will have to be altered to store a copy of the data?
#' @seealso The model fitting function [coca]
#' @keywords multivariate
#' @rdname crossval
#' @export
#' @param parallel A positive integer worker count (default `1L`, serial), a
#' caller-owned [parallel::makeCluster()] cluster, or a function with arguments
#' `X`, `FUN`, and `...` returning a list in input order. Counts greater than one
#' create a PSOCK cluster, which is stopped on exit. Supplied clusters are never
#' stopped. Optional future and futurize adapters are described in
#' `vignette("parallel-computation")`.
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
crossval <- function(
  y,
  x,
  n.axes = min(dim(x), dim(y)) - 1,
  centre = TRUE,
  verbose = TRUE,
  parallel = 1L
) {
  namY <- deparse(substitute(y))
  namX <- deparse(substitute(x))
  y <- checkCommunityData(y)
  x <- checkCommunityData(x)
  validate_pair(y, x, n.axes = n.axes)
  msgIfDroppedVars(c(attr(y, "dropped"), attr(x, "dropped")), list(namY, namX))
  if (n.axes > min(dim(x), dim(y)) - 1L) {
    stop("Number of PLS axes must be less than min(n, p)")
  }
  if (nrow(x) < 3L) {
    stop("Leave-one-out cross-validation needs at least three rows")
  }
  R0 <- rowSums(y) / sum(y)
  mapper <- make_mapper(parallel)
  on.exit(mapper$close(), add = TRUE)
  if (verbose) {
    message("Leave-one-out cross-validation: ", nrow(x), " sites")
  }
  folds <- mapper$map(
    seq_len(nrow(x)),
    cv_fold,
    x = x,
    y = y,
    R0 = R0,
    n.axes = n.axes
  )
  press0 <- sum(vapply(folds, `[[`, numeric(1), "press0"))
  press <- Reduce(`+`, lapply(folds, `[[`, "press"))
  if (!is.finite(press0) || press0 <= 0) {
    stop("Cross-validation has zero baseline inertia")
  }
  fit <- simpls(mcChi(x, R0)$Ychi, mcChi(y, R0)$Ychi, n.axes, stripped = TRUE)
  if (verbose) {
    message("Leave-one-out cross-validation complete")
  }
  structure(
    list(
      dimx = dim(x),
      dimy = dim(y),
      n.axes = n.axes,
      press0 = press0,
      CVfit = 100 * (1 - press / press0),
      varianceExp = list(Xblock = fit$Xvar, Yblock = fit$Yvar),
      totalVar = list(Xblock = fit$Xtotvar, Yblock = fit$Ytotvar),
      call = match.call(),
      nam.dat = list(namY = namY, namX = namX)
    ),
    class = c("crossval", "list")
  )
}

#' @noRd
cv_fold <- function(i, x, y, R0, n.axes) {
  calx <- mcChi(x[-i, , drop = FALSE], R0[-i])
  caly <- mcChi(y[-i, , drop = FALSE], R0[-i])
  testx <- scaleChi(x[i, , drop = FALSE], calx$Kn, R0[i])
  testy <- scaleChi(y[i, , drop = FALSE], caly$Kn, R0[i])
  fit <- simpls(calx$Ychi, caly$Ychi, n.axes, stripped = TRUE)
  press <- vapply(
    seq_len(n.axes),
    function(j) {
      B <- matrix(fit$coefficients[,, j], nrow = ncol(x), ncol = ncol(y))
      sum((testx %*% B - testy)^2)
    },
    numeric(1)
  )
  if (!all(is.finite(press))) {
    stop("non-finite prediction error")
  }
  list(press = press, press0 = sum(testy^2))
}
