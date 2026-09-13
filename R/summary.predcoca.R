#' Summarising Co-CA Model Fits
#'
#' `summary` methods for classes `"predcoca"` and
#' `"symcoca"`. These provide a summary of the main results of a
#' Co-Correspondence Analysis model.
#' @param object,x an object of class `"predcoca"` or
#' `"symcoca"`. Generally the result of a call to
#' [coca].
#' @param axes numeric; how many axes to summarise? The default is to
#' display 6 axes or all available axes, whichever is the smaller.
#' @param ... arguments to be passed to other methods.
#' @param digits Number of significant digits for printing.
#' @returns A list with the some of the following components:
#'
#' - **cocaScores**: The site and/or species scores for the axes requested.
#'
#' - **call**: The call used to fit the model.
#'
#' - **lambda**: The eigenvalues for the axes requested. Not for [predcoca.simpls].
#'
#' - **namY, namX**: the names of the response and predictor either supplied by the user or derived from the original call.
#'
#' - **loadings**: a list with two components `loadings1` and `loadings2`, which refer to the response and the predictor matrices respectively. (Only for predictive CoCA models.)
#'
#' - **varianceExp**: a list with components `Yblock` and `Xblock` containing the amount of variance explained on each CoCA axis in the response and the predictor respectively. (Only for predictive CoCA models.)
#'
#' - **totalVar**: a list with components `Yblock` and `Xblock` containing the total variance in the response and the predictor data sets respectively
#'
#' - **inertia**: a list with components `total` and `residual` containing the total and residual inertia (variance) in the response and the predictor matrices of a symmetric CoCA model. (Only for symmetric CoCA models.)
#'
#' - **scaling**: the scaling used/requested. (Only for symmetric CoCA models.)
#' @author Gavin L. Simpson
#' @seealso The model fitting function [coca]
#' @examples
#' \dontshow{od <- options(digits = 4)}
#' ## symmetric CoCA
#' data(beetles)
#' data(plants)
#'
#' ## log transform the bettle data
#' beetles <- log(beetles + 1)
#'
#' ## fit the model
#' bp.sym <- coca(beetles ~ ., data = plants, method = "symmetric")
#' summary(bp.sym)
#'
#' ## Predictive CoCA
#' bp.pred <- coca(beetles ~ ., data = plants)
#' summary(bp.pred, axes = 1:2)
#' \dontshow{options(od)}
#' @keywords multivariate
#' @rdname summary.predcoca
#' @export
summary.predcoca <- function(object, axes = NULL, ...) {
  if (missing(axes) || is.null(axes)) {
    axes <- seq_len(min(6, object$n.axes))
  }
  cocaScores <- scores(object, choices = axes, ...)
  retval <- list(
    cocaScores = cocaScores,
    call = object$call,
    lambda = eigenvals(object),
    namY = object$nam.dat$namY,
    namX = object$nam.dat$namX,
    loadings = lapply(object$loadings, `[`, , axes),
    varianceExp = object$varianceExp,
    totalVar = object$totalVar
  )
  class(retval) <- "summary.predcoca"
  retval
}
