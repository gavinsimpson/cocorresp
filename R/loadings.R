#' CoCA species loadings
#'
#' Extract CoCA species loadings from fitted objects.
#' @details `loadings()` is an extractor function to access the loadings of a
#' fitted CoCA model.
#'
#' This is a generic function, replacing the
#' [loadings][stats::loadings] function, which is preserved as the
#' exported default S3 method. Methods are provided for both predictive
#' and symmetric CoCA.
#' @param x an object resulting from a call to [coca].
#' @param choices numeric; vector of Co-CA axes to extract loadings for.
#' @param which character; should the response or predictor scores be
#' plotted. Can be specified in several ways: *response* choices
#' are one from `c("y", "Y", "y1", "response")`; *predictor*
#' choices are one from `c("x", "X", "y2", "predictor")`.
#' @param ... additional arguments to be passed to lower level
#' methods.
#' @returns A list of numeric matrices, or a single matrix when one community is selected.
#' Selecting a single axis simplifies that matrix to a numeric vector.
#' @author Gavin L. Simpson
#' @seealso [coca] for how to fit CoCA models.
#' @examples
#' ## symmetric CoCA
#' data(beetles)
#' ## log transform the bettle data
#' beetles <- log(beetles + 1)
#' data(plants)
#' ## fit the model
#' bp.sym <- coca(beetles ~ ., data = plants, method = "symmetric")
#'
#' ## extract the loadings
#' lds <- loadings(bp.sym)
#' @keywords utilities
#' @rdname loadings
#' @name loadings
#' @aliases lodaings.default
#' @export
loadings <- function(x, ...) {
  UseMethod("loadings")
}
#' @noRd
#' @export
loadings.default <- function(x, ...) stats::loadings(x)
#' @rdname loadings
#' @export
loadings.predcoca <- function(
  x,
  choices = c(1, 2),
  which = c("response", "predictor"),
  ...
) {
  if (!isTRUE(inherits(x, "predcoca"))) {
    stop("x must be of class \"predcoca\"")
  }
  which <- selectWhich(which)
  out <- x[["loadings"]][which]
  out <- lapply(out, `[`, , choices)
  if (length(out) == 1L) {
    out <- out[[1]]
  }
  out
}
#' @rdname loadings
#' @export
loadings.symcoca <- function(x, choices = c(1, 2), which = c("y1", "y2"), ...) {
  if (!isTRUE(inherits(x, "symcoca"))) {
    stop("x must be of class \"symcoca\"")
  }
  which <- selectWhich(which)
  out <- x[["loadings"]][which]
  out <- lapply(out, `[`, , choices)
  if (length(out) == 1L) {
    out <- out[[1]]
  }
  out
}
