#' @rdname corAxis
#' @name corAxis
#' @export
corAxis <- function(x, ...) {
  UseMethod("corAxis")
}

#' @rdname corAxis
#' @export
corAxis.default <- function(x, ...) {
  stop("No default method for corAxis")
}

#' Correlation between ordination axes
#'
#' Calculates the Pearson product-moment correlation coefficient for the
#' site scores of ordination axes.
#' @param x an ordination object. Only methods for objects of class
#' [symcoca] are currently available.
#' @param axes numeric; the number of axes to calculate the correlation
#' coefficients for. If `NULL`, coefficients for all axes are
#' returned.
#' @param ... arguments to be passed on to other methods.
#' @returns A named vector containing the correlation coefficients for the
#' requested axes.
#' @author Gavin L. Simpson
#' @note The arguments for `cor` are hard coded at their
#' defaults, see [cor] for details. A more flexible version
#' is planned that will allow arguments to be passed to `cor`.
#' @seealso [cor], for the main analysis function.
#' @keywords univar
#' @rdname corAxis
#' @export
#' @examples
#' \dontshow{od <- options(digits = 4)}
#' ## load some data
#' data(beetles)
#' data(plants)
#'
#' ## log transform the beetle data
#' beetles <- log(beetles + 1)
#'
#' ## symmetric Co-CA model
#' beetles.sym <- coca(beetles ~ ., data = plants, method = "symmetric")
#'
#' ## correlations between axes
#' corAxis(beetles.sym)
#' \dontshow{options(od)}
corAxis.symcoca <- function(x, axes = NULL, ...) {
  if (!inherits(x, "symcoca")) {
    stop("object must be of class \"symcoca\"")
  }
  if (is.null(axes)) {
    axes <- seq_len(x$n.axes)
  }
  scrs <- scores(x, axes, display = "sites")
  diag(cor(scrs$sites$Y, scrs$sites$X))
}
