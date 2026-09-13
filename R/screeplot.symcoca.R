#' Screeplot for symmetric co-correspondence analysis
#'
#' Produces a screeplot of the eigenvalues of a symmetric co-correspondence analysis.
#' @param x an object of class `"symcoca"`, the result of
#'     a call to [symcoca].
#' @param type the type of points to draw; see [points].
#' @param xlab, ylab labels for the x and y axes. If none supplied
#'     suitable labels are formed from the result object.
#' @param \ldots other graphical parameters as in 'par' may also be passed as
#'     arguments.
#' @author Gavin L. Simpson.
#' @seealso [coca], [screeplot], [plot.default]
#' @examples
#' ## symmetric CoCA
#' data(beetles)
#' data(plants)
#'
#' ## log transform the bettle data
#' beetles <- log(beetles + 1)
#'
#' ## fit the model
#' bp.sym <- coca(beetles ~ ., data = plants, method = "symmetric")
#'
#' ## draw a screeplot of the eignevalues
#' screeplot(bp.sym)
#' @keywords methods
#' @rdname screeplot.symcoca
#' @export
`screeplot.symcoca` <- function(x, type = "b",
                                xlab = NULL, ylab = NULL, ...) {
    if (is.null(ylab)) {
        ylab <- "Eigenvalue"
    }
    if (is.null(xlab)) {
        xlab <- "Co-CA Axis"
    }
    evals <- eigenvals(x)
    xvals <- seq_along(evals)
    plot(xvals, evals, type = type, xlab = xlab, ylab = ylab, ...)
    invisible()
}
