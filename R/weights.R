#' Extract CoCA model weights
#'
#' Extractor function to identify and select appropriate analysis weights
#' from Co-correspondence Analysis models.
#' @param object an object of class `"symcoca"` or `"predcoca"`.
#' @param ... arguments passed to other methods. Not used.
#' @returns A numeric vector of common site weights is currently returned. These
#' correspond to \eqn{R_{0}}{R[0]} in ter Braak and Schaffers (2004).
#' @references ter Braak, C.J.F and Schaffers, A.P. (2004) Co-Correspondence
#' Analysis: a new ordination method to relate two community
#' compositions. *Ecology* **85(3)**, 834--846.
#' @author Gavin L. Simpson
#' @seealso [envfit.coca] uses these weights to estimate
#' weighted correlations in ordination space. See [coca] for
#' details on creating CoCA models.
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
#'
#' ## weights == R[0]
#' weights(bp.sym)
#' \dontshow{options(od)}
#' @keywords utilities
#' @rdname weights
#' @name weights.symcoca
#' @export
weights.symcoca <- function(object, ...) {
  unname(object$weights)
}

#' @rdname weights
#' @export
weights.predcoca <- function(object, ...) {
  unname(object$R0)
}
