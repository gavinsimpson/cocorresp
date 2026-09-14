#' Extract Model Residuals
#'
#' Extracts the residuals of the fitted model of a symmetric CoCA
#' to the response and the predictor.
#' @param object an object of class `"symcoca"`.
#' @param ... arguments to be passed to other methods.
#' @returns A list containing the residuals for the response and the
#' predictor with the following components:
#'
#' - **Y**: residuals of the fit to the response.
#'
#' - **X**: residuals of the fit to the predictor.
#' @author Gavin L. Simpson
#' @seealso [symcoca]
#' @keywords multivariate
#' @rdname resid.symcoca
#' @name resid.symcoca
#' @export
#' @examples
#' \dontshow{od <- options(digits = 4)}
#' data(bryophyte)
#' data(vascular)
#' bryo.sym <- coca(bryophyte ~ ., data = vascular, method = "symmetric")
#' eps <- resid(bryo.sym)
#' \dontshow{options(od)}
resid.symcoca <-
  function(object, ...) {
    retval <- object$residuals
    retval
  }

#' @rdname resid.symcoca
#' @export
residuals.symcoca <- function(object, ...) resid.symcoca(object, ...)
