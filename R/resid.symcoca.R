#' Extract Model Residuals
#'
#' Extracts the residuals of the fitted model of a symmetric CoCA
#'   to the response and the predictor.
#' @param object an object of class `"symcoca"`.
#' @param \ldots arguments to be passed to other methods.
#' @returns A list containing the residuals for the response and the 
#'   predictor with the following components:
#'   \item{Y}{residuals of the fit to the response.}
#'   \item{X}{residuals of the fit to the predictor.}
#' @author Gavin L. Simpson
#' @seealso [symcoca]
#' @examples
#' \dontshow{od <- options(digits = 4)}
#' data(bryophyte)
#' data(vascular)
#' bryo.sym <- coca(bryophyte ~ ., data = vascular, method = "symmetric")
#' eps <- resid(bryo.sym)
#' \dontshow{options(od)}
#' @keywords multivariate
#' @rdname resid.symcoca
#' @export
"resid.symcoca" <-
function(object, ...)
  {
    retval <- object$residuals
    retval
  }

