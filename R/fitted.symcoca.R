#' Fitted values of a Symmetric Co-Correspondence analysis model.
#'
#' Calculates and extracts the fitted values of a Symmetric
#' Co-Correspondence analysis model.
#' @param object,x an object of class `"symcoca"`
#' @param which character; should the response or predictor scores be
#' plotted. Can be specified in several ways: *response* choices
#' are one from `c("y", "Y", "y1", "response")`; *predictor*
#' choices are one from `c("x", "X", "y2", "predictor")`.
#' @param ... arguments to be passed to other methods.
#' @param digits Number of significant digits for printing.
#' @returns A list with the following components:
#'
#' - **Y**: the fitted values for the "response" matrix.
#'
#' - **X**: the fitted values for the "predictor" matrix.
#'
#' - **nam.dat**: a vector containing the names of the "response" and "predictor" matrices respectively. Used for printing the results.
#' @references Ter Braak, C.J.F and Schaffers, A.P. (2004) Co-Correspondence
#' Analysis: a new ordination method to relate two community
#' compositions. *Ecology* **85(3)**, 834--846
#' @author Gavin L. Simpson, based on Matlab code by C.J.F. ter Braak and
#' A.P. Schaffers.
#' @note This function needs an update and to allow option to restrict
#' fitted values to specified axes, and the names of the returned objects
#' need making more obvious!
#' @seealso The model fitting function [coca]
#' @keywords multivariate
#' @rdname fitted.symcoca
#' @export
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
#' ## fitted values
#' bp.fit <- fitted(bp.sym)
#' \donttest{bp.fit}
#'
#' ## fitted values for beetles only
#' beetle.fit <- fitted(bp.sym, which = "y1")
fitted.symcoca <- function(object, which = c("y1", "y2"), ...) {
  `getFitted` <- function(x, take) {
    TAKE <- ifelse(take == "Y", 1L, 2L)
    rsum <- x$rowsum[[TAKE]]
    csum <- x$colsum[[TAKE]]
    tot <- sum(rsum)
    exp <- rsum %*% t(csum) / tot
    Yhat <- exp *
      (1 +
        (x$scores$site[[take]] %*%
          t(x$scores$species[[take]])))
    rownames(Yhat) <- rownames(x$scores$site[[take]])
    Yhat
  }
  which <- selectWhich(which)
  out <- list()
  nam <- character(0)
  if ("Y" %in% which) {
    out$Y <- getFitted(object, take = "Y")
    nam <- c(nam, object$nam.dat$namY)
  }
  if ("X" %in% which) {
    out$X <- getFitted(object, take = "X")
    nam <- c(nam, object$nam.dat$namX)
  }
  out$nam.dat <- nam
  names(out$nam.dat) <- c("namY", "namX")[c("Y", "X") %in% which]
  class(out) <- "fitted.symcoca"
  out
}
