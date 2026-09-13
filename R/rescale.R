#' Rescales CoCA species scores
#'
#' Rescales CoCA species scores to the quarter root of the
#' eigenvalues.
#' @details Currently only implemented for objects of class `"symcoca"`.
#' @param object an R object. Currently only objects of class
#' `"symcoca"` are supported.
#' @param choices numeric; which axes to rescale and return.
#' @param display character; which type of scores to rescale.
#' @param ... other arguments to be passed to `rescale`
#' methods. Currently not used.
#' @returns Returns a list with one or both of the following components:
#'
#' - **species**: rescaled species scores for the response
#'
#' - **sites**: rescaled species scores for the predictor
#' @author Matlab original by C.J.F. ter Braak and A.P. Schaffers.
#' R port by Gavin L. Simpson.
#' @seealso [symcoca]
#' @examples
#' \dontshow{od <- options(digits = 4)}
#' data(bryophyte)
#' data(vascular)
#'
#' bryo.sym <- coca(bryophyte ~ ., data = vascular, method = "symmetric")
#'
#' \donttest{rescale(bryo.sym, axes = 1:2)}
#' \dontshow{options(od)}
#' @keywords multivariate
#' @rdname rescale
#' @export
rescale <-
  function(object, ...) {
    UseMethod("rescale")
  }
