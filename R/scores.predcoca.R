#' Get Species or Site Scores from an Ordination
#'
#' Function to access either species or site scores for specified axes in
#' co-correspondence analysis ordination methods.
#' @details Implements a [scores][vegan::scores] method for symmetric
#' co-correspondence analysis ordination results.
#' @param x an ordination result
#' @param display partial match to access scores for "sites"
#' "species", "loadings" or "xmatrix". The latter
#' two are only available for [symcoca].
#' @param choices numeric; the ordination axes to return.
#' @param scaling logical; whether scores should be rescaled
#' by the quarter root of the eigenvalues using
#' [rescale.symcoca].
#' @param ... arguments to be passed to other methods.
#' @returns A list with one or more components containing matrices of the
#' requested scores:
#'
#'
#' - **species**: A list with two components, `Y` and `X`, containing the species scores for the response matrix `Y` and the predictor matrix `X` respectively.
#'
#' - **sites**: A list with two components, `Y` and `X`, containing the site scores for the response matrix `Y` and the predictor matrix `X` respectively.
#'
#' - **loadings**: A list with two components, `Y` and `X` containing the loadings for the response and predictor matrix. For [symcoca] only.
#'
#' - **xmatrix**: The X matrix. For [symcoca] only.
#' @references ter Braak, C.J.F and Schaffers, A.P. (2004) Co-Correspondence
#' Analysis: a new ordination method to relate two community
#' compositions. *Ecology* **85(3)**, 834--846
#' @author Gavin L. Simpson, based on Matlab code by C.J.F. ter Braak and
#' A.P. Schaffers.
#' @seealso [scores][vegan::scores], for further details on the method.
#' @keywords methods
#' @rdname scores.predcoca
#' @export
#' @examples
#' \dontshow{od <- options(digits = 4)}
#' ## load some data
#' data(beetles)
#' data(plants)
#'
#' ## log transform the bettle data
#' beetles <- log(beetles + 1)
#'
#' ## fit the model, a symmetric CoCA
#' bp.sym <- coca(beetles ~ ., data = plants, method = "symmetric")
#'
#' ## extract the scores
#' scr <- scores(bp.sym)
#'
#' ## predictive CoCA using SIMPLS and formula interface
#' bp.pred <- coca(beetles ~ ., data = plants)
#' scr2 <- scores(bp.pred)
#'
#' \dontshow{options(od)}
scores.predcoca <- function(
  x,
  choices = c(1, 2),
  display = c("sites", "species"),
  ...
) {
  if (!inherits(x, "predcoca")) {
    stop("x must be of class \"predcoca\"")
  }
  scoreOpts <- c("species", "sites")
  names(scoreOpts) <- c("species", "sites")
  take <- scoreOpts[display]
  retval <- list()
  retval$species <- if ("species" %in% take) {
    list(
      Y = x$scores$species$Y[, choices, drop = FALSE],
      X = x$scores$species$X[, choices, drop = FALSE]
    )
  }
  retval$sites <- if ("sites" %in% take) {
    list(
      Y = x$scores$site$Y[, choices, drop = FALSE],
      X = x$scores$site$X[, choices, drop = FALSE]
    )
  }
  ##class(retval) <- "scores.predcoca"
  retval
}
