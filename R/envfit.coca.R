#' Fit an environmental vector or factor to a Co-CA ordination
#'
#' The function fits environmental vectors or factors to a Co-CA
#' ordination. The projections of points onto vectors have maximum
#' correlation with corresponding environmental variables, and the
#' factors show the averages of factor levels.
#' @details See [envfit][vegan::envfit] for details of the method.
#' @param ord a Co-CA ordination object, the result of a call to
#' [coca].
#' @param env a data frame, matrix or vector of environmental/external
#' variable(s) to be fitted to the ordination. The variables may be
#' of a mixed type (factors and continuous variables) in a data frame.
#' @param which character; which of the response or predictor ordinations
#' should be used during fitting of vectors and factors.
#' @param choices numeric; the axes to which vectors and factors are
#' fitted.
#' @param scaling logical; should scaling be applied. See
#' [scores.symcoca].
#' @param w weights used in fitting vectors and factors.
#' @param na.rm Remove points with missing values in ordination scores or
#' environmental variables. The operation is case-wise; the whole row of
#' data is removed if there is a missing value and `na.rm =
#' TRUE`.
#' @param strata An integer vector or factor specifying the strata for
#' permutation. If supplied, observations are permuted only within the
#' specified strata.
#' @param permutations Number of permutations for assessing significance
#' of vectors or factors. Set to `0` to skip permutations.
#' @param ... Arguments passed to [vectorfit][vegan::vectorfit] and
#' [factorfit][vegan::factorfit].
#' @returns Returns an object of class [envfit][vegan::envfit].
#' @author Gavin L. Simpson. The code interfaces with and uses code from
#' [envfit][vegan::envfit] for the main computations, which was written by
#' Jari Oksanen.
#' @seealso [coca] for fitting models. [envfit][vegan::envfit] for
#' details of the generic function and the computations performed.
#' @keywords htest
#' @section Warning:
#' %   This function is experimental. The vectors fitted are unweighted
#' %   whereas to be consistent with the [cca] method of
#' %   [envfit] they should be weighted. I just need to work out
#' %   the correct sets of weights to use.
#' %
#' @rdname envfit.coca
#' @name envfit.coca
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
#' ## load the environmental data
#' data(verges)
#'
#' ## fit vectors for the environmental data
#' sol <- envfit(bp.sym, verges, which = "response")
#' \donttest{sol}
#'
#' ## plot the response matrix and the fitted vectors
#' biplot(bp.sym, which = "y1")
#' plot(sol)
envfit.coca <- function(
  ord,
  env,
  which = c("response", "predictor"),
  choices = c(1, 2),
  scaling = FALSE,
  w,
  na.rm = FALSE,
  strata = NULL,
  permutations = 999,
  ...
) {
  if (missing(w)) {
    w <- weights(ord)
  }
  which <- match.arg(which)
  block <- if (which == "response") "Y" else "X"
  X <- scores(
    ord,
    display = "sites",
    choices = choices,
    scaling = scaling
  )$sites[[block]]
  if (is.null(dim(env))) {
    env <- data.frame(env = env)
  }
  if (nrow(env) != nrow(X)) {
    stop("env and ord must have the same number of rows")
  }
  if (length(w) == 1L) {
    w <- rep(w, nrow(X))
  }
  if (
    !is.null(w) && (length(w) != nrow(X) || !all(is.finite(w)) || any(w < 0))
  ) {
    stop("w must contain one finite non-negative weight per row")
  }
  if (!is.null(strata) && length(strata) != nrow(X)) {
    stop("strata must match the data rows")
  }
  keep <- complete.cases(X) & complete.cases(env)
  omitted <- NULL
  if (!all(keep)) {
    if (!na.rm) {
      stop("missing values in data: consider na.rm = TRUE")
    }
    permutations <- subset_permutations(permutations, keep)
    X <- X[keep, , drop = FALSE]
    env <- env[keep, , drop = FALSE]
    if (is.data.frame(env)) {
      env <- droplevels(env)
    }
    if (!is.null(w)) {
      w <- w[keep]
    }
    if (!is.null(strata)) {
      strata <- strata[keep]
    }
    omitted <- structure(which(!keep), class = "omit")
  }
  if (nrow(X) < 2L) {
    stop("At least two complete observations are required")
  }
  if (is.null(w)) {
    w <- rep(1, nrow(X))
  }
  if (sum(w) <= 0) {
    stop("w must have positive total weight")
  }
  if (is.numeric(permutations) && is.null(dim(permutations))) {
    if (identical(as.double(permutations), 0)) {
      permat <- matrix(integer(), 0L, nrow(X))
    } else {
      validate_count(permutations, "permutations")
      control <- permute::how(nperm = permutations, blocks = strata)
      permat <- permute::shuffleSet(nrow(X), control = control)
    }
  } else {
    permat <- permutation_matrix(permutations, nrow(X))
  }
  vectors <- factors <- NULL
  numeric_vars <- if (is.data.frame(env)) {
    vapply(env, is.numeric, logical(1))
  } else {
    rep(TRUE, ncol(env))
  }
  if (any(numeric_vars)) {
    vectors <- vegan::vectorfit(
      X,
      env[, numeric_vars, drop = FALSE],
      permutations = permat,
      w = w,
      ...
    )
  }
  if (!all(numeric_vars)) {
    factors <- vegan::factorfit(
      X,
      env[, !numeric_vars, drop = FALSE],
      permutations = permat,
      w = w,
      ...
    )
  }
  sol <- list(vectors = vectors, factors = factors)
  if (!is.null(omitted)) {
    sol$na.action <- omitted
  }
  class(sol) <- "envfit"
  sol
}

#' @noRd
subset_permutations <- function(permutations, keep) {
  if (is.matrix(permutations)) {
    permutation_matrix(permutations, length(keep))
    reduced <- permutations[, keep, drop = FALSE]
    if (!all(keep[reduced])) {
      stop(
        "Supplied permutations move omitted observations; supply complete-case data and permutations"
      )
    }
    return(matrix(match(reduced, which(keep)), nrow = nrow(reduced)))
  }
  if (inherits(permutations, "how")) {
    if (
      permute::getType(permute::getWithin(permutations)) != "free" ||
        permute::getType(permute::getPlots(permutations)) != "none"
    ) {
      stop(
        "For structured designs, supply complete-case data and a matching permutation control"
      )
    }
    blocks <- permute::getBlocks(permutations)
    if (!is.null(blocks)) {
      if (length(blocks) != length(keep)) {
        stop("Permutation blocks must match data rows")
      }
      permute::setBlocks(permutations) <- blocks[keep]
    }
  }
  permutations
}
