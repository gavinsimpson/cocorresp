#' Permutation test for predictive co-correspondence analysis
#' models
#'
#' A permutation test for predictive co-correspondence analysis models to
#' assess the significance of each CoCA ordination axes.
#' @details An alternative approach to cross-validation (see
#' [crossval]) to select the number of axes to retain in a
#' predictive co-correspondence analysis is to test the statistical
#' significance of each ordination axis using permutation tests.
#'
#' The test statistic used is the *F*-ratio based on the fit of the
#' first axis to the response data (ter Braak and Smilauer 2002). The
#' second and subsequent axes are tested by treating previous axes as
#' co-variables.
#'
#' To be precise, this approach does not test the significance of SIMPLS
#' axes, but those of NIPALS-PLS axes (ter Braak and de Jong 1998).
#'
#' `permutations` can also be a [permute::how()] control object or a
#' numeric matrix whose rows are permutations of `seq_len(nrow(x$Ychi$Ychi1))`.
#' Matrix rows select residual rows directly and are reused for each axis.
#' Controls generate a matrix once and use the actual number returned.
#' Integer counts retain historical sampling and inverse-assignment semantics,
#' with fresh permutations for each axis. Permutations are generated in the
#' calling process, so worker count and scheduling do not change the RNG stream.
#' Replicates run in parallel; dependent axes remain sequential.
#' `R0` is retained for compatibility and ignored; weights are stored in `x`.
#' @param x an object of class `"predcoca"`.
#' @param R0 row weights to use in the analysis. If missing, the
#' default, these are determined from `x`.
#' @param permutations the number of permutations to perform.
#' @param n.axes The number of axes to test. Defaults to the number of
#' axes stated in `x$n.axes`.
#' @param verbose if `TRUE`, the default, print information on the
#' progress of the permutation test procedure.
#' @param object an object of class `"permutest.coca"`.
#' @param ... arguments to be passed to other methods.
#' @returns A list with the following components:
#'
#' - **pval**: a vector of *P*-values for each ordination axis.
#'
#' - **permstat**: a vector of values for the test statistic for each axis.
#'
#' - **total.inertia**: the total inertia in the response matrix.
#'
#' - **inertia**: a vector containing the *residualised* inertia. This is the total inertia in the response *after* removing the inertia explained by all previous axes. For the first CoCA axis this is, by definition, the total inertia in the response.
#'
#' - **fitax**: a vector containing the amount of inertia in the response matrix explained by each ordination axis.
#'
#' - **pcent.fit**: a vector containing the fit of each axis to the response as a percentage of the total inertia (variance).
#'
#' - **n.axes**: the number of axes in the ordination.
#'
#' - **call**: the matched call.
#' @references ter Braak, C.J.F. and de Jong, S. (1998) The objective function of
#' partial least squares regression. *Journal of Chemometrics*
#' **12**, 41--54.
#'
#' ter Braak, C.J.F and Schaffers, A.P. (2004) Co-Correspondence
#' Analysis: a new ordination method to relate two community
#' compositions. *Ecology* **85(3)**, 834--846.
#'
#' ter Braak, C.J.F. and Smilauer, P. (2002) *Canoco reference manual
#' and CanoDraw for Windows user's guide: software for canonical
#' community ordination. Version 4.5*. New York: Microcomputer Power.
#' @author Gavin L. Simpson, based on Matlab code by C.J.F. ter Braak and
#' A.P. Schaffers.
#' @note Argument `R0` is provided for compatibility with the original
#' MATLAB code. The R usage paradigm makes this argument redundant in the
#' current code and it may be invalid to supply different row weights
#' (\eqn{R_0}) as `R0`. This argument will likely be removed in future
#' versions.
#' @seealso [coca], for the model fitting function,
#' [crossval], for a leave-one-out cross-validation
#' procedure, which is the preferred way to select axes in a predictive
#' co-correspondence analysis.
#' @keywords multivariate
#' @section Warning:
#' This function is **slow**. Beware setting argument
#' `permutations` higher than the default. Determine how long it
#' takes for the default 99 permutations to complete before going crazy
#' and asking for thousands of permutations - you've been warned, have a
#' good book to hand.
#' @rdname permutest.coca
#' @export
#' @inheritParams crossval
#' @examples
#' \dontshow{
#' old_rng <- RNGkind()
#' suppressWarnings(RNGversion("3.5.0"))
#' od <- options(digits = 4)
#' }
#' ## load some data
#' data(beetles)
#' data(plants)
#'
#' ## log transform the bettle data
#' beetles <- log(beetles + 1)
#' ## predictive CoCA using SIMPLS and formula interface
#' bp.pred <- coca(beetles ~ ., data = plants)
#'
#' ## should retain only the useful PLS components for a parsimonious model
#' \donttest{
#' ## Leave-one-out crossvalidation - this takes a while
#' crossval(beetles, plants)
#' ## so 2 axes are sufficient
#' }
#'
#' ## permutation test
#' ## (Testing the first 2 axes & only 25 perms for speed.)
#' bp.perm <- permutest(bp.pred, permutations = 25, n.axes = 2)
#' bp.perm
#'
#' \dontshow{options(od); do.call(RNGkind, as.list(old_rng))}
permutest.coca <- function(
  x,
  R0 = NULL,
  permutations = 99,
  n.axes = x$n.axes,
  verbose = TRUE,
  ...,
  parallel = 1L
) {
  if (!inherits(x, "predcoca")) {
    stop("x must be of class 'predcoca'")
  }
  validate_count(n.axes, "n.axes")
  mapper <- make_mapper(parallel)
  on.exit(mapper$close(), add = TRUE)
  design <- if (is.numeric(permutations) && is.null(dim(permutations))) {
    validate_count(permutations, "permutations")
    NULL
  } else {
    permutation_matrix(permutations, nrow(x$Ychi$Ychi1))
  }
  Ychi1 <- x$Ychi$Ychi1
  Ychi2 <- x$Ychi$Ychi2
  if (n.axes > x$n.axes) {
    n.axes <- x$n.axes
    warning("n.axes too large, reset to x$n.axes.")
  }
  pval <- permstat <- inertia <- fitax <- numeric(n.axes)
  for (j in seq_len(n.axes)) {
    if (verbose) {
      message("Permutations for axis: ", j)
    }
    if (j == 1) {
      covar <- NULL
    }
    ptest <- tryCatch(
      permutation_axis(
        Ychi1,
        Ychi2,
        covar,
        permutations,
        j,
        design,
        mapper$map
      ),
      error = function(e) {
        stop("Axis ", j, ": ", conditionMessage(e), call. = FALSE)
      }
    )
    permstat[j] <- ptest$stati[1]
    pval[j] <- ptest$pval
    Psi <- coinertiaI(X = Ychi1, Y = Ychi2, fast = TRUE)[, 1, drop = FALSE]
    res.mat1 <- residual_matrix(Ychi1, Psi)
    Ychi1 <- res.mat1$Yr
    if (j == 1) {
      total.inertia1 <- res.mat1$inertia$total
    }
    res.mat2 <- residual_matrix(Ychi2, Psi)
    Ychi2 <- res.mat2$Yr
    if (is.null(covar)) {
      covar <- cbind(NULL, Psi)
    } else {
      covar <- cbind(covar, Psi)
    }
    inertia[j] <- res.mat1$inertia$total
    fitax[j] <- res.mat1$inertia$fitted
    if (verbose) {
      message("Axis ", j, " completed")
    }
  }
  pcent.fit <- 100 * fitax / total.inertia1
  retval <- list(
    pval = pval,
    permstat = permstat,
    total.inertia = total.inertia1,
    inertia = inertia,
    fitax = fitax,
    pcent.fit = pcent.fit,
    n.axes = n.axes,
    ##Ychi1 = Ychi1, Ychi2 = Ychi2, stati.Ax1 = stati.Ax1
    call = match.call()
  )
  class(retval) <- "permutest.coca"
  retval
}


#' @noRd
permutation_axis <- function(Y, X1, X0, permutations, step, design, map) {
  if (nrow(Y) != nrow(X1)) {
    stop("Matrix X1 must have the same number of rows as Y")
  }
  mu <- if (is.null(X0)) 0 else X0 %*% qr.coef(qr(X0), Y)
  E <- Y - mu
  observed <- permutation_stat(E, X0, X1, step)
  indices <- if (is.null(design)) {
    # Historical code assigns E into sampled rows, i.e. inverse permutation.
    lapply(seq_len(permutations), function(i) order(sample(nrow(Y))))
  } else {
    lapply(seq_len(nrow(design)), function(i) design[i, ])
  }
  values <- map(
    seq_along(indices),
    permutation_replica,
    indices = indices,
    E = E,
    X0 = X0,
    X1 = X1,
    step = step
  )
  stati <- c(observed, unlist(values, use.names = FALSE))
  if (!all(is.finite(stati))) {
    stop("Permutation statistic is undefined for these data")
  }
  list(pval = sum(stati >= observed) / length(stati), stati = stati)
}

#' @noRd
permutation_replica <- function(i, indices, E, X0, X1, step) {
  permutation_stat(E[indices[[i]], , drop = FALSE], X0, X1, step)
}

#' @noRd
permutation_stat <- function(Y, X0, X1, step) {
  Psi <- coinertiaI(X = Y, Y = X1, fast = TRUE)
  fit0 <- if (is.null(X0)) 0 else residual_matrix(Y, X0)$inertia$fitted
  combined <- cbind(X0, Psi[, 1, drop = FALSE])
  inertia <- residual_matrix(Y, combined)$inertia
  (inertia$fitted - fit0) / (inertia$total - inertia$fitted)
}

#' @noRd
residual_matrix <- function(Y, X) {
  if (nrow(Y) != nrow(X)) {
    stop("Matrices must have the same number of rows")
  }
  fitted <- X %*% qr.coef(qr(X), Y)
  residual <- Y - fitted
  inertia <- list(
    total = sum(Y^2),
    residual = sum(residual^2),
    fitted = sum(fitted^2)
  )
  difference <- inertia$fitted + inertia$residual - inertia$total
  if (is.finite(difference) && abs(difference) > 1e-6) {
    warning(
      "Residual inertia + fitted inertia did not equal total inertia.",
      call. = FALSE
    )
  }
  list(inertia = inertia, Yr = residual, zerosum = difference)
}

#' @noRd
permutation_matrix <- function(permutations, n) {
  if (inherits(permutations, "how")) {
    permutations <- permute::shuffleSet(n, control = permutations)
  }
  if (
    !is.matrix(permutations) ||
      !is.numeric(permutations) ||
      nrow(permutations) < 1L ||
      ncol(permutations) != n ||
      !all(is.finite(permutations)) ||
      !all(apply(permutations, 1L, function(x) {
        identical(sort(as.double(x)), as.double(seq_len(n)))
      }))
  ) {
    stop(
      "permutations must be a control object or a matrix of row permutations"
    )
  }
  permutations
}
