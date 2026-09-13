#' @rdname coca
#' @export
coca.default <- function(
  y,
  x,
  method = c("predictive", "symmetric"),
  reg.method = c("simpls", "eigen"),
  weights = NULL,
  n.axes = NULL,
  symmetric = FALSE,
  quiet = FALSE,
  ...
) {
  nam.dat <- list(namY = deparse(substitute(y)), namX = deparse(substitute(x)))
  fit_coca(y, x, method, reg.method, weights, n.axes, symmetric, quiet, nam.dat)
}

#' @noRd
checkCommunityData <- function(x) {
  if (!is.matrix(x) && !is.data.frame(x)) {
    stop("Community data must be a numeric matrix or data frame")
  }
  x <- as.matrix(x)
  if (!is.numeric(x) || !all(is.finite(x)) || any(x < 0)) {
    stop("Community data must contain finite, non-negative numeric values")
  }
  if (nrow(x) < 2L || ncol(x) < 2L) {
    stop("Community data must have at least two rows and columns")
  }
  if (any(rowSums(x) <= 0)) {
    stop("All row sums must be > 0 in data matrix")
  }
  keep <- colSums(x) > 0
  x <- x[, keep, drop = FALSE]
  if (ncol(x) < 2L) {
    stop("At least two non-empty species are required")
  }
  attr(x, "dropped") <- !all(keep)
  x
}

#' @noRd
msgIfDroppedVars <- function(x, objNames) {
  if (any(x)) {
    message(
      "Removed some species that contained no data in: ",
      paste(unlist(objNames)[x], collapse = ", ")
    )
  }
  invisible(NULL)
}

#' @noRd
fit_coca <- function(
  y,
  x,
  method,
  reg.method,
  weights,
  n.axes,
  symmetric,
  quiet,
  nam.dat
) {
  y <- checkCommunityData(y)
  x <- checkCommunityData(x)
  validate_pair(y, x, weights, n.axes)
  if (!quiet) {
    msgIfDroppedVars(c(attr(y, "dropped"), attr(x, "dropped")), nam.dat)
  }
  method <- match.arg(method, c("predictive", "symmetric"))
  if (method == "predictive") {
    reg.method <- match.arg(reg.method, c("simpls", "eigen"))
    if (reg.method == "simpls") {
      predcoca.simpls(y, x, R0 = weights, n.axes = n.axes, nam.dat)
    } else {
      predcoca.eigen(y, x, R0 = weights, n.axes = n.axes, nam.dat)
    }
  } else {
    symcoca(y, x, n.axes = n.axes, R0 = weights, symmetric = symmetric, nam.dat)
  }
}

#' @noRd
validate_pair <- function(y, x, weights = NULL, n.axes = NULL) {
  if (nrow(y) != nrow(x)) {
    stop("Number of rows in y and x is not equal")
  }
  if (
    !is.null(weights) &&
      (!is.numeric(weights) ||
        length(weights) != nrow(y) ||
        !all(is.finite(weights)) ||
        any(weights <= 0))
  ) {
    stop("weights must contain one finite positive value per row")
  }
  if (!is.null(n.axes)) {
    validate_count(n.axes, "n.axes")
  }
  invisible(NULL)
}

#' @noRd
validate_count <- function(x, name) {
  if (
    !is.numeric(x) ||
      length(x) != 1L ||
      !is.finite(x) ||
      x < 1 ||
      x != floor(x) ||
      x > .Machine$integer.max
  ) {
    stop(name, " must be a positive integer")
  }
  invisible(NULL)
}
