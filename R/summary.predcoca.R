"summary.predcoca" <- function(object, axes = NULL, ...) {
    if (missing(axes) || is.null(axes)) {
        axes <- seq_len(min(6, object$n.axes))
    }
    cocaScores <- scores(object, choices = axes, ...)
    retval <- list(cocaScores = cocaScores, call = object$call,
                   lambda = object$lambda, namY = object$nam.dat$namY,
                   namX = object$nam.dat$namX,
                   loadings = lapply(object$loadings, `[`, , axes),
                   varianceExp = object$varianceExp,
                   totalVar = object$totalVar)
    class(retval) <- "summary.predcoca"
    retval
}

