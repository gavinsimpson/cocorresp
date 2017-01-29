"coca.default" <- function(y, x, method = c("predictive", "symmetric"),
                           reg.method = c("simpls", "eigen"),
                           weights = NULL,
                           n.axes = NULL,
                           symmetric = FALSE,
                           quiet = FALSE, ...) {
    nam.dat <- list(namY = deparse(substitute(y)),
                    namX = deparse(substitute(x)))
    y <- checkCommunityData(y)
    x <- checkCommunityData(x)
    dropped <- c(attr(y, "dropped"), attr(x, "dropped"))
    if (!quiet) {
        msgIfDroppedVars(dropped, nam.dat)
    }
    method <- match.arg(method)
    if(method == "predictive") {
        reg.method <- match.arg(reg.method)
        retval <- switch(reg.method,
                         simpls = predcoca.simpls(y, x, R0 = weights,
                         n.axes = n.axes, nam.dat),
                         eigen = predcoca.eigen(y, x, R0 = weights,
                         n.axes = n.axes, nam.dat))
    } else {
        retval <- symcoca(y, x, n.axes = n.axes, R0 = weights,
                          symmetric = symmetric, nam.dat)
    }
    class(retval) <- c("coca", class(retval))
    retval
}

`checkCommunityData` <- function(x) {
    dropped <- FALSE
    if(any(rowSums(x) <= 0 )) {
        stop("All row sums must be > 0 in data matrix")
    }
    if(any((csum <- colSums(x)) <= 0 )) {
        x <- x[, csum > 0, drop = FALSE]
        dropped <- TRUE
    }
    attr(x, "dropped") <- dropped
    x
}

`msgIfDroppedVars` <- function(x, objNames) {
    dropped <- vector(mode = "list", length = length(x))
    dropped[x] <- objNames
    ## print message if we dropped some species
    if (any(!sapply(dropped, is.null))) {
        message(paste(paste("\nRemoved some species that contained no data in:",
                            paste(unlist(dropped), collapse = ", ")), "\n"))
    }
    invisible()
}
