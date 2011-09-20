"scores.symcoca" <-
function(x, choices = c(1, 2), display = c("sites", "species"),
         scaling = 1, ...) {
    if (!inherits(x, "symcoca"))
        stop("x must be of class \"symcoca\"")
    opts <- c("species", "sites", "loadings", "xmatrix")
    names(opts) <- c("species", "sites", "loadings", "xmatrix")
    take <- opts[display]
    retval <- list()
    if ("species" %in% take) {
        retval$species <- if(scaling == 2) {
            rescale(x, choices)
        } else {
            list(Y = x$scores$species$Y[, choices, drop = FALSE],
                 X = x$scores$species$X[, choices, drop = FALSE])
        }
    }
    retval$site <- if ("sites" %in% take)
        list(Y = x$scores$site$Y[, choices, drop = FALSE],
             X = x$scores$site$X[, choices, drop = FALSE])
    retval$loadings <- if ("loadings" %in% take)
        list(Y = x$loadings$Y[, choices, drop = FALSE],
             X = x$loadings$X[ choices, drop = FALSE])
    retval$xmatrix <- if ("xmatrix" %in% take)
        x$X[, choices, drop = FALSE]
    ## if only one type of scores, return as a single list, not
    ## a nested one
    if (length(retval) == 1)
        retval <- retval[[1]]
    ##class(retval) <- "scores.symcoca"
    return(retval)
}
