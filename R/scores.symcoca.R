`scores.symcoca` <- function(x, choices = c(1, 2),
                             display = c("sites", "species"),
                             scaling = FALSE, ...) {
    if (!inherits(x, "symcoca"))
        stop("x must be of class \"symcoca\"")
    opts <- c("species", "sites", "loadings", "xmatrix")
    names(opts) <- c("species", "sites", "loadings", "xmatrix")
    take <- opts[display]
    retval <- list()
    if ("species" %in% take) {
        retval$species <- if(scaling) {
            rescale(x, choices = choices, display = "species")
        } else {
            list(Y = x$scores$species$Y[, choices, drop = FALSE],
                 X = x$scores$species$X[, choices, drop = FALSE])
        }
    }
    if ("sites" %in% take) {
        retval$sites <- if(scaling) {
                            rescale(x, choices = choices, display = "sites")
        } else {
            list(Y = x$scores$site$Y[, choices, drop = FALSE],
                 X = x$scores$site$X[, choices, drop = FALSE])
        }
    }
    retval$loadings <- if ("loadings" %in% take)
        list(Y = x$loadings$Y[, choices, drop = FALSE],
             X = x$loadings$X[ choices, drop = FALSE])
    retval$xmatrix <- if ("xmatrix" %in% take)
        x$X[, choices, drop = FALSE]
    retval
}
