`eigenvals.coca` <- function(x, choices = NULL, ...) {
    eig <- x$lambda
    if (!is.null(choices)) {
        eig <- eig[choices]
    }
    eig
}
