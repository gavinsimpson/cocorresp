`rescale.symcoca` <- function(object, choices = NULL,
                              display = c("species", "sites"), ...) {
    if (is.null(choices)) {
        choices <- seq_len(object$n.axes)
    }
    display <- match.arg(display, several.ok = TRUE)
    lambda4 <- diag(sqrt(sqrt(eigenvals(object, choices = choices))),
                    nrow = length(choices),
                    ncol = length(choices))
    out <- list()
    if ("species" %in% display) {
        out$species <-
            list(Y = object$scores$species$Y[, choices, drop = FALSE] %*% lambda4,
                 X = object$scores$species$X[, choices, drop = FALSE] %*% lambda4)
    }
    if ("sites" %in% display) {
        out$sites <-
            list(Y = object$scores$site$Y[, choices, drop = FALSE] %*% lambda4,
                 X = object$scores$site$X[, choices, drop = FALSE] %*% lambda4)
    }
    if (length(out) == 1L) {
        out <- out[[1L]]
    }
    out
}

