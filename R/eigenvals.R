`eigenvals.coca` <- function(x, choices = NULL, ...) {
    lambda(x, choices = choices)
}

`eigenvals.summary.predcoca` <- function(x, choices = NULL, ...) {
    lambda(x, choices = choices)
}

`eigenvals.summary.symcoca` <- function(x, choices = NULL, ...) {
    lambda(x, choices = choices)
}

`eigenvals.summary.fitCoinertia` <- function(x, choices = NULL, ...) {
    lambda(x, choices = choices)
}

`eigenvals.fitCoinertia` <- function(x, choices = NULL, ...) {
    lambda(x, choices = choices)
}

`lambda` <- function(x, choices = NULL) {
    eig <- x[["lambda"]]
    if (!is.null(choices)) {
        eig <- eig[choices]
    }
    eig
}
