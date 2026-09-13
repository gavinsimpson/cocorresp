#' @noRd
#' @export
`eigenvals.coca` <- function(x, choices = NULL, ...) {
    lambda(x, choices = choices)
}

#' @noRd
#' @export
`eigenvals.summary.predcoca` <- function(x, choices = NULL, ...) {
    lambda(x, choices = choices)
}

#' @noRd
#' @export
`eigenvals.summary.symcoca` <- function(x, choices = NULL, ...) {
    lambda(x, choices = choices)
}

#' @noRd
#' @export
`eigenvals.summary.fitCoinertia` <- function(x, choices = NULL, ...) {
    lambda(x, choices = choices)
}

#' @noRd
#' @export
`eigenvals.fitCoinertia` <- function(x, choices = NULL, ...) {
    lambda(x, choices = choices)
}

#' @noRd
`lambda` <- function(x, choices = NULL) {
    eig <- x[["lambda"]]
    if (!is.null(choices)) {
        eig <- eig[choices]
    }
    if (!is.null(eig)) {
        class(eig) <- c("eigenvals", "numeric")
    }
    eig
}
