`corAxis` <- function(x, ...)
    UseMethod("corAxis")

`corAxis.default` <- function(x, ...)
    stop("No default method for corAxis")

`corAxis.symcoca` <- function(x, axes = NULL, ...) {
    if (!inherits(x, "symcoca")) {
        stop("object must be of class \"symcoca\"")
    }
    if (is.null(axes)) {
        axes <- seq_len(x$n.axes)
    }
    scrs <- scores(x, axes, display = "sites")
    diag(cor(scrs$sites$Y, scrs$sites$X))
}
