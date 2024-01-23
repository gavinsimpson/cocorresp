"print.fitted.symcoca" <- function(x, digits = max(3, getOption("digits") - 3),
                                   ...) {
    if (!is.null(x[["Y"]])) {
        cat("\nFitted values for:", x$nam.dat["namY"], "\n")
        print(zapsmall(x[["Y"]]), digits = digits, ..., print.gap = 2)
    }
    if (!is.null(x[["X"]])) {
        cat("\nFitted values for:", x$nam.dat["namX"], "\n")
        print(zapsmall(x[["X"]]), digits = digits, ..., print.gap = 2)
    }
    invisible(x)
}
