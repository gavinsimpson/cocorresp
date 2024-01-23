`print.summary.symcoca` <- function(x, digits = max(3, getOption("digits") - 3), ...) {
    cat("\nSymmetric Co-Correspondence Analysis\n\n")
    writeLines(strwrap(pasteCall(x$call)))
    cat("\nInertia:\n")
    printCoefmat(x$inertia, digits = digits, na.print = "")
    cat("\nEigenvalues:\n")
    print(format(eigenvals(x), digits = digits), ..., print.gap = 2,
          quote = FALSE)
    invisible(x)
}
