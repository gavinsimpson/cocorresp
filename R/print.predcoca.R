`print.predcoca` <- function(x, digits = NULL, ...) {
    if (missing(digits) || is.null(digits)) {
        digits <- max(3, getOption("digits") - 3)
    }
    cat("\nPredictive Co-Correspondence Analysis\n\n")
    writeLines(strwrap(pasteCall(x$call)))
    cat("\n")
    writeLines(strwrap(paste("Co-CA Method:", x$method)))
    cat("\n")
    if (is.null(x$totalVar)) {
        writeLines("\nEigenvalues:\n\n")
        print(round(x$lambda, digits), ...)
    } else {
        totVar <- unlist(x$totalVar, use.names = FALSE)
        ##names(totVar) <- paste(unlist(x$nam.dat, use.names = FALSE), c("(predictor)", "(response)"))
        totVar <- data.frame(Role = c("Predictor", "Response"),
                             "Variance" = totVar,
                             check.names = FALSE, stringsAsFactors = FALSE)
        rownames(totVar) <- unlist(x$nam.dat, use.names = FALSE)
        print(totVar, digits = digits, ...)
    }
    invisible(x)
}
