`print.summary.predcoca` <- function(x, digits = max(3, getOption("digits") - 3), ...) {
    cat("\nPredictive Co-Correspondence Analysis\n\n")
    writeLines(strwrap(pasteCall(x$call)))
    lambda <- eigenvals(x)
    if (!is.null(lambda)) {
        cat("\nEigenvalues:\n")
        print.default(format(lambda, digits = digits), ..., print.gap = 3,
                      quote = FALSE)
    } else {
        pcentX <- (x$varianceExp$Xblock / x$totalVar$Xblock) * 100
        Xvar.mat <- rbind(pcentX, cumsum(pcentX))
        pcentY <- (x$varianceExp$Yblock / x$totalVar$Yblock) * 100
        Yvar.mat <- rbind(pcentY, cumsum(pcentY))
        rownames(Xvar.mat) <- rownames(Yvar.mat) <- c("Individual:", "Cumulative:")
        cat("\nPercentage Variance Explained:\n")
        cat("\nY-block: variance explained in", x$namY,
            "(response) \n", sep = " ")
        print(format(Yvar.mat, digits = digits), ..., print.gap = 2,
              quote = FALSE)
        cat("\nX-block: variance explained in", x$namX,
            "(predictor) \n", sep = " ")
        print(format(Xvar.mat, digits = digits), ..., print.gap = 2,
              quote = FALSE)
    }
    invisible(x)
}

