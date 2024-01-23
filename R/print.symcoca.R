`print.symcoca` <- function(x, digits = max(3, getOption("digits") - 3),
                            ...) {
    cat("\nSymmetric Co-Correspondence Analysis\n\n")
    writeLines(strwrap(pasteCall(x$call)))
    cat("\nInertia:\n")
    inert <- rbind(unlist(x$inertia$total),
                   unlist(x$inertia$residual))
    explained <- inert[1,] - inert[2,]
    inert <- cbind(inert[1, ], explained, inert[2, ])
    colnames(inert) <- c("Total", "Explained", "Residual")
    rownames(inert) <- paste0(c(x$nam.dat$namY, x$nam.dat$namX), ":")
    printCoefmat(inert, digits = digits, na.print = "")
    cat("\n")
    invisible(x)
}
