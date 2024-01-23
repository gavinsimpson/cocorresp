`summary.symcoca` <- function(object, ...) {
    inert <- rbind(unlist(object$inertia$total),
                   unlist(object$inertia$residual))
    explained <- inert[1,] - inert[2,]
    inert <- cbind(inert[1, ], explained, inert[2, ])
    colnames(inert) <- c("Total", "Explained", "Residual")
    rownames(inert) <- paste0(c(object$nam.dat$namY, object$nam.dat$namX),
                              ":")
    retval <- list(inertia = inert, lambda = eigenvals(object),
                   call = object$call)
    class(retval) <- "summary.symcoca"
    retval
}

