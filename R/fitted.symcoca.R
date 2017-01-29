"fitted.symcoca" <- function(object, which = c("y1","y2"), ...) {
    `getFitted` <- function(x, take) {
        TAKE <- ifelse(take == "Y", 1L, 2L)
        rsum <- x$rowsum[[TAKE]]
        csum <- x$colsum[[TAKE]]
        tot <- sum(rsum)
        exp <- rsum %*% t(csum) / tot
        Yhat <- exp * (1 + (x$scores$site[[take]] %*%
                            t(x$scores$species[[take]])))
        rownames(Yhat) <- rownames(x$scores$site[[take]])
        Yhat
    }
    which <- selectWhich(which)
    out <- list()
    nam <- character(0)
    if ("Y" %in% which) {
        out$Y <- getFitted(object, take = "Y")
        nam <- c(nam, object$nam.dat$namY)
    }
    if ("X" %in% which) {
        out$X <- getFitted(object, take = "X")
        nam <- c(nam, object$nam.dat$namX)
    }
    out$nam.dat <- nam
    names(out$nam.dat) <- c("namY","namX")[c("Y","X") %in% which]
    class(out) <- "fitted.symcoca"
    out
}

