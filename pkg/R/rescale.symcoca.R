"rescale.symcoca" <-
function(object, axes = c(1:object$n.axes), ...)
  {
    col.nam <- colnames(object$scores$species$Y)[axes]
    lambda4 <- diag(sqrt(sqrt(object$lambda[axes])),
                    nrow = length(axes),
                    ncol = length(axes))
    U1.scale <- object$scores$species$Y[, axes, drop = FALSE] %*% lambda4
    U2.scale <- object$scores$species$X[, axes, drop = FALSE] %*% lambda4
    colnames(U1.scale) <- colnames(U2.scale) <- col.nam
    retval <- list(U1 = U1.scale, U2 = U2.scale)
    return(retval)
  }

