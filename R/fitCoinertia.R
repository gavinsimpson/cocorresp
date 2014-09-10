`fitCoinertia` <- function(X, Dp, Y, Dq, Dn, n.axes) {
    ax.names <- paste("COCA", 1:n.axes, sep = " ")
    Dp05 <- diag(sqrt(Dp))
    Dq05 <- diag(sqrt(Dq))
    A <- Dp05 %*% t(X) %*% diag(Dn) %*% Y %*% Dq05
    svdA <- La.svd(A)
    U <- diag(1 / sqrt(Dp)) %*% svdA$u
    V <- diag(1 / sqrt(Dq)) %*% t(svdA$vt)
    Ksi <- X %*% diag(Dp) %*% U
    Psi <- Y %*% diag(Dq) %*% V
    L <- diag(svdA$d)
    L <- L * L
    U1 <- U[, 1:n.axes, drop = FALSE]
    U2 <- V[, 1:n.axes, drop = FALSE]
    colnames(U1) <- colnames(U2) <- ax.names
    rownames(U1) <- colnames(X)
    rownames(U2) <- colnames(Y)
    X1 <- Ksi[, 1:n.axes, drop = FALSE]
    X2 <- Psi[, 1:n.axes, drop = FALSE]
    colnames(X1) <- colnames(X2) <- ax.names
    lambda <- diag(L[1:n.axes, 1:n.axes, drop = FALSE])
    names(lambda) <- ax.names
    retval <- list(scores = list(species = list(Y = U1, X= U2),
                   site = list(Y = X1, X = X2)),
                   lambda = lambda, n.axes = n.axes, call = match.call())
    class(retval) <- "fitCoinertia"
    return(retval)
}

