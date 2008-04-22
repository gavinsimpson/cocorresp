"scores.symcoca" <-
function(x, choices = c(1, 2), display = c("site", "species"),
         scaling = 1, ...)
  {
    if (class(x) != "symcoca")
      stop("x must be of class \"symcoca\"")
    scoreOpts <- c("species", "site", "loadings", "xmatrix")
    names(scoreOpts) <- c("species", "site", "loadings", "xmatrix")
    take <- scoreOpts[display]
    retval <- list()
    if ("species" %in% take) {
        retval$species <- if(scaling == 2)
          rescale(x, choices)
        else {
          list(U1 = x$scores$species$Y[, choices, drop = FALSE],
               U2 = x$scores$species$X[, choices, drop = FALSE])
        }
      }
    retval$site <- if ("site" %in% take)
      list(X1 = x$scores$site$Y[, choices, drop = FALSE],
           X2 = x$scores$site$X[, choices, drop = FALSE])
    retval$loadings <- if ("loadings" %in% take)
      list(Y1 = x$loadings$Y[, choices, drop = FALSE],
           Y2 = x$loadings$X[ choices, drop = FALSE])
    retval$xmatrix <- if ("xmatrix" %in% take)
      x$X[, choices, drop = FALSE]
    if (length(retval) == 1) 
      retval <- retval[[1]]
    class(retval) <- "scores.symcoca"
    return(retval)
  }

