"scores.predcoca" <-
function(x, choices = c(1,2), display = c("site", "species"), ...)
  {
    if (class(x) != "predcoca")
      stop("x must be of class \"predcoca\"")
    scoreOpts <- c("species", "site")
    names(scoreOpts) <- c("species", "site")
    take <- scoreOpts[display]
    retval <- list()
    retval$species <- if ("species" %in% take)
      list(U1 = x$scores$species$Y[, choices, drop = FALSE],
           U2 = x$scores$species$X[, choices, drop = FALSE])
    retval$site <- if ("site" %in% take)
      list(X1 = x$scores$site$Y[, choices, drop = FALSE],
           X2 = x$scores$site$X[, choices, drop = FALSE])
    class(retval) <- "scores.predcoca"
    return(retval)
  }
