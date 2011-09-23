`plot.symcoca` <- function(x,
                           which = c("response", "predictor"),
                           choices = 1:2,
                           display = c("species", "sites"),
                           scaling = FALSE,
                           type,
                           xlim = NULL,
                           ylim = NULL,
                           main = "", sub = "",
                           ylab, xlab,
                           ann = par("ann"),
                           axes = TRUE,
                           ...) {
    ## process the scores to display
    if(missing(display))
        display <-  c("species", "sites")
    display <- match.arg(display, several.ok = TRUE)
    ## what are we plotting, response or predictor?
    which <- match.arg(which)
    ## and map to X and Y for extraction
    WHICH <- ifelse(which == "response", "Y", "X")
    ## should the scores be rescaled - only for species though
    if(is.logical(scaling))
        scaling <- ifelse(scaling, 2, 1)
    ## need two and only two axes to plot
    if(length(choices) != 2)
        stop("Exactly two axes should be specified in `choices`")
    ## extract the scores
    scrs <- scores(x, choices = choices, display = display,
                   scaling = scaling)
    ## then extract the response or predictor scores
    scrs <- lapply(scrs, `[[`, WHICH)
    ## what type of plot?
    TYPES <- c("text", "points", "none")
    if (missing(type)) { ## work out whether to plot by text or points
        nitlimit <- 80
        nit <- max(nrow(scrs$species), nrow(scrs$sites))
        type <- if (nit > nitlimit)
            "points"
        else "text"
    } else type <- match.arg(type, TYPES)
    ## compute xy coords for each set of scores
    xy <- lapply(scrs, xy.coords)
    ## process axis limits if non supplied
    if (is.null(xlim))
        xlim <- range(sapply(xy, function(x) range(x$x[is.finite(x$x)])))
    if (is.null(ylim))
        ylim <- range(sapply(xy, function(x) range(x$y[is.finite(x$y)])))
    ## process x/y labels
    if(missing(xlab)) {
        xlabs <- sapply(xy, `[[`, "xlab")
        xlab <- xlabs[!is.null(xlabs)][1]
        eigx <- round(x$lambda[choices[1]], 4)
        xlab <- bquote(.(xlab) ~~ (lambda[.(choices[1])] == .(eigx)))
    }
    if(missing(ylab)) {
        ylabs <- sapply(xy, `[[`, "ylab")
        ylab <- ylabs[!is.null(ylabs)][1]
        eigy <- round(x$lambda[choices[2]], 4)
        ylab <- bquote(.(ylab) ~~ (lambda[.(choices[2])] == .(eigy)))
    }
    #opar <- par(no.readonly=TRUE)
    #on.exit(par(opar))
    ## plotting
    plot.new()
    plot.window(xlim, ylim, ...)
    if(!is.null(scrs$species)) {
        if(type == "text") {
            text(scrs$species, rownames(scrs$species), col = "red",
                 cex = 0.7, ...)
        }
        if(type == "points"){
            points(scrs$species, col = "red", pch = 3, cex = 0.7, ...)
        }
    }
    if(!is.null(scrs$sites)) {
        if(type == "text") {
            text(scrs$sites, rownames(scrs$sites), col = "black",
                 cex = 0.7, ...)
        }
        if(type == "points"){
            points(scrs$sites, col = "black", pch = 1, cex = 0.7, ...)
        }
    }
    if (axes) {
        axis(1)
        axis(2)
        box()
    }
    if(ann)
        title(main = main, sub = sub, xlab = xlab, ylab = ylab, ...)
    class(scrs) <- "ordiplot"
    invisible(scrs)
}

## "plot.symcoca" <-function(x,
##                           choices= c(1:2),
##                           display = c("species", "sites"),
##                           scaling = FALSE,
##                           oneFig = TRUE,
##                           type = c("points", "text", "none"),
##                           ask = prod(par("mfcol")) < 2 && dev.interactive(),
##                           cex = c(0.7, 0.7),
##                           pch = c(par("pch"), 3),
##                           col = c("black", "red"),
##                           ylab, xlab,
##                           main, sub = "",
##                           axes = TRUE,
##                           ann = par("ann"),
##                           ...) {
##     if(!inherits(x, "symcoca"))
##         stop("use only with \"symcoca\" objects")
##     ## needs a check on choices length == 2,
##     ## and max(choices) =< x$n.axes && any(choices > 0)
##     ##choice.len <- length(choices)
##     if(length(choices) != 2)
##         stop("choices must be of length 2 only")
##     if(max(choices) > x$n.axes)
##         stop("axis choice > number of axes in model")
##     if(any(choices < 1))
##         stop("choices must be greater than 0")
##     ## check to see what has been entered for display
##     display <- match.arg(display, several.ok = TRUE)
##     ## partition the plotting region if using only one figure
##     if(oneFig) {
##         opar <- par(mfrow = c(1, 2), no.readonly = TRUE)
##         on.exit(par(opar))
##     } else {
##         opar <- par(ask = TRUE, no.readonly = TRUE)
##         on.exit(par(opar))
##     }
##     ## FIXME - scaling is integer in scores.symcoca but I think
##     ## it is better if it is logical. He we set scaling == 2
##     ## is scaling == TRUE
##     if(scaling)
##         scaling <- 2
##     ## this is for the type of plot
##     type <- match.arg(type)
##     plt.dat <- scores(x, choices = choices, scaling = scaling,
##                       display = display)
##     ##if (is.null(plt.dat$species) && is.null(plt.dat$sites))
##     ##    plt.dat <- list(default = plt.dat)
##     if("species" %in% display) {
##         if("sites" %in% display) {
##             ranx1 <- range(plt.dat$species$Y[,1], plt.dat$sites$Y[,1])
##             rany1 <- range(plt.dat$species$Y[,2], plt.dat$sites$Y[,2])
##             ranx2 <- range(plt.dat$species$X[,1], plt.dat$sites$X[,1])
##             rany2 <- range(plt.dat$species$X[,2], plt.dat$sites$X[,2])
##         } else {
##             ranx1 <- range(plt.dat$species$Y[,1])
##             rany1 <- range(plt.dat$species$Y[,2])
##             ranx2 <- range(plt.dat$species$X[,1])
##             rany2 <- range(plt.dat$species$X[,2])
##         }
##     } else {
##         ranx1 <- range(plt.dat$sites$Y[,1])
##         rany1 <- range(plt.dat$sites$Y[,2])
##         ranx2 <- range(plt.dat$sites$X[,1])
##         rany2 <- range(plt.dat$sites$X[,2])
##     }
##     if(missing(ylab))
##         ylabs <- substitute("Axis " * arg1 * " " * (lambda[arg1] == arg2),
##                             list(arg1 = choices[2],
##                                  arg2 = round(x$lambda[choices[2]], 4)))
##     if(missing(xlab))
##         xlabs <- substitute("Axis " * arg1 * " " * (lambda[arg1] == arg2),
##                             list(arg1 = choices[1],
##                                  arg2 = round(x$lambda[choices[1]], 4)))
##     if(missing(main)) {
##         main1 <- x$nam.dat$namY
##         main2 <- x$nam.dat$namX
##     }
##     plot.new()
##     plot.window(xlim = ranx1, ylim = rany1, asp = 1, ...)
##     if(!is.null(plt.dat$sites))
##         if(type == "text")
##             text(plt.dat$sites$Y, rownames(plt.dat$sites$Y),
##                  cex = cex[1], col = col[1])
##         else if (type == "points")
##             points(plt.dat$sites$X1, cex = cex[1], pch = pch[1],
##                    col = col[1])
##     if(!is.null(plt.dat$species))
##         if(type == "text")
##             text(plt.dat$species$Y, rownames(plt.dat$species$Y),
##                  cex = cex[1], col = col[1])
##         else if(type == "points")
##             points(plt.dat$species$U1, cex = cex[2], pch = pch[2],
##                    col = col[2])
##     if(!is.null(plt.dat$default))
##         if(type == "text")
##             text(plt.dat$default[[1]], rownames(plt.dat$default[[1]]),
##                  cex = cex[1], col = col[1])
##         else if(type == "points")
##             points(plt.dat$default[[1]], cex = cex[2], pch = pch[2],
##                    col = col[2])
##     if(axes) {
##         axis(1)
##         axis(2)
##         box()
##     }
##     if(ann)
##         title(main = main1, sub = sub, ylab = ylabs, xlab = xlabs)
##     plot.new()
##     plot.window(xlim = ranx2, ylim = rany2, asp = 1, ...)
##     if(!is.null(plt.dat$sites))
##         if(type == "text")
##             text(plt.dat$sites$X, rownames(plt.dat$sites$X),
##                  cex = cex[1], col = col[1])
##         else if(type == "points")
##             points(plt.dat$sites$X, cex = cex[1], pch = pch[1],
##                    col = col[1])
##     if(!is.null(plt.dat$species))
##         if(type == "text")
##             text(plt.dat$species$X, rownames(plt.dat$species$X),
##                  cex = cex[2], col = col[2])
##         else if(type == "points")
##             points(plt.dat$species$X, cex = cex[2], pch = pch[2],
##                    col = col[2])
##     if(!is.null(plt.dat$default))
##         if(type == "text")
##             text(plt.dat$default[[2]], rownames(plt.dat$default[[2]]),
##                  cex = cex[1], col = col[1])
##         else if(type == "points")
##             points(plt.dat$default[[2]], cex = cex[1], pch = pch[1],
##                    col = col[1])
##     if(axes){
##         axis(1)
##         axis(2)
##         box()
##     }
##     if(ann)
##         title(main = main2, sub = sub, ylab = ylabs, xlab = xlabs)
##     invisible()
## }
