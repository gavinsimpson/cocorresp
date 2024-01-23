## `biplot.coca` <- function(x, ...) {
##
## }

`biplot.predcoca` <- function(x, which = "response", choices = 1:2,
                              type = NULL, xlim = NULL, ylim = NULL,
                              col.species = "red", col.sites = "black",
                              pch.species = 3, pch.sites = 1, cex = 0.7,
                              main = "", sub = "", ylab, xlab, ann = par("ann"),
                              axes = TRUE, ...) {
    ## sort out arguments
    which <- selectWhich(which)  # verify what we're plotting
    ## need two and only two axes to plot
    if(length(choices) != 2L) {
        stop("Exactly two axes should be specified in `choices`")
    }
    ## grab the scores we need depending on what matrix is requested
    scrs <- if (isTRUE(which == "Y")) {
                ## for proper biplot of response we want spp scores from
                ## response & site score from predictor
                list(species = scores(x, display = "species",
                                      choices = choices)[["species"]][["Y"]],
                     sites = scores(x, display = "sites",
                                    choices = choices)[["sites"]][["X"]])
    } else {
        ## for proper biplot of predictor we want spp loadings from predictor
        ## & site score from predictor
        list(species = loadings.predcoca(x, which = "predictor", choices = choices),
             sites = scores(x, display = "sites",
                            choices = choices)[["sites"]][["X"]])
    }
    ## what type of plot?
    TYPES <- c("text", "points")
    if (missing(type) || is.null(type)) { ## work out whether to plot by text or points
        nitlimit <- 80
        nit <- max(nrow(scrs[["species"]]), nrow(scrs[["sites"]]))
        type <- if (nit > nitlimit) {
            "points"
        } else {
            "text"
        }
    } else {
        type <- match.arg(type, TYPES)
    }

    ## compute xy coords for each set of scores
    xy <- lapply(scrs, xy.coords)
    ## process axis limits if non supplied
    if (is.null(xlim)) {
        xlim <- range(sapply(xy, function(x) range(x$x[is.finite(x$x)])))
    }
    if (is.null(ylim)) {
        ylim <- range(sapply(xy, function(x) range(x$y[is.finite(x$y)])))
    }
    ## Eigenvalues
    ev <- eigenvals(x)
    ## process x/y labels
    if(missing(xlab)) {
        xlabs <- sapply(xy, `[[`, "xlab")
        xlab <- xlabs[!is.null(xlabs)][1]
        if(!is.null(ev)) {
            eigx <- round(ev[choices[1]], 4)
            xlab <- bquote(.(xlab) ~~ (lambda[.(choices[1])] == .(eigx)))
        } else {
            xlab <- bquote(.(xlab))
        }
    }
    if(missing(ylab)) {
        ylabs <- sapply(xy, `[[`, "ylab")
        ylab <- ylabs[!is.null(ylabs)][1]
        if(!is.null(ev)) {
            eigy <- round(ev[choices[2]], 4)
            ylab <- bquote(.(ylab) ~~ (lambda[.(choices[2])] == .(eigy)))
        } else {
            ylab <- bquote(.(ylab))
        }
    }

    ## start plotting
    plot.new()
    plot.window(xlim, ylim, ...)
    abline(h = 0, lty = "dashed", col = "grey")
    abline(v = 0, lty = "dashed", col = "grey")
    if(type == "text") {
        text(scrs[["species"]], rownames(scrs[["species"]]),
             col = col.species, cex = cex, ...)
        text(scrs[["sites"]], rownames(scrs[["sites"]]),
             col = col.sites, cex = cex, ...)
    } else {
        points(scrs[["species"]], col = col.species, pch = pch.species,
               cex = cex, ...)
        points(scrs[["sites"]], col = col.sites, pch = pch.sites,
               cex = cex, ...)
    }
    if (axes) {
        axis(1)
        axis(2)
        box()
    }
    if(ann) {
        title(main = main, sub = sub, xlab = xlab, ylab = ylab, ...)
    }
    class(scrs) <- "ordiplot"
    invisible(scrs)
}

`biplot.symcoca` <- function(x, which = "y1", choices = 1:2,
                             benzecri = TRUE,
                             type = NULL, xlim = NULL, ylim = NULL,
                             col.species = "red", col.sites = "black",
                             pch.species = 3, pch.sites = 1, cex = 0.7,
                             main = "", sub = "", ylab, xlab, ann = par("ann"),
                             axes = TRUE, ...) {
    ## sort out arguments
    which <- selectWhich(which)  # verify what we're plotting
    ## need two and only two axes to plot
    if(length(choices) != 2L) {
        stop("Exactly two axes should be specified in `choices`")
    }
    if (isTRUE(benzecri)) {
        scale.species <- TRUE
        scale.sites   <- FALSE
    } else {
        scale.species <- TRUE
        scale.sites   <- TRUE
    }
    scrs <- list(species = scores(x, choices = choices, display = "species",
                                  scaling = scale.species)[["species"]][[which]],
                 sites = scores(x, choices = choices, display = "sites",
                                scaling = scale.sites)[["sites"]][[which]])

    ## what type of plot?
    TYPES <- c("text", "points")
    if (missing(type) || is.null(type)) { ## work out whether to plot by text or points
        nitlimit <- 80
        nit <- max(nrow(scrs[["species"]]), nrow(scrs[["sites"]]))
        type <- if (nit > nitlimit) {
            "points"
        } else {
            "text"
        }
    } else {
        type <- match.arg(type, TYPES)
    }

    ## compute xy coords for each set of scores
    xy <- lapply(scrs, xy.coords)
    ## process axis limits if non supplied
    if (is.null(xlim)) {
        xlim <- range(sapply(xy, function(x) range(x$x[is.finite(x$x)])))
    }
    if (is.null(ylim)) {
        ylim <- range(sapply(xy, function(x) range(x$y[is.finite(x$y)])))
    }
    ## Eigenvalues
    ev <- eigenvals(x)
    ## process x/y labels
    if(missing(xlab)) {
        xlabs <- sapply(xy, `[[`, "xlab")
        xlab <- xlabs[!is.null(xlabs)][1]
        if(!is.null(ev)) {
            eigx <- round(ev[choices[1]], 4)
            xlab <- bquote(.(xlab) ~~ (lambda[.(choices[1])] == .(eigx)))
        } else {
            xlab <- bquote(.(xlab))
        }
    }
    if(missing(ylab)) {
        ylabs <- sapply(xy, `[[`, "ylab")
        ylab <- ylabs[!is.null(ylabs)][1]
        if(!is.null(ev)) {
            eigy <- round(ev[choices[2]], 4)
            ylab <- bquote(.(ylab) ~~ (lambda[.(choices[2])] == .(eigy)))
        } else {
            ylab <- bquote(.(ylab))
        }
    }

    ## start plotting
    plot.new()
    plot.window(xlim, ylim, ...)
    abline(h = 0, lty = "dashed", col = "grey")
    abline(v = 0, lty = "dashed", col = "grey")
    if(type == "text") {
        text(scrs[["species"]], rownames(scrs[["species"]]),
             col = col.species, cex = cex, ...)
        text(scrs[["sites"]], rownames(scrs[["sites"]]),
             col = col.sites, cex = cex, ...)
    } else {
        points(scrs[["species"]], col = col.species, pch = pch.species,
               cex = cex, ...)
        points(scrs[["sites"]], col = col.sites, pch = pch.sites,
               cex = cex, ...)
    }
    if (axes) {
        axis(1)
        axis(2)
        box()
    }
    if(ann) {
        title(main = main, sub = sub, xlab = xlab, ylab = ylab, ...)
    }
    class(scrs) <- "ordiplot"
    invisible(scrs)
}
