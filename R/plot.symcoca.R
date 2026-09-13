#' Plots for symmetric co-correspondence analysis
#'
#' Produces plots of the response and predictor from the results of a
#'   symmetric co-correspondence analysis.
#' @param x an object of class `"symcoca"`, the result of
#'     a call to [symcoca].
#' @param which character; should the response or predictor scores be
#'     plotted.
#' @param choices a vector of length 2 indicating which predictive CoCA
#'     axes to plot.
#' @param display which sets of scores are drawn. See
#'     [scores.symcoca].
#' @param scaling logical, whether scaling should be applied. See
#'     [scores.symcoca].
#' @param type one of `"points"`, `"text"`, or
#'     `"none"`. Determines how the site and species scores are
#'     displayed. If `type = "points"`, scores are plotted as
#'     points. If `type = "text"`, then the row names of the scores
#'     matrices are plotted. If `type = "none"`, then the scores are
#'     not plotted.
#' @param xlim, ylim limits for the x and y axes. If non supplied,
#'     suitable limits will be determined from the data.
#' @param xlab, ylab labels for the x and y axes. If non supplied
#'     suitable labels are formed from the result object.
#' @param main, sub the main and sub titles for the plot.
#' @param ann logical, if TRUE plots are annotated and not if FALSE,
#'     currently ignored.
#' @param axes a logical value indicating whether both axes should be drawn
#'     on the plot.
#' @param ... other graphical parameters as in 'par' may also be passed as
#'     arguments.
#' @references Ter Braak, C.J.F and Schaffers, A.P. (2004) Co-Correspondence
#'   Analysis: a new ordination method to relate two community
#'   compositions. *Ecology* **85(3)**, 834--846
#' @author Gavin L. Simpson.
#' @seealso [coca], [plot.default]
#' @examples
#' ## symmetric CoCA
#' data(beetles)
#' data(plants)
#'
#' ## log transform the beetle data
#' beetles <- log(beetles + 1)
#'
#' ## fit the model
#' bp.sym <- coca(beetles ~ ., data = plants, method = "symmetric")
#'
#' ## draw a plot of the response scores
#' plot(bp.sym)
#'
#' ## plot of both
#' layout(matrix(1:2, ncol = 2))
#' plot(bp.sym, which = "response", main = "Beetles")
#' plot(bp.sym, which = "predictor", main = "Plants")
#' layout(1)
#' @keywords methods
#' @rdname plot.symcoca
#' @export
`plot.symcoca` <- function(x,
                           which = "response",
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
    if(missing(display)) {
        display <-  c("species", "sites")
    }
    display <- match.arg(display, several.ok = TRUE)
    ## what are we plotting, response or predictor?
    if (length(which) > 1L) {
        message("Only a single value of 'which' is allowed.\nUsing first supplied.")
    }
    ## and map to X and Y for extraction
    WHICH <- selectWhich(which)
    ## should the scores be rescaled - only for species though
    if(is.logical(scaling)) {
        scaling <- ifelse(scaling, 2, 1)
    }
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
    ## process x/y label
    ev <- eigenvals(x)
    if(missing(xlab)) {
        xlabs <- sapply(xy, `[[`, "xlab")
        xlab <- xlabs[!is.null(xlabs)][1]
        eigx <- round(ev[choices[1]], 4)
        xlab <- bquote(.(xlab) ~~ (lambda[.(choices[1])] == .(eigx)))
    }
    if(missing(ylab)) {
        ylabs <- sapply(xy, `[[`, "ylab")
        ylab <- ylabs[!is.null(ylabs)][1]
        eigy <- round(ev[choices[2]], 4)
        ylab <- bquote(.(ylab) ~~ (lambda[.(choices[2])] == .(eigy)))
    }
    #opar <- par(no.readonly=TRUE)
    #on.exit(par(opar))
    ## plotting
    plot.new()
    plot.window(xlim, ylim, ...)
    abline(h = 0, lty = "dashed", col = "grey")
    abline(v = 0, lty = "dashed", col = "grey")
    if (!is.null(scrs$species)) {
        if (type == "text") {
            text(scrs$species, rownames(scrs$species), col = "red",
                 cex = 0.7, ...)
        }
        if (type == "points"){
            points(scrs$species, col = "red", pch = 3, cex = 0.7, ...)
        }
    }
    if (!is.null(scrs$sites)) {
        if (type == "text") {
            text(scrs$sites, rownames(scrs$sites), col = "black",
                 cex = 0.7, ...)
        }
        if (type == "points"){
            points(scrs$sites, col = "black", pch = 1, cex = 0.7, ...)
        }
    }
    if (axes) {
        axis(1)
        axis(2)
        box()
    }
    if (ann) {
        title(main = main, sub = sub, xlab = xlab, ylab = ylab, ...)
    }
    class(scrs) <- "ordiplot"
    invisible(scrs)
}
