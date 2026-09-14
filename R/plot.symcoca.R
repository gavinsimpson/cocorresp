#' Plots for symmetric co-correspondence analysis
#'
#' Produces plots of the response and predictor from the results of a
#' symmetric co-correspondence analysis.
#' @param x an object of class `"symcoca"`, the result of
#' a call to [symcoca].
#' @param which character; should the response or predictor scores be
#' plotted.
#' @param choices a vector of length 2 indicating which predictive CoCA
#' axes to plot.
#' @param display which sets of scores are drawn. See
#' [scores.symcoca].
#' @param scaling logical, whether scaling should be applied. See
#' [scores.symcoca].
#' @param type one of `"points"`, `"text"`, or
#' `"none"`. Determines how the site and species scores are
#' displayed. If `type = "points"`, scores are plotted as
#' points. If `type = "text"`, then the row names of the scores
#' matrices are plotted. If `type = "none"`, then the scores are
#' not plotted.
#' @param xlim,ylim limits for the x and y axes. If non supplied,
#' suitable limits will be determined from the data.
#' @param xlab,ylab labels for the x and y axes. If non supplied
#' suitable labels are formed from the result object.
#' @param main,sub the main and sub titles for the plot.
#' @param ann logical, if TRUE plots are annotated and not if FALSE,
#' currently ignored.
#' @param axes a logical value indicating whether both axes should be drawn
#' on the plot.
#' @param ... other graphical parameters as in 'par' may also be passed as
#' arguments.
#' @references Ter Braak, C.J.F and Schaffers, A.P. (2004) Co-Correspondence
#' Analysis: a new ordination method to relate two community
#' compositions. *Ecology* **85(3)**, 834--846
#' @author Gavin L. Simpson.
#' @seealso [coca], [plot.default]
#' @keywords methods
#' @rdname plot.symcoca
#' @name plot.symcoca
#' @export
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
plot.symcoca <- function(
  x,
  which = "response",
  choices = 1:2,
  display = c("species", "sites"),
  scaling = FALSE,
  type,
  xlim = NULL,
  ylim = NULL,
  main = "",
  sub = "",
  ylab,
  xlab,
  ann = par("ann"),
  axes = TRUE,
  ...
) {
  ## process the scores to display
  if (missing(display)) {
    display <- c("species", "sites")
  }
  display <- match.arg(display, several.ok = TRUE)
  ## what are we plotting, response or predictor?
  if (length(which) > 1L) {
    message("Only a single value of 'which' is allowed.\nUsing first supplied.")
  }
  ## and map to X and Y for extraction
  WHICH <- selectWhich(which[1L])
  if (length(WHICH) != 1L) {
    stop("Unknown community in which")
  }
  ## should the scores be rescaled - only for species though
  ## need two and only two axes to plot
  if (length(choices) != 2) {
    stop("Exactly two axes should be specified in `choices`")
  }
  ## extract the scores
  scrs <- scores(x, choices = choices, display = display, scaling = scaling)
  ## then extract the response or predictor scores
  scrs <- lapply(scrs, `[[`, WHICH)
  draw_coca(
    scrs,
    eigenvals(x),
    choices,
    type = if (missing(type)) NULL else type,
    xlim = xlim,
    ylim = ylim,
    main = main,
    sub = sub,
    xlab = if (missing(xlab)) NULL else xlab,
    ylab = if (missing(ylab)) NULL else ylab,
    ann = ann,
    axes = axes,
    ...
  )
}
