## `biplot.coca` <- function(x, ...) {
##
## }

#' @rdname biplots
#' @name biplot-methods
#' @export
biplot.predcoca <- function(
  x,
  which = "response",
  choices = 1:2,
  type = NULL,
  xlim = NULL,
  ylim = NULL,
  col.species = "red",
  col.sites = "black",
  pch.species = 3,
  pch.sites = 1,
  cex = 0.7,
  main = "",
  sub = "",
  ylab,
  xlab,
  ann = par("ann"),
  axes = TRUE,
  ...
) {
  ## sort out arguments
  which <- selectWhich(which)
  if (length(which) != 1L) {
    stop("Select exactly one community in which")
  }
  ## need two and only two axes to plot
  if (length(choices) != 2L) {
    stop("Exactly two axes should be specified in `choices`")
  }
  ## grab the scores we need depending on what matrix is requested
  scrs <- if (isTRUE(which == "Y")) {
    ## for proper biplot of response we want spp scores from
    ## response & site score from predictor
    list(
      species = scores(x, display = "species", choices = choices)[["species"]][[
        "Y"
      ]],
      sites = scores(x, display = "sites", choices = choices)[["sites"]][["X"]]
    )
  } else {
    ## for proper biplot of predictor we want spp loadings from predictor
    ## & site score from predictor
    list(
      species = loadings.predcoca(x, which = "predictor", choices = choices),
      sites = scores(x, display = "sites", choices = choices)[["sites"]][["X"]]
    )
  }
  if (!is.null(type)) {
    type <- match.arg(type, c("text", "points"))
  }
  draw_coca(
    scrs,
    eigenvals(x),
    choices,
    type,
    xlim,
    ylim,
    col.species,
    col.sites,
    pch.species,
    pch.sites,
    cex,
    main,
    sub,
    if (missing(ylab)) NULL else ylab,
    if (missing(xlab)) NULL else xlab,
    ann,
    axes,
    ...
  )
}
#' Biplots of co-correspondence analysis models
#'
#' Produces biplots of the response and predictor from the results of a
#' co-correspondence analysis models.
#' @param x an object of class `"symcoca"`, the result of
#' a call to [symcoca].
#' @param which character; should the response or predictor scores be
#' plotted. Can be specified in several ways: *response* choices
#' are one from `c("y", "Y", "y1", "response")`; *predictor*
#' choices are one from `c("x", "X", "y2", "predictor")`.
#' @param choices a vector of length 2 indicating which predictive CoCA
#' axes to plot.
#' @param benzecri logical, should a Benzecri plot be drawn? Such plots
#' draw species scores, scaled by the quarter root of the respective
#' eigenvalues, with unscaled site scores. A Benzecri plot is the
#' recommended biplot for symmetric CoCA. See
#' [scores.symcoca].
#' @param type one of `"points"`, or `"text"`. Determines how
#' the site and species scores are displayed. If `type = "points"`,
#' scores are plotted as points. If `type = "text"`, then the row
#' names of the scores matrices are plotted.
#' @param xlim,ylim limits for the x and y axes. If non supplied,
#' suitable limits will be determined from the data.
#' @param col.species,col.sites,pch.species,pch.sites colours and
#' plotting characters used when plotting the species and sites
#' scores.
#' @param cex numeric; scaling factor when drawing points or text
#' labels.
#' @param xlab,ylab labels for the x and y axes. If non supplied
#' suitable labels are formed from the result object.
#' @param main,sub the main and sub titles for the plot.
#' @param ann logical, if TRUE plots are annotated and not if FALSE,
#' currently ignored.
#' @param axes a logical value indicating whether axes and plot border
#' should be drawn on the plot.
#' @param ... other graphical parameters as in 'par' may also be passed as
#' arguments.
#' @references Ter Braak, C.J.F and Schaffers, A.P. (2004) Co-Correspondence
#' Analysis: a new ordination method to relate two community
#' compositions. *Ecology* **85(3)**, 834--846
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
#' ## draw a biplot of the beetle results
#' biplot(bp.sym)
#'
#' ## biplot of both - Fig 1 in ter Braak & Schaffers (2004)
#' layout(matrix(1:2, ncol = 2))
#' biplot(bp.sym, which = "y1", main = "Beetles")
#' biplot(bp.sym, which = "y2", main = "Plants")
#' layout(1)
#'
#' ## predictive CoCA
#' bp.pred <- coca(beetles ~ ., data = plants)
#'
#' ## draw a biplot of the response
#' biplot(bp.pred)
#'
#' ## recreate Fig 3 in ter Braak & Schaffers (2004)
#' layout(matrix(1:2, ncol = 2))
#' biplot(bp.pred, which = "response", main = "Beetles")
#' biplot(bp.pred, which = "predictor", main = "Plants")
#' layout(1)
#' @keywords methods
#' @rdname biplots
#' @export
biplot.symcoca <- function(
  x,
  which = "y1",
  choices = 1:2,
  benzecri = TRUE,
  type = NULL,
  xlim = NULL,
  ylim = NULL,
  col.species = "red",
  col.sites = "black",
  pch.species = 3,
  pch.sites = 1,
  cex = 0.7,
  main = "",
  sub = "",
  ylab,
  xlab,
  ann = par("ann"),
  axes = TRUE,
  ...
) {
  ## sort out arguments
  which <- selectWhich(which)
  if (length(which) != 1L) {
    stop("Select exactly one community in which")
  }
  ## need two and only two axes to plot
  if (length(choices) != 2L) {
    stop("Exactly two axes should be specified in `choices`")
  }
  if (isTRUE(benzecri)) {
    scale.species <- TRUE
    scale.sites <- FALSE
  } else {
    scale.species <- TRUE
    scale.sites <- TRUE
  }
  scrs <- list(
    species = scores(
      x,
      choices = choices,
      display = "species",
      scaling = scale.species
    )[["species"]][[which]],
    sites = scores(
      x,
      choices = choices,
      display = "sites",
      scaling = scale.sites
    )[["sites"]][[which]]
  )

  if (!is.null(type)) {
    type <- match.arg(type, c("text", "points"))
  }
  draw_coca(
    scrs,
    eigenvals(x),
    choices,
    type,
    xlim,
    ylim,
    col.species,
    col.sites,
    pch.species,
    pch.sites,
    cex,
    main,
    sub,
    if (missing(ylab)) NULL else ylab,
    if (missing(xlab)) NULL else xlab,
    ann,
    axes,
    ...
  )
}
