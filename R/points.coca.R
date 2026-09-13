#' Add points to a Co-CA plot
#'
#' Draws points on the current graphic device based on in
#' supplied [coca] model object.
#' @details The visual appearance of the plotted points can be controlled by
#' supplying appropriate graphical parameters via the `...`
#' argument. See [par] for details.
#' @param x an object inheriting from class [coca].
#' @param display character; one of the stated choices. Indicates which
#' scores to use to draw points.
#' @param which character; one of the stated choices. Indicates which of
#' the response or predictor data sets is used to select scores from.
#' @param choices The Co-CA axes to draw points for.
#' @param scaling logical; should species scores in a symmetric Co-CA be
#' rescaled?
#' @param select Items to be displayed. This can either be a logical
#' vector which is `TRUE` for displayed items or a vector of
#' indices of displayed items.
#' @param ... Arguments passed to other methods
#' @returns Returns the plotted x and y coordinates as a matrix.
#' @author Gavin L. Simpson
#' @seealso [plot] methods; [plot.predcoca] and
#' [plot.symcoca].
#' @examples
#' ## symmetric CoCA
#' data(beetles)
#' data(plants)
#'
#' ## log transform the bettle data
#' beetles <- log(beetles + 1)
#'
#' ## fit the model
#' bp.sym <- coca(beetles ~ ., data = plants, method = "symmetric")
#'
#' ## draw a plot of the response scores
#' plot(bp.sym, type = "none")
#' points(bp.sym, display = "sites", col = "blue", pch = 16)
#' points(bp.sym, display = "species", col = "red", pch = 3, cex = 0.8)
#' @keywords methods
#' @rdname points.coca
#' @name points.coca
#' @export
points.coca <- function(
  x,
  display = c("sites", "species"),
  which = c("response", "predictor"),
  choices = c(1, 2),
  scaling = FALSE,
  select,
  ...
) {
  if (length(display) > 1) {
    warning("Only one set of scores can be plotted at a time.")
  }
  display <- match.arg(display)
  ## what are we plotting, response or predictor?
  which <- match.arg(which)
  ## and map to X and Y for extraction
  WHICH <- ifelse(which == "response", "Y", "X")
  ## should the scores be rescaled - only for species though
  ## need two and only two axes to plot
  if (length(choices) != 2) {
    stop("Exactly two axes should be specified in `choices`")
  }
  ## get the scores for plotting
  scrs <- scores(x, choices = choices, display = display, scaling = scaling)
  ## then extract the response or predictor scores
  scrs <- lapply(scrs, `[[`, WHICH)
  ## alter the names of scrs so xy.coords knows what to do
  scrs <- scrs[[1]]
  colnames(scrs) <- c("x", "y")
  ## draw the scores with points()
  points(scrs, ...)
  invisible(scrs)
}
