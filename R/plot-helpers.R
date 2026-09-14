#' @noRd
draw_coca <- function(
  scrs,
  eig,
  choices,
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
  ylab = NULL,
  xlab = NULL,
  ann = par("ann"),
  axes = TRUE,
  ...
) {
  present <- Filter(Negate(is.null), scrs)
  if (!length(present)) {
    stop("No scores selected")
  }
  if (is.null(type)) {
    type <- if (max(vapply(present, nrow, integer(1))) > 80L) {
      "points"
    } else {
      "text"
    }
  }
  type <- match.arg(type, c("text", "points", "none"))
  xy <- lapply(present, xy.coords)
  if (is.null(xlim)) {
    xlim <- range(unlist(lapply(xy, `[[`, "x")), finite = TRUE)
  }
  if (is.null(ylim)) {
    ylim <- range(unlist(lapply(xy, `[[`, "y")), finite = TRUE)
  }
  if (is.null(xlab)) {
    xlab <- axis_label(xy[[1L]]$xlab, eig, choices[1L])
  }
  if (is.null(ylab)) {
    ylab <- axis_label(xy[[1L]]$ylab, eig, choices[2L])
  }
  plot.new()
  plot.window(xlim, ylim, ...)
  abline(h = 0, v = 0, lty = "dashed", col = "grey")
  cols <- c(species = col.species, sites = col.sites)
  pchs <- c(species = pch.species, sites = pch.sites)
  for (nm in names(present)) {
    values <- present[[nm]]
    if (type == "text") {
      text(values, rownames(values), col = cols[[nm]], cex = cex, ...)
    }
    if (type == "points") {
      points(values, col = cols[[nm]], pch = pchs[[nm]], cex = cex, ...)
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

#' @noRd
axis_label <- function(label, eig, choice) {
  if (is.null(eig)) {
    return(label)
  }
  value <- round(eig[choice], 4)
  bquote(.(label) ~ ~ (lambda[.(choice)] == .(value)))
}
