#' @rdname crossval
#' @name crossval
#' @export
print.crossval <- function(x, digits = min(3, getOption("digits") - 3), ...) {
  names(x$CVfit) <- paste0("COCA", seq_len(x$n.axes))
  cat("\nCross-validation for Predictive Co-Correspondence Analysis\n\n")
  writeLines(strwrap(pasteCall(x$call)))
  cat(sprintf(
    "\nCross-validatory %%fit of %s to %s:\n\n",
    x$nam.dat$namY,
    x$nam.dat$namX
  ))
  print(round(x$CVfit, digits), print.gap = 2)
  invisible(x)
}
