#' @rdname coca
#' @name coca
#' @export
coca.formula <- function(
  formula,
  data,
  method = c("predictive", "symmetric"),
  reg.method = c("simpls", "eigen"),
  weights = NULL,
  n.axes = NULL,
  symmetric = FALSE,
  quiet = FALSE,
  ...
) {
  parseFormula <- function(formula, data) {
    Terms <- terms(formula, "Condition", data = data)
    formula <- formula(Terms, width.cutoff = 500)
    specdata <- formula[[2]]
    Yresponse <- as.matrix(eval(specdata, data, environment(Terms)))
    formula[[2]] <- NULL
    if (identical(formula[[2]], 1) || identical(formula[[2]], 0)) {
      Ypredictors <- NULL
    } else {
      mf <- model.frame(formula, data, na.action = na.fail)
      Ypredictors <- model.matrix(formula, mf)
      if (any(colnames(Ypredictors) == "(Intercept)")) {
        xint <- which(colnames(Ypredictors) == "(Intercept)")
        Ypredictors <- Ypredictors[, -xint, drop = FALSE]
      }
    }
    list(Yresponse = Yresponse, Ypredictors = Ypredictors)
  }
  if (missing(data)) {
    data <- parent.frame()
  }
  dat <- parseFormula(formula, data)
  x <- dat$Ypredictors
  y <- dat$Yresponse
  nam.dat <- list(
    namY = deparse(formula[[2]], width.cutoff = 500),
    namX = deparse(formula[[3]], width.cutoff = 500)
  )
  if (nam.dat$namX == ".") {
    nam.dat$namX <- deparse(substitute(data))
  }
  fit_coca(y, x, method, reg.method, weights, n.axes, symmetric, quiet, nam.dat)
}
