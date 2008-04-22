"corAxis" <-
function(x, ...)
  UseMethod("corAxis")

"corAxis.default" <-
function(x, ...)
  stop("No default method for corAxis")

"corAxis.symcoca" <-
function(x, axes = c(1:min(6, x$n.axes)), ...) {
  if (class(x) != "symcoca")
    stop("object must be of class \"symcoca\"")
  score.dat <- scores(x, axes, display = "site")
  diag(cor(score.dat$X1, score.dat$X2))
}
