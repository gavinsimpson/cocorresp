"print.fitted.symcoca" <-
function(x, digits = max(3, getOption("digits") - 3), ...)
  {
    cat("\nFitted values for:", x$nam.dat["namY"], "\n")
    print(x$Yhat1, digits = digits, ..., print.gap = 2)
    cat("\nFitted values for:", x$nam.dat["namX"], "\n")
    print(x$Yhat2, digits = digits, ..., print.gap = 2)
  }

