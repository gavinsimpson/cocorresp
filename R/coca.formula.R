"coca.formula" <- function(formula, data, method = c("predictive", "symmetric"),
                           reg.method = c("simpls", "eigen"), weights = NULL,
                           n.axes = NULL, symmetric = FALSE, quiet = FALSE, ...) {
    parseFormula <- function (formula, data) {
        Terms <- terms(formula, "Condition", data = data)
        flapart <- fla <- formula <- formula(Terms, width.cutoff = 500)
        specdata <- formula[[2]]
        Yresponse <- as.matrix(eval(specdata, data, parent.frame()))
        formula[[2]] <- NULL
        Ypredictors <- eval(formula[[2]], data, parent.frame())
        if (formula[[2]] == "1" || formula[[2]] == "0") {
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
    nam.dat <- list(namY = deparse(formula[[2]], width.cutoff = 500),
                    namX = deparse(formula[[3]], width.cutoff = 500))
    if (nam.dat$namX == ".") {
        nam.dat$namX <- deparse(substitute(data))
    }
    y <- checkCommunityData(y)
    x <- checkCommunityData(x)
    dropped <- c(attr(y, "dropped"), attr(x, "dropped"))
    if (!quiet) {
        msgIfDroppedVars(dropped, nam.dat)
    }
    method <- match.arg(method)
    if(method == "predictive") {
        reg.method <- match.arg(reg.method)
        retval <- switch(reg.method,
                         simpls = predcoca.simpls(y, x, R0 = weights,
                                                  n.axes = n.axes, nam.dat),
                         eigen = predcoca.eigen(y, x, R0 = weights,
                                                n.axes = n.axes, nam.dat))
    } else {
        retval <- symcoca(y, x, n.axes = n.axes, R0 = weights,
                          symmetric = symmetric, nam.dat)
    }
    retval
}

