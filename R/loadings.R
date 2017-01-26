##' CoCA species loadings
##'
##' Extract CoCA species loadings from fitted objects
##'
##' \code{loadings()} is an extractor function to access the loadings of a
##' fitted CoCA model.
##'
##' This is a generic function, replacing the \code{\link[stats]{loadings}}
##' function, which is preserved as the exported default S3 method. Methods
##' are provided for both predictive and symmetric CoCA.
##'
##' @param x an object resulting from a call to \code{\link{coca}}
##' @param ... arguments passed to other methods
##'
##' @return A list of data frames or a single data frame depending on other arguments.
##'
##' @author Gavin L. Simpson
##'
##' @importFrom stats loadings
##' @keywords methods utilities
##' @rdname loadings
##'
##' @examples
##' ## symmetric CoCA
##' data(beetles)
##' ## log transform the bettle data
##' beetles <- log(beetles + 1)
##' data(plants)
##' ## fit the model
##' bp.sym <- coca(beetles ~ ., data = plants, method = "symmetric")
##' ## extract the loadings
##' lds <- loadings(bp.sym)
##' head(lds[["Y"]])                        # loadings for the Y_1 matrix
##' str(lds)
`loadings` <- function(x, ...) {
    UseMethod("loadings")
}

##' @rdname loadings
`loadings.default` <- function(x, ...) stats::loadings(x)


##' @rdname loadings
##' @param choices numeric; vector of Co-CA axes to extract loadings for.
##' @param which character; for which matrices should loadings be extracted?
##' @importFrom stats setNames
`loadings.predcoca` <- function(x, choices = c(1, 2),
                                which = c("response", "predictor"), ...) {
    if (!isTRUE(inherits(x, "predcoca"))) {
        stop("x must be of class \"predcoca\"")
    }
    which <- match.arg(which, several.ok = TRUE)
    WHICH <- setNames(c("Y","X"), c("response", "predictor"))
    WHICH <- WHICH[which]
    out <- x[["loadings"]][WHICH]
    out <- lapply(out, `[`, , choices)
    if (length(out) == 1L) {
        out <- out[[1]]
    }
    out
}

##' @rdname loadings
##' @importFrom stats setNames
`loadings.symcoca` <- function(x, choices = c(1, 2),
                               which = c("y1", "y2"), ...) {
    if (!isTRUE(inherits(x, "symcoca"))) {
        stop("x must be of class \"symcoca\"")
    }
    which <- match.arg(which, several.ok = TRUE)
    WHICH <- setNames(c("Y","X"), c("y1", "y2"))
    WHICH <- WHICH[which]
    out <- x[["loadings"]][WHICH]
    out <- lapply(out, `[`, , choices)
    if (length(out) == 1L) {
        out <- out[[1]]
    }
    out
}
