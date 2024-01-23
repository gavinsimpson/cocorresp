## Function that provides a common & consistent interface
## to select which community matrix to display/work with

`selectWhich` <- function(x) {
    optsY1 <- c("y1", "Y1", "y", "Y", "response")
    optsY2 <- c("y2", "Y2", "x", "X", "predictor")
    opts <- c(optsY1, optsY2)

    ## Match x against available options
    sel <- match(x, opts)
    opts <- opts[sel]

    ## for each group relating to response or predictor,
    ## take only one of each even if multiple options for
    ## same group are supplied
    selY1 <- any(opts %in% optsY1)
    selY2 <- any(opts %in% optsY2)

    c("Y", "X")[c(selY1, selY2)]
}
