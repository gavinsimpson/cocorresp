#' Co-correspondence analysis ordination methods for community ecology
#'
#' Fits predictive and symmetric co-correspondence analysis (CoCA) models
#' to relate one data matrix to another data matrix. More specifically,
#' CoCA maximises the weighted covariance between the weighted averaged
#' species scores of one community and the weighted averaged species
#' scores of another community. CoCA attempts to find patterns that are
#' common to both communities.
#' @details The main interface function is [coca] which accepts a
#' formula or two community data matrices. An appropriate formula is
#' `Y ~ ., data = X` and the associated `data` object from
#' which `.` will be looked up. The `method` argument is used
#' to select from the two forms of CoCA: `method = "predictive"` for
#' predictive CoCA (the default), and `method = "symmetric"` for
#' symmetric CoCA.
#'
#' vegan is imported but is not attached by `library(cocorresp)`. Its
#' [scores()][vegan::scores], [eigenvals()][vegan::eigenvals],
#' [permutest()][vegan::permutest] and [envfit()][vegan::envfit] generics are
#' re-exported, so these functions remain available after attaching cocorresp.
#' @author Original Matlab routines by C.J.F. ter Braak and A.P. Schaffers. R port
#' by Gavin L. Simpson. Function simpls based on `simpls.fit` (package
#' pls) by Ron Wehrens and Bjorn-Helge Mevik.
#'
#' Maintainer: Gavin L. Simpson <ucfagls@gmail.com>
#' @keywords package
#' @rdname cocorresp-package
#' @aliases cocorresp-package cocorresp
#' @name cocorresp-package
#' @importFrom vegan pasteCall vectorfit factorfit
#' @importFrom stats resid weights complete.cases cor model.frame model.matrix na.fail printCoefmat runif terms screeplot setNames
#' @importFrom graphics abline plot axis box par plot.new plot.window points text title
#' @importFrom grDevices xy.coords
#' @importFrom utils flush.console
"_PACKAGE"
