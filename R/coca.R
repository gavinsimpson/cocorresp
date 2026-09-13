#' Fit Co-Correspondence Analysis Ordination Models
#'
#' `coca` is used to fit Co-Correspondence Analysis (CoCA)
#' models. It can fit predictive or symmetric models to two community
#' data matrices containing species abundance data.
#' @details `coca` is the main user-callable function.
#'
#' A typical model has the form `response ~ terms` where
#' `response` is the (numeric) response data frame and `terms`
#' is a series of terms which specifies a linear predictor for
#' `response`. A typical form for `terms` is `.`,
#' which is shorthand for "all variables" in `data`. If `.` is
#' used, `data` must also be provided. If specific species
#' (variables) are required then `terms` should take the form
#' `spp1 + spp2 + spp3`.
#'
#' The default is to fit a predictive CoCA model using SIMPLS via a
#' modified version of [simpls.fit][pls::simpls.fit] from package
#' `pls`. Alternatively, `reg.method = "eigen"` fits the model
#' using an older, slower eigen analysis version of the SIMPLS
#' algorithm. `reg.method = "eigen"` is about 100\% slower than
#' `reg.method = "simpls"`.
#' @param y a data frame containing the response community data matrix.
#' @param x a data frame containing the predictor community data matrix.
#' @param formula a symbolic description of the model to be fit. The
#' details of model specification are given below.
#' @param data an optional data frame containing the variables in the model.
#' If not found in `data`, the variables are taken from
#' `environment(formula)`, typically the environment from which
#' `coca` is called.
#' @param method a character string indicating which co-correspondence
#' analysis method to use. One of `"predictive"`(default), or
#' `"symmetric"`, can be abbreviated.
#' @param reg.method One of `"simpls"` (default) or
#' `"eigen"`. If `method` is `"predictive"` then
#' `reg.method` controls whether the co-correspondence analysis
#' should be fitted using the SIMPLS algorithm or via an eigen
#' analysis.
#' @param weights a vector of length `nrow(y)` of user supplied
#' weights for \eqn{R_0}. If `weights = NULL` (default) then the
#' weights are determined from `y` (default) or `x` and `y`
#' (`symmetric = TRUE` only).
#' @param n.axes the number of CoCA axes to extract. If missing (default)
#' the `n.axes` is \deqn{min(ncol(y), ncol(x), nrow(y), nrow(x)) -
#' 1}.
#' @param symmetric if `method` is `"symmetric"` then
#' `symmetric` determines whether weights for \eqn{R_0} are
#' symmetric and taken as the average of the row sums of `x` and
#' `y` (`symmetric = TRUE`). If `symmetric = FALSE`
#' (default) then the weights \eqn{R_0} are taken as the row sums of `y`
#' unless user defined weights are provided via argument
#' `weights`. Ignored if `method` is `"predictive"`.
#' @param quiet logical; suppress messages due to removal of species with
#' no data.
#' @param ... additional arguments to be passed to lower level methods.
#' @param digits Number of significant digits for printing.
#' @returns `coca` returns a list with `method` and `reg.method`
#' determining the actual components returned.
#'
#'
#' - **nam.dat**: list with components `namY` and `namX` containing the names of the response and the predictor(s) respectively.
#'
#'
#' - **call**: the matched call.
#'
#'
#' - **method**: the CoCA method used, one of `"predictive"` or `"symmetric"`.
#'
#'
#' - **scores**: the species and site scores of the fitted model.
#'
#'
#' - **loadings**: the site loadings of the fitted model for the response and the predictor. (Predictive CoCA via SIMPLS only.)
#'
#'
#' - **fitted**: the fitted values for the response. A list with 2 components `Yhat` (the fitted values on the original scale) and `Yhat1` (the fitted values on the chi-square transformed scale). (Predictive CoCA via SIMPLS only.)
#'
#'
#' - **varianceExp**: list with components `Yblock` and `Xblock` containing the variances in the response and the predictor respectively, explained by each fitted PLS axis. (Predictive CoCA via SIMPLS only.)
#'
#'
#' - **totalVar**: list with components `Yblock` and `Xblock` containing the total variance in the response and the predictor respectively. (Predictive CoCA via SIMPLS only.)
#'
#'
#' - **lambda**: the Eigenvalues of the analysis.
#'
#'
#' - **n.axes**: the number of fitted axes
#'
#'
#' - **Ychi**: a list containing the mean-centred chi-square matrices for the response (`Ychi1`) and the predictor (`Ychi2`). (Predictive CoCA only.)
#'
#'
#' - **R0**: the (possibly user-supplied) row weights used in the analysis.
#'
#'
#' - **X**: X-Matrix (symmetric CoCA only).
#'
#'
#' - **residuals**: Residuals of a symmetric model (symmetric CoCA only).
#'
#'
#' - **inertia**: list with components `total` and `residual` containing the total and residual inertia for the response and the predictor (symmetric CoCA only).
#'
#'
#' - **rowsum**: a list with the row sums for the response (`rsum1`) and the predictor (`rsum2`) (symmetric CoCA only).
#'
#'
#' - **colsum**: a list with the column sums for the response (`csum1`)and the predictor (`csum2`) (symmetric CoCA only).
#' @references ter Braak, C.J.F and Schaffers, A.P. (2004) Co-Correspondence
#' Analysis: a new ordination method to relate two community
#' compositions. *Ecology* **85(3)**, 834--846
#' @author Original Matlab code by C.J.F. ter Braak and A.P. Schaffers. R
#' port by Gavin L. Simpson. Formula method for `coca` uses a
#' modified version of [ordiParseFormula][vegan::ordiParseFormula] by Jari
#' Oksanen to handle formulas.
#' @seealso [crossval] for cross-validation and
#' [permutest.coca] for permutation test to determine the
#' number of PLS axes to retain in for predictive CoCA.
#'
#' [summary.predcoca] and [summary.symcoca] for
#' summary methods.
#' @examples
#' \dontshow{
#' od <- options(digits = 4)
#' mpt <- Sys.getenv("OMP_THREAD_LIMIT", unset = "not_set")
#' Sys.setenv("OMP_THREAD_LIMIT" = 1)
#' }
#' ## symmetric CoCA
#' data(beetles)
#' ## log transform the bettle data
#' beetles <- log(beetles + 1)
#' data(plants)
#' ## fit the model
#' bp.sym <- coca(beetles ~ ., data = plants, method = "symmetric")
#' bp.sym
#' summary(bp.sym)
#' biplot(bp.sym)                          # produces a Benzecri biplot
#'
#' ## extract eigenvalues of the analysis
#' eigenvals(bp.sym)
#'
#' ## correlations between beetle and plant score scores on Co-CA axes
#' corAxis(bp.sym)
#'
#' ## predictive CoCA using SIMPLS and formula interface
#' bp.pred <- coca(beetles ~ ., data = plants)
#' ## should retain only the useful PLS components for a parsimonious model
#'
#' \donttest{
#' ## Leave-one-out crossvalidation - this takes a while
#' crossval(beetles, plants)
#' ## so 2 axes are sufficient
#' ## permutation test to assess significant PLS components - takes a while
#' bp.perm <- permutest(bp.pred, permutations = 99)
#' bp.perm
#' }
#'
#' ## agrees with the Leave-one-out cross-validation
#' ## refit the model with only 2 PLS components
#' bp.pred <- coca(beetles ~ ., data = plants, n.axes = 2)
#' bp.pred
#' summary(bp.pred)
#' biplot(bp.pred)                     # plots correct scores or loadings
#'
#' ## predictive CoCA using Eigen-analysis
#' data(bryophyte)
#' data(vascular)
#' carp.pred <- coca(y = bryophyte, x = vascular, reg.method = "eigen")
#' carp.pred
#'
#' \donttest{
#' ## determine important PLS components - takes a while
#' crossval(bryophyte, vascular)
#' (carp.perm <- permutest(carp.pred, permutations = 99))
#' }
#'
#' ## 2 components again, refit
#' carp.pred <- coca(y = bryophyte, x = vascular,
#' reg.method = "eigen", n.axes = 2)
#' carp.pred
#' ## drawn biplot
#' biplot(carp.pred)
#' \dontshow{
#' options(od)
#' if (mpt == "not_set") {
#' Sys.unsetenv("OMP_THREAD_LIMIT")
#' } else {
#' Sys.setenv("OMP_THREAD_LIMIT" = mpt)
#' }
#' }
#' @keywords models
#' @rdname coca
#' @export
coca <- function(y, ...) {
  UseMethod("coca", y)
}
