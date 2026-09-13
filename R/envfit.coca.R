#' Fit an environmental vector or factor to a Co-CA ordination
#'
#' The function fits environmental vectors or factors to a Co-CA
#'   ordination. The projections of points onto vectors have maximum
#'   correlation with corresponding environmental variables, and the
#'   factors show the averages of factor levels.
#' @details See [envfit][vegan::envfit] for details of the method.
#' @param ord a Co-CA ordination object, the result of a call to
#'     [coca].
#' @param env a data frame, matrix or vector of environmental/external
#'     variable(s) to be fitted to the ordination. The variables may be
#'     of a mixed type (factors and continuous variables) in a data frame.
#' @param which character; which of the response or predictor ordinations
#'     should be used during fitting of vectors and factors.
#' @param choices numeric; the axes to which vectors and factors are
#'     fitted.
#' @param scaling logical; should scaling be applied. See
#'     [scores.symcoca].
#' @param w weights used in fitting vectors and factors.
#' @param na.rm Remove points with missing values in ordination scores or
#'     environmental variables. The operation is case-wise; the whole row of
#'     data is removed if there is a missing value and `na.rm =
#'       TRUE`.
#' @param strata An integer vector or factor specifying the strata for
#'     permutation. If supplied, observations are permuted only within the
#'     specified strata.
#' @param permutations Number of permutations for assessing significance
#'     of vectors or factors. Set to `0` to skip permutations.
#' @param ... Arguments passed to [vectorfit][vegan::vectorfit] and
#'     [factorfit][vegan::factorfit].
#' @returns Returns an object of class [envfit][vegan::envfit].
#' @author Gavin L. Simpson. The code interfaces with and uses code from
#'   [envfit][vegan::envfit] for the main computations, which was written by
#'   Jari Oksanen.
#' @seealso [coca] for fitting models. [envfit][vegan::envfit] for
#'   details of the generic function and the computations performed.
#' @examples
#' ## symmetric CoCA
#' data(beetles)
#' data(plants)
#'
#' ## log transform the bettle data
#' beetles <- log(beetles + 1)
#'
#' ## fit the model
#' bp.sym <- coca(beetles ~ ., data = plants, method = "symmetric")
#'
#' ## load the environmental data
#' data(verges)
#'
#' ## fit vectors for the environmental data
#' sol <- envfit(bp.sym, verges, which = "response")
#' \donttest{sol}
#'
#' ## plot the response matrix and the fitted vectors
#' biplot(bp.sym, which = "y1")
#' plot(sol)
#' @keywords htest
#' @section Warning:
#' %   This function is experimental. The vectors fitted are unweighted
#' %   whereas to be consistent with the [cca] method of
#' %   [envfit] they should be weighted. I just need to work out
#' %   the correct sets of weights to use.
#' %
#' @rdname envfit.coca
#' @export
`envfit.coca` <- function(ord, env,
                          which = c("response", "predictor"),
                          choices = c(1,2),
                          scaling = FALSE,
                          w,
                          na.rm = FALSE,
                          strata = NULL,
                          permutations = 999, ...) {
    ## get weights
    if(missing(w)) {
        w <- weights(ord)
    }
    vectors <- factors <- seed <- NULL
    ## what are we plotting, response or predictor?
    which <- match.arg(which)
    ## and map to X and Y for extraction
    WHICH <- ifelse(which == "response", "Y", "X")
    ## should the scores be rescaled - only for species though
    if(is.logical(scaling))
        scaling <- ifelse(scaling, 2, 1)
    X <- scores(ord, display = "sites", choices = choices,
                scaling = scaling)
    ## then extract the response or predictor scores
    X <- lapply(X, `[[`, WHICH)[[1]]
    keep <- complete.cases(X) & complete.cases(env)
    if (any(!keep)) {
        if (!na.rm)
            stop("missing values in data: consider na.rm = TRUE")
        X <- X[keep, , drop=FALSE]
        env <- env[keep, , drop=FALSE]
        na.action <- structure(seq_along(keep)[!keep], class="omit")
    }
    if (is.data.frame(env)) {
        facts <- unlist(lapply(env, is.factor))
        if (sum(facts)) {
            Pfac <- env[, facts, drop = FALSE]
            P <- env[, !facts, drop = FALSE]
            if (length(P)) {
                if (permutations) {
                    if (!exists(".Random.seed", envir = .GlobalEnv,
                                inherits = FALSE)) {
                        runif(1)
                    }
                    seed <- get(".Random.seed", envir = .GlobalEnv,
                                inherits = FALSE)
                }
                vectors <- vectorfit(X, P, permutations, strata,
                                     choices, w = w, ...)
            }
            if (!is.null(seed)) {
                assign(".Random.seed", seed, envir = .GlobalEnv)
            }
            factors <- factorfit(X, Pfac, permutations, strata,
                                 choices, w = w, ...)
            sol <- list(vector = vectors, factors = factors)
        }
        else vectors <- vectorfit(X, env, permutations, strata,
                                  choices, w = w, ...)
    } else {
        vectors <- vectorfit(X, env, permutations, strata,
                             choices, w = w, ...)
    }
    sol <- list(vectors = vectors, factors = factors)
    if (!is.null(na.action))
        sol$na.action <- na.action
    class(sol) <- "envfit"
    sol
}
