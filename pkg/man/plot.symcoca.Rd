\name{plot.symcoca}
\alias{plot.symcoca}
\title{Biplots for symmetric co-correspondence analysis}
\description{
  Produces biplots of the response and predictor from the results of a
  symmetric co-correspondence analysis.
}
\usage{
\method{plot}{symcoca}(x,
    which = c("response", "predictor"),
    choices = 1:2,
    display = c("species", "sites"),
    scaling = FALSE,
    type,
    xlim = NULL,
    ylim = NULL,
    main = "",
    sub = "",
    ylab, xlab,
    ann = par("ann"),
    axes = TRUE,
    \ldots)
}

\arguments{
  \item{x}{ an object of class \code{"symcoca"}, the result of
    a call to \code{\link{symcoca}}.}
  \item{which}{character; should the response or predictor scores be
    plotted.}
  \item{choices}{ a vector of length 2 indicating which predictive CoCA
    axes to plot.}
  \item{display}{ which sets of scores are drawn. See
    \code{\link{scores.symcoca}}.}
  \item{scaling}{ logical, whether scaling should be applied. See
    \code{\link{scores.symcoca}}.}
  \item{type}{ one of \code{"points"}, \code{"text"}, or
    \code{"none"}. Determines how the site and species scores are
    displayed. If \code{type = "points"}, scores are plotted as
    points. If \code{type = "text"}, then the row names of the scores
    matrices are plotted. If \code{type = "none"}, then the scores are
    not plotted.}
  \item{xlim, ylim}{limits for the x and y axes. If non supplied,
    suitable limits will be determined from the data.}
  \item{xlab, ylab}{ labels for the x and y axes. If non supplied
    suitable labels are formed from the result object.}
  \item{main, sub}{ the main and sub titles for the plot.}
  \item{ann}{ logical, if TRUE plots are annotated and not if FALSE,
    currently ignored.}
  \item{axes}{ a logical value indicating whether both axes should be drawn
    on the plot.}
  \item{\dots}{ other graphical parameters as in 'par' may also be passed as
    arguments.}
}
\references{

  Ter Braak, C.J.F and Schaffers, A.P. (2004) Co-Correspondence
  Analysis: a new ordination method to relate two community
  compositions. \emph{Ecology} \bold{85(3)}, 834--846
}

\author{Gavin L. Simpson.}

\seealso{\code{\link{coca}}, \code{\link{plot.default}}}

\examples{
## symmetric CoCA
data(beetles)
data(plants)

## log transform the bettle data
beetles <- log(beetles + 1)

## fit the model
bp.sym <- coca(beetles ~ ., data = plants, method = "symmetric")

## draw a biplot of the response
plot(bp.sym)

## plot of both
layout(matrix(1:2, ncol = 2))
plot(bp.sym, which = "response", main = "Beetles")
plot(bp.sym, which = "predictor", main = "Plants")
layout(1)
}
\keyword{hplot}
\keyword{methods}
