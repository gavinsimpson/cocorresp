\name{plot.symcoca}
\alias{plot.symcoca}
\title{Biplots for symmetric co-correspondence analysis}
\description{
  Produces biplots of the response and predictor from the results of a
  symmetric co-correspondence analysis.
}
\usage{
\method{plot}{symcoca}(x, choices = c(1:2),
     display = c("species", "site"), scaling = FALSE,
     oneFig = TRUE, type = c("points", "text", "none"),
     ask = prod(par("mfcol")) < 2 && dev.interactive(),
     cex = c(0.7, 0.7), pch = c(par("pch"), 3),
     col = c("black", "red"), ylab, xlab, main, sub = "",
     axes = TRUE, ann = par("ann"), \ldots)
}

\arguments{
  \item{x}{ an object of class \code{"symcoca"}, the result of
    a call to \code{\link{symcoca}}.}
  \item{choices}{ a vector of length 2 indicating which predictive CoCA
    axes to plot.}
  \item{display}{ which sets of scores are drawn. See
    \code{\link{scores.symcoca}}.}
  \item{scaling}{ logical, whether scaling should be applied. See
    \code{\link{scores.symcoca}}.}
  \item{oneFig}{ if \code{TRUE}, biplots for the response and predictor
    are plotted on the same figure using \code{par(mfrow = c(1,2))}. If
    \code{FALSE} each biplot is plotted in turn on the active
    device. The first plot will be overwritten unless \code{ask = TRUE},
    see below.}
  \item{type}{ one of \code{"points"}, \code{"text"}, or
    \code{"none"}. Determines how the site and species scores are
    displayed. If \code{type = "points"}, scores are plotted as points
    using plotting characters given in argument \code{pch}. If
    \code{type = "text"}, then the row names of the scores matrices are
    plotted. If \code{type = "none"}, then the scores are not plotted.}
  \item{ask}{logical, if \code{TRUE}, the user is \emph{ask}ed before
    each plot, see \code{\link{par}}.}
  \item{cex}{ a vector of length 2, containing the character expansion
    factors to use for the samples (sites) and the species.}
  \item{col}{ a vector of length 2, containing the colours used to draw
    the samples (sites) and the species markers.}
  \item{pch}{ a vector of length 2, containing the plotting character to
    use for the samples (sites) and the species.}
  \item{xlab, ylab}{ labels for the x and y axes, defaults to
    \dQuote{Axis X (lamda\_X = Y)}, where \sQuote{X} is the CoCa axis
    plotted and \sQuote{Y} is the Eigenvalue for axis \sQuote{X}.}
  \item{main}{ a main title for the plot, defaults to the names given in
  \code{x\$nam.dat}. User supplied values currently ignored.}
  \item{sub}{a sub title for the plots. Currently the same sub title is
    applied to each biplot.}
  \item{axes}{ a logical value indicating whether both axes should be drawn
    on the plot.}
  \item{ann}{ logical, if TRUE plots are annotated and not if FALSE,
    currently ignored.}
  \item{\dots}{ other graphical parameters as in 'par' may also be passed as
    arguments.}
}
\references{

  Ter Braak, C.J.F and Schaffers, A.P. (2004) Co-Correspondence
  Analysis: a new ordination method to relate two community
  compositions. \emph{Ecology} \bold{85(3)}, 834--846
}

\author{Gavin L. Simpson.}

\note{This is an improved plot method for \code{symcoca} objects, that
  is far more configurable. More intuitive handling of user supplied
  values for \code{main}, \code{xlab}, \code{ylab} and \code{sub} will
  be provided in the next version.}

\seealso{\code{\link{coca}}, \code{\link{plot.default}}}

\examples{
## symmetric CoCA
data(beetles)

## log transform the bettle data
beetles <- log(beetles + 1)

data(plants)

## fit the model
bp.sym <- coca(beetles ~ ., data = plants, method = "symmetric")
## draw the biplots

plot(bp.sym)
}
\keyword{hplot}
