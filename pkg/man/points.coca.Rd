\name{points.coca}
\alias{points.coca}

\title{Add points to a Co-CA plot}
\description{Draws points on the current graphic device based on in
  supplied \code{\link{coca}} model object.}
\usage{
\method{points}{coca}(x, display = c("sites", "species"),
       which = c("response", "predictor"), choices = c(1, 2),
       scaling = FALSE, select, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{an object inheriting from class \code{\link{coca}}.}
  \item{display}{character; one of the stated choices. Indicates which
    scores to use to draw points.}
  \item{which}{character; one of the stated choices. Indicates which of
    the response or predictor data sets is used to select scores from.}
  \item{choices}{The Co-CA axes to draw points for.}
  \item{scaling}{logical; should species scores in a symmetric Co-CA be
    rescaled?}
  \item{select}{Items to be displayed. This can either be a logical
    vector which is \code{TRUE} for displayed items or a vector of
    indices of displayed items.}
  \item{\dots}{Arguments passed to other methods}
}
\details{
  The visual appearance of the plotted points can be controlled by
  supplying appropriate graphical parameters via the \code{\ldots}
  argument. See \code{\link{par}} for details.
}
\value{
  Returns the plotted x and y coordinates as a matrix.
}
% \references{
% %% ~put references to the literature/web site here ~
% }
\author{
  Gavin L. Simpson
}
% \note{
% %%  ~~further notes~~
% }

\seealso{
  \code{\link{plot}} methods; \code{\link{plot.predcoca}} and
  \code{\link{plot.symcoca}}.
}
\examples{
## symmetric CoCA
data(beetles)
data(plants)
     
## log transform the bettle data
beetles <- log(beetles + 1)
     
## fit the model
bp.sym <- coca(beetles ~ ., data = plants, method = "symmetric")

## draw a biplot of the response
plot(bp.sym, type = "none")
points(bp.sym, display = "sites", col = "blue", pch = 16)
points(bp.sym, display = "species", col = "red", pch = 3, cex = 0.8)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ methods }
