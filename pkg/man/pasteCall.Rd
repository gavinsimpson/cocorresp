\name{pasteCall}
\alias{pasteCall}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Pretty printing of R language calls }
\description{
  A helper function to facilitate printing of R calls stored in R
  objects.
}
\usage{
pasteCall(call, prefix = "Call:")
}
\arguments{
  \item{call}{ an object of class \code{\link{call}}.}
  \item{prefix}{ a prefix to prepend to the concatenated, textual
    representation of the call. }
}
\details{
  The function deparses the matched call. If the length of this deparsed
  object is greater than 1, \code{\link{paste}} is used to paste
  together each section of the call, separating each section with a
  space. The concatenated or original call then has the \code{prefix}
  prepended to it.
}
\value{
  A character vector of length one containing the original call.
}
\author{Gavin L.Simpson.}
\note{
  This function is used in a number of the print methods in the
  cocorresp package, with \code{\link{writeLines}} and
  \code{\link{strwrap}} to achieve prettier printing of calls within the
  printed output.
}
\examples{
# From the example in ?call
cl <- call("round", 10.5)
pcl <- pasteCall(cl)
writeLines(strwrap(pcl))

# A silly example of a long call, wrapped on to 2 lines
dat <- as.data.frame(matrix(rnorm(100), nc = 10))
(names(dat) <- paste("var", 1:10, sep = ""))
mod <- lm(var5 ~ var1 + var2 + var3 + var4 + var6 + var7 + var8 +
var9 + var10, data = dat)
writeLines(strwrap(pasteCall(mod$call)))
}
\keyword{utilities}
