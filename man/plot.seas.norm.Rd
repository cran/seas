\name{plot.seas.norm}
\alias{plot.seas.norm}
\alias{plot.precip.norm}
\title{Plot seasonal normal of a variable, including precipitation normals}
\description{
  Plots a \dQuote{normal} of a seasonal variable, including a
  precipitation normal (which shows rain and snow fractions, where
  available). Significant missing data values are also indicated.
}
\usage{
\method{plot}{seas.norm}(x, maxy, varwidth=FALSE, normwidth=FALSE,
     show.na=TRUE, leg, add.alt=FALSE, \ldots)
}
\arguments{
  \item{x}{a \code{seas.norm} object created by either
    \code{\link{seas.norm}} or \code{\link{precip.norm}}}
  \item{maxy}{maximum y-axis for plot}
  \item{varwidth}{\code{logical}; varies the width of each bar directly
    proportional to the frequency of active days (defined by a
    threshold); the value is normalized according to the next argument}
  \item{normwidth}{normalizes the width of the bars to a fixed
    \code{numeric} value (in days), or the maximum value if given
    \code{TRUE}; the default \code{FALSE} value normalizes each bar to
    the number of potentially active days}
  \item{show.na}{\code{logical}; indicates if \code{NA}s should be
    displayed (as red lines on bottom) or not}
  \item{leg}{if \code{TRUE} shows a legend summary of the statistics in
    the upper left hand corner; if \code{"\link{locator}"} you can
    manually place the legend by clicking on the active graphics device}
  \item{add.alt}{\code{logical}; adds imperial units on the right-hand y-axis}
  \item{\ldots}{other arguments passed to \code{\link{.seastitle}} and
    \code{\link{.seascols}} for title style, colours, and other
    customizations to the appearance}
}
\details{
  The \code{varwidth} variable is useful for separating different
  precipitation patterns throughout the season.  It changes the width of
  the bar proportional to the frequency of precipitation events within
  the bin. Ideally, the bars will be tall and narrow with intense storms
  that occur infrequently, such as convective storms. Conversely the
  bars will be broader with less-intense rainfall events occurring more
  often.
}
\author{M.W. Toews}
\seealso{\code{\link{seas.norm}}, \code{\link{precip.norm}},
  \code{\link{seas.sum}}}
\examples{
data(mscdata)

d.ss <- seas.sum(mscdata, id=1108447)
d.nm <- precip.norm(d.ss, fun="mean")
plot(d.nm)
}
\keyword{hplot}
