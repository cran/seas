\name{plot.seas.sum}
\alias{plot.seas.sum}
\alias{plot.precip.sum}
\title{Plot boxplots of normalized seasonal sums}
\description{
  Plots normalized sums using boxplots.
}
\usage{
\method{plot}{seas.sum}(x, var, norm="days", maxy, logy = FALSE,
    col = "lightgrey", \ldots)
}
\arguments{
  \item{x}{a \code{seas.sum} object created by \code{\link{seas.sum}}}
  \item{var}{name of seasonal variable in \code{x}}
  \item{norm}{variable to normalize by, either \code{"days"} (to
    produce \emph{unit/day}) or \code{"active"} (\emph{unit/day}, when
    active); it may also be a \code{matrix} with the same dimensions as
    \code{x$days}}
  \item{maxy}{maximum y-axis for plot}
  \item{logy}{\code{logical}; makes the y-axis logarithmic, which may be
    necessary of strongly skewed distributions (usually made using small
    \code{width} arguments)}
  \item{col}{colour for boxplot, default is \code{"lightgrey"}}
  \item{\ldots}{other arguments passed to \code{\link{.seastitle}} and
    \code{\link{.seascols}} for title style, colours, and other
    customizations to the appearance}
}
\details{
  This function is a boxplot interpretation of a \code{seas.sum}
  object. This is not the same as treating \code{var} as a continuous
  variable and using \code{plot.seas.var}, since a \code{seas.sum}
  object has been smoothed. Daily extreme values are not well
  represented here as a result.
}
\section{Warning}{
  The appearance of the boxplots are sensitive to the \code{width}
  parameter specified in the \code{seas.sum} function on strongly
  discontinuous variables. Small bin widths capture the discontinuities
  better than wider bins, and changes the skew of the distribution.

  For instance, the median will appear to decrease as \code{width} decreases.
}
\author{M.W. Toews}
\seealso{\code{\link{seas.sum}}, \code{\link{image.seas.sum}},
  \code{\link{seas.sum}}, \code{\link{seas.norm}}}
\examples{
data(mscdata)

van.ss <- seas.sum(mscdata, id=1108447)

# Normalized by the number of days in each bin
plot(van.ss)

# Normalized by the number of active days in each bin
plot(van.ss,norm="active")

}
\keyword{hplot}
