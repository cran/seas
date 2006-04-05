\name{image.seas.sum}
\alias{image.seas.sum}
\title{Show a seasonal sum data object}
\description{
  Graphically display a seasonal sum object, as well as the method of
  solution of the median/quantile \sQuote{normal}
}
\usage{
\method{image}{seas.sum}(x, start, end, var, norm = "days", maxz, nlevels = 128,
      maxa, col = .65, dark = 0, gamma = 0.8, sqrt = FALSE, 
      show.na = TRUE, show.median = TRUE, contour = TRUE, \ldots)
}
\arguments{
  \item{x}{a \code{\link{seas.sum}} object}
  \item{start}{start year; if omitted minimum year will be used}
  \item{end}{end year; if omitted will use same as \code{start}, and if
    \code{start} is omitted, will use maximum year}
  \item{var}{the desired variable to show, otherwise will use the prime
    variable, defined in \code{x}}
  \item{norm}{variable to normalize by, usually \code{"days"}, to
    produce \emph{unit/day}}
  \item{maxz}{maximum value to be displayed}
  \item{nlevels}{number of colour levels}
  \item{maxa}{maximum for annual axis, on right-hand graphs (if
    \code{show.median=TRUE})}
  \item{col}{colour or hue, between [0,1]}
  \item{dark}{darkness of colour}
  \item{gamma}{gamma correction for colour}
  \item{sqrt}{\code{logical}; square root of the values to be taken
    (legend is transposed back to original unit)}
  \item{show.na}{\code{logical}; put red a \sQuote{x} where data are \code{NA}}
  \item{show.median}{\code{logical}; show how the median calculation is
    achieved graphically (computationally it is done using a secant
    method); see \code{\link{seas.norm}} for more information on this
    method}
  \item{contour}{\code{logical}; show contours in lower left-hand plot}
  \item{\ldots}{other arguments passed to \code{\link{.seastitle}} and
    \code{\link{.seascols}} for title style, colours, and other
    customizations to the appearance}
}
\details{
  This is a graphical representation of a \code{\link{seas.sum}} object,
  and is far more informative than a traditional precipitation
  \dQuote{normal} (i.e., \code{\link{precip.norm}} or
  \code{\link{precip.norm}})

  If \code{norm = "days"} and \code{show.median = TRUE} (default), the
  seasonal sums appear in right-hand frames. Horizontal and vertical
  lines indicate a \sQuote{normal} from the image, whereby the sum of
  the quantile is equal to the median of the annual ammount. This
  numerical solution is found using \code{\link{seas.norm}}.
}
\author{M.W. Toews}
\seealso{\code{\link{seas.sum}}, \code{\link{seas.norm}}}
\examples{
data(mscdata)

dat.ss <- seas.sum(mscdata, id=1108447, width="mon")
image(dat.ss)

image(dat.ss,contour=FALSE)

image(dat.ss,norm="active")

}
\keyword{hplot}
