\name{plot.year}
\alias{plot.year}
\title{Plot annual temperature and precipitation statistics}
\description{
  Plots a continuous set of annual temperature and precipitation
  statistics for a single climate station.
}
\synopsis{
plot.year(x, start, end, id, precip.only = FALSE, maxy, ylim,
          units=c("mm","C"), na.cut = 10, ...)
}
\usage{
plot.year(x)

plot.year(x, start, end, id, precip.only = FALSE, maxy, ylim,
          units=c("mm","C"), na.cut = 10, ...)
}
\arguments{
  \item{x}{a \code{data.frame} of climate data}
  \item{start}{starting year; if missing, minimum year will be used}
  \item{end}{ending year; if missing, maximum year will be used}
  \item{id}{unique station identifier used to extract a subset of data
    from \code{dat}}
  \item{precip.only}{only precipitation data is used; rain and snow ignored}
  \item{maxy}{maximum for lower, precipitation graph}
  \item{ylim}{\code{c(min,max)} range of temperature for upper graph}
  \item{units}{native units for \emph{precipitation} and
    \emph{temperature} data}
  \item{na.cut}{minimum number of missing data points in a year to make
    it void; temperature and precipitation are treated independently}
  \item{...}{other arguments passed to \code{\link{.seastitle}} and
    \code{\link{.seascols}} for title style, colours, and other
    customizations to the appearance}
}
\details{
  This simply shows temperature using (\code{\link{boxplot}}s) and
  annual precipitation totals. The red bars are directly proportional to
  the fraction of missing (or \code{NA}) values for the year; statistics
  not shown if there are more than \code{na.cut} \code{NA} values in a
  given year.
}
\author{M.W. Toews}
\seealso{\code{\link{mscdata}}, \code{\link{plot.seas.temp}},
  \code{\link{plot.seas.norm}} (can be used for precipitation normals),
  calculate statistics with \code{\link{tapply}}}
\examples{
data(mscdata)

plot.year(mscdata,id=1108447)
}
\keyword{hplot}
