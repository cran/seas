\name{interarrival}
\alias{interarrival}
\title{Calculate the interarrivals between and within precipitation events}
\description{
  Calculate the interarrivals (or spell periods), which are the number
  of days between precipitation events (dry days), and the number of
  days of continuous precipitation (wet days).
}
\synopsis{
interarrival(dat, start, end, param = "precip", p.cut = 0.3,
             inv = FALSE, id)
}
\usage{
# minimum
interarrival(dat)

# with optional arguments
interarrival(dat, start, end, param = "precip", p.cut = 0.3,
             inv = FALSE, id)
}
\arguments{
  \item{dat}{\code{data.frame}, must have at least \code{date} and a
    field to calculate an interarrival with (such as precip)}
  \item{start}{start year; if omitted minimum year will be used}
  \item{end}{end year; if omitted will use same as \code{start}, and if
    \code{start} is omitted, will use maximum year}
  \item{param}{parameter on to which the interarrivals are calculated;
    default is \code{precip}}
  \item{p.cut}{days with precipitation values greater than \code{p.cut}
    are considered to be \emph{wet} days, and the complement are
    \emph{dry} days; a trace amount of 0.3 mm is suggested}
  \item{inv}{\code{logical}; invert convention of the starting date such
    that the date is the first \emph{wet} day if \code{inv=FALSE}
    (default), or the date is the first \emph{dry} day if
    \code{inv=TRUE}}
  \item{id}{unique station identifier used to extract a subset of data
    from \code{dat}}
}
\details{
  The interarrival is the same as the \emph{spell} period (i.e., dry
  spell), however this function simultaneously counts the number of
  \emph{dry} and \emph{wet} days relative to a single date. The date
  represents the first day of precipitation (if \code{inv=TRUE}, this
  convention is inverted to the first day of non-precipitation).
  
  Missing or \code{NA} precipitation values voids the number of counted
  days between and within segments, which implies that days without
  precipitation need to explicitly have zeros.
}
\value{
  \code{interarrival} object (which inherits the \code{data.frame}
  class) with \code{date}, \code{wet}, \code{dry} and \code{fact} fields
  (if supplied with a \code{width} value).
}
\author{M.W. Toews}
\references{
  von Storch, H. and Zwiers, F.W., 1999, \emph{Statistical analysis in
    climate research}, Cambridge: Cambridge University Press, 484 p.
}
\seealso{\code{\link{plot.interarrival}}}
\examples{
data(mscdata)

van.int <- interarrival(mscdata, id=1108447)
plot(van.int, log=FALSE, maxy=30)
}
\keyword{datagen}
\keyword{ts}