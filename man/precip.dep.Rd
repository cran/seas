\name{precip.dep}
\alias{precip.dep}
\title{Cumulative precipitation departure}
\description{
  Calculate the cumulative precipitation departure (\acronym{CPD}) for a
  station with a given precipitation normal.
}
\usage{
precip.dep(dat, norm, id)
}
\arguments{
  \item{dat}{\code{data.frame} of climate data}
  \item{norm}{\code{precip.norm} object containing the precipitation
    normal for the same station as \code{dat}}
  \item{id}{unique station identifier used to extract a subset of data
    from \code{dat}; if missing will use \code{norm$id} (if it exists)}
}
\details{
  This function is useful for looking at the behavior of a precipitation
  time-series in relation to its precipitation normal over an extended
  period of time. This is especially useful for identifying changes in
  precipitation, and is useful for relating to groundwater recharge
  patterns
  (i.e. \url{http://wlapwww.gov.bc.ca/rfc/river_forecast/grwater/obsw002.html}).
}
\value{
  Returns a \code{data.frame} with \code{year}, \code{jday},
  \code{date}, \code{precip}, and \code{dep} fields.
}
\references{
  Hodge, W.S., 2001, \emph{A Preliminary Survey of Methyl Tertiary-Butyl
    Ether (\acronym{MTBE}) in Groundwater at Selected Well Sites in
    British Columbia, Canada}, Groundwater Section, Water Protection
  Branch, Ministry of Water, Land and Air Protection.
}
\author{M.W. Toews}
\note{
  The selection of \code{fun} in \code{\link{precip.norm}} will affect
  the result of this function; \code{width} has a small effect.
  
  Periods with missing (\code{NA}) values in \code{dat$precip} will have
  a flat departure, neither increasing nor decreasing.
}
\seealso{\code{\link{precip.norm}}}
\examples{
data(mscstn)
data(mscdata)

d <- mksub(mscdata, id=1108447, rm.id=FALSE)
d.ss <- seas.sum(d)
d.dep <- precip.dep(d,precip.norm(d.ss,fun="mean"))
plot(dep ~ date, d.dep, type="l", main="CPD from mean normals")

d.dep <- precip.dep(d, precip.norm(d.ss,fun="median"))
plot(dep ~ date, d.dep, type="l", main="CPD from median normals")
}
\keyword{manip}
\keyword{datagen}
