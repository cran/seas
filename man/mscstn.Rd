\name{mscstn}
\alias{mscstn}
\docType{data}
\title{Meteorological Survey of Canada station information}
\description{
  Meteorological Survey of Canada (\acronym{MSC}) weather station data,
  including national ID, station ID, Province, latitude and longitude.
}
\usage{data(mscstn)}
\format{
  A \code{\link{data.frame}} with 4493 climate stations with the
  following 6 variables:
  \describe{
    \item{\code{name}}{Full station name}
    \item{\code{nid}}{National ID, alphanumeric key used to distinguish
      unique weather stations}
    \item{\code{sid}}{Station ID, also used for airport codes}
    \item{\code{prov}}{Canadian Province}
    \item{\code{lat}}{Decimal degrees latitude; NAD83}
    \item{\code{long}}{Decimal degrees longitude; NAD83}
  }
}
\details{
  This data object is used as a look-up table to convert a unique
  station identifier (\code{nid}) or ID into a station name, using
  \code{\link{getstnname}}.

  Currently, this data only includes weather stations from Alberta,
  British Columbia and the Yukon.
}
\source{
  Provided by the Meteorological Survey of Canada
  (\url{http://www.msc.ec.gc.ca/}), with permission.
}
\seealso{\code{\link{getstnname}}, \code{\link{mscdata}},
  \code{\link{read.msc}}}
\author{M.W. Toews}
\examples{
data(mscstn)
head(mscstn)
table(mscstn$prov)
plot(lat ~ long, mscstn)
}
\keyword{datasets}
