\name{mscdata}
\alias{mscdata}
\docType{data}
\title{Meteorological Survey of Canada sample climate data}
\description{
  Sample climate data from Meteorological Survey of Canada
  (\acronym{MSC}) climate stations in western Canada.
}
\usage{data(mscdata)}
\format{
  A \code{\link{data.frame}} with 26358 daily observations on the
  following 10 variables (metric units of degrees C and mm/day):
  \describe{
    \item{\code{id}}{\code{\link{factor}} used to distinguish multiple
      stations within a single data frame}
    \item{\code{year}}{\code{\link{integer}} year}
    \item{\code{jday}}{\code{\link{integer}} Julian day; 1-365 or 1-366}
    \item{\code{date}}{\code{\link{Date}} class; useful for plotting a
      continuous time-series}
    \item{\code{max_t}}{Maximum daily temperature}
    \item{\code{min_t}}{Minimum daily temperature}
    \item{\code{mean_t}}{Mean daily temperature}
    \item{\code{precip}}{Total daily precipitation}
    \item{\code{rain}}{Total daily liquid-phase precipitation}
    \item{\code{snow}}{Total daily solid-phase precipitation}
  }
  There are three climate stations in this data frame from:
  \tabular{lll}{
    \bold{ID} \tab \bold{Station Location} \tab \bold{Province}\cr
    1096450   \tab Prince George           \tab \acronym{BC}\cr
    1108447   \tab Vancouver               \tab \acronym{BC}\cr
    2100630   \tab Haines Junction         \tab \acronym{YT}
  }
  
  All data spans from 1975 to 2004 for each station.
}
\details{
  The field \code{id} is optional, but very handy when handling multiple
  stations. The \code{date} column was made using
  \code{dat$date <- as.Date(paste(dat$year,dat$jday),"\%Y \%j")}.
}
\source{
  Data provided by the Meteorological Survey of Canada
  (\url{http://www.msc.ec.gc.ca/}), with permission. This data may only
  be reproduced for personal use; any other reproduction is permitted
  only with the written consent of Environment Canada
  (\url{http://climate.weatheroffice.ec.gc.ca/contacts/}).
}
\seealso{\code{\link{mscstn}}, \code{\link{mksub}},
  \code{\link{mkfact}}, \code{\link{read.msc}}}
\author{M.W. Toews}
\examples{
data(mscstn)
data(mscdata)

head(mscdata)
stnids <- levels(mscdata$id)

# show stations and station names available in this data frame
data.frame(stnids, name=getstnname(stnids))
dat <- mksub(mscdata, id=1108447)
dat$fact <- mkfact(dat, "mon")
plot(mean_t ~ date, data=dat, type="l")
plot(mean_t ~ date, data=dat, subset=fact=="Dec", type="l")
plot.seas.temp(dat)
plot.year(dat)
}
\keyword{datasets}
