\name{read.msc}
\alias{read.msc}
\title{Read a MSC archive file into a data.frame}
\description{
  Reads a Meteorological Service of Canada (\acronym{MSC}) digital
  archive files (HLY and DLY formats) into a data.frame.
}
\synopsis{
read.msc(file, flags = FALSE, add.elem, format, verbose = TRUE)
}
\usage{
read.msc(file)

read.msc(file, flags = FALSE, add.elem, format, verbose = TRUE)
}
\arguments{
  \item{file}{file name (with path, if not in \code{\link{getwd}}); it
    can also be a \code{\link{connection}}, such as
    \code{\link{bzfile}}}
  \item{flags}{\code{\link{logical}} return the flags with the
    \code{\link{data.frame}}}
  \item{add.elem}{either a \code{\link{data.frame}} or a
    \code{\link{list}} with additional elements not found in this
    function}
  \item{format}{force this function to read a format (not recommended,
    since this is automatically done)}
  \item{verbose}{\code{\link{logical}} verbose output, such as number of
    stations, elements, records and years in the archive file}
}
\details{
  This function currently reads in HLY (hourly) and DLY (daily) archive
  formats. This is automatically detected. The other formats, FIF
  (fifteen-minute) and MLY (monthly), are not currently supported.
  
  The input file can include multiple stations and multiple elements
  (measured variables). The multiple stations are deciphered through
  the \code{id} column, and the multiple variables appear as columns to
  the output data frame.
  
  This function currently only reads a limited number of elements,
  however additional elements can be used by editing two lines in the R
  source for this function.
}
\value{
  Returns a \code{\link{data.frame}} object with the following minimum fields:
  \item{id}{\code{\link{factor}} used to distinguish multiple stations
    within a single data frame}
  \item{year}{\code{\link{integer}} year}
  \item{jday}{\code{\link{integer}} Julian day; 1--365 or 1--366}
  \item{date}{\code{\link{Date}}, useful for plotting a continuous time-series}
  \item{datetime}{\code{\link{POSIXct}}, includes date and time info,
    only included if \code{file} is in HLY archive format}
  \item{element}{\code{\link{numeric}} \ldots}
  \item{flag}{\code{\link{factor}} \ldots (included if \code{flags=TRUE})}
  
  The are as many \code{element} columns for each element found in the
  archive file, such as:

  \item{max_t}{maximum temperature}
  \item{min_t}{minimum temperature}
  \item{mean_t}{mean temperature}
  \item{rain}{rain}
  \item{snow}{snow}
  \item{precip}{total precipitation}
  \item{snow_d}{snow depth}
  \item{\ldots}{other elements can be added using the \code{add.elem}
    parameter}

Units are in common metric units: mm for precipitation-related
measurements, cm for snow depth, and degrees C for temperature. The
\code{flag} columns are a single character \code{\link{factor}},
described in the \acronym{MSC} Archive documentation.
}
\source{
  Climate data can be requested from \acronym{MSC}, or can be obtained
  directly from the Canadian Daily Climate Data (\acronym{CDCD})
  CD-ROMs, which are available for a free download (procedure described
  in \code{\link{A1128551.DLY}}).
}
\note{
  This function is not optimized, since it uses many \code{\link{for}}
  loops.  It would be nice to have this function operate faster, however
  this is not priority for me right now. If any one is interested in
  optimizing this function (by writing the second half in C or FORTRAN),
  please contact me, and I will supply you with example archive files.
}
\author{M.W. Toews}
\references{
  \url{http://climate.weatheroffice.ec.gc.ca/prods_servs/documentation_index_e.html} documentation for the MSC digital archive formats
  
  \url{http://www.climate.weatheroffice.ec.gc.ca/prods_servs/cdcd_iso_e.html}
  download \acronym{ISO} file images of the CD-ROMs with DLY data
}
\seealso{\code{\link{mscstn}}, \code{\link{mksub}}, \code{\link{mkfact}},
    \code{\link{A1128551.DLY}}}
\examples{
file <- system.file("data/A1128551.DLY",package="seas")
print(file)
dat <- read.msc(file)
print(head(dat))

plot.seas.temp(dat)
plot.year(dat)

# Show how to convert from daily to monthly data
dat$yearmonth <- factor(paste(format(dat$date,"\%Y-\%m"),15,sep="-"))
mlydat <- data.frame(date=as.Date(levels(dat$yearmonth)))
mlydat$year <- factor(format(mlydat$date,"\%Y"))
mlydat$month <- mkfact(mlydat$date,"mon")

# means for temperature data
mlydat$max_t <- as.numeric(tapply(dat$max_t, dat$yearmonth,
                                  mean,na.rm=TRUE))
mlydat$min_t <- as.numeric(tapply(dat$min_t, dat$yearmonth,
                                  mean,na.rm=TRUE))
mlydat$mean_t <- as.numeric(tapply(dat$mean_t, dat$yearmonth,
                                   mean,na.rm=TRUE))
# sums for precipitation-related data

mlydat$rain <- as.numeric(tapply(dat$rain, dat$yearmonth,
                                 sum,na.rm=TRUE))
mlydat$snow <- as.numeric(tapply(dat$snow, dat$yearmonth,
                                 sum,na.rm=TRUE))
mlydat$precip <- as.numeric(tapply(dat$precip, dat$yearmonth,
                                   sum,na.rm=TRUE))
print(head(mlydat),12)

# Show how to convert from a HLY file into daily summaries
\dontrun{
# this is currently shown somewhere at http://www.sfu.ca/~mwtoews/
hlydat <- read.msc(bzfile("HLY11_L1127800.bz2"), flags=TRUE)
hlydat$date <- factor(hlydat$date)

# sum the solar radiation for each day to find the 'total daily'
sumdat <- tapply(hlydat$solar, hlydat$date, sum, na.rm=TRUE)
dlydat <- data.frame(date=as.Date(names(sumdat)),
                     solar=as.numeric(sumdat))

# sum the number of hours without measurements
sumdat <- tapply(hlydat$solar, hlydat$date,
                 function(v)(24 - sum(!is.na(v))))
dlydat$na <- as.integer(sumdat)

# quality control to remove days with less than 4 hours missing
Summerland <- dlydat[dlydat$na < 4,]

plot.seas.var(Summerland,var="solar", col="yellow", width=5,
         ylab="Daily total global solar radiation (W/(m^2*day)")
}
}
\keyword{file}
\keyword{connection}
\keyword{utilities}
