\name{sdsm}
\alias{sdsm}
\alias{read.sdsm}
\alias{write.sdsm}
\title{Read and write from SDSM}
\description{
  Reads and writes the data format used in
  SDSM's \file{DAT} and \file{OUT} extensions.
}
\synopsis{
read.sdsm(file, start = 1961, end = 2000, year.length = 366)
write.sdsm(dat, var, start, end, file = "")
}
\usage{
# reading
read.sdsm(file, start = 1961, end = 2000, year.length = 366)
read.sdsm(file)

# writing
write.sdsm(dat, var, start, end, file)
write.sdsm(dat, var)
}
\arguments{
  \item{file}{name of \file{DAT} or \file{OUT} file}
  \item{dat}{\code{data.frame} of variables to be written}
  \item{start}{starting year}
  \item{end}{ending year}
  \item{var}{name of variable to be written from \code{dat}}
  \item{year.length}{maximum allowable year length; this is usually 366
    for measured data, 365 for CCCma model data and 360 for
    Hadley model data}
}
\details{
  This function readings and writes climate data with the
  Statistical Downscaling Model, or SDSM. The model uses \file{DAT}
  extensions for input data, such as daily observations of mean
  temperature, and \file{OUT} extensions for modeled output.
  
  Where \code{year.length} is not equal to either 366 (normal Gregorian) or 365 (no-leap year), the days of the year are calculated as \cr
  \code{floor(seq(1,365,length.out=year.length))}.
}
\value{
  \code{read.sdsm} returns a \code{data.frame} of the measured
  variables. The variables are named \code{V1}\ldots\code{Vn},
  for \emph{n} ensembles.
}
\author{M.W. Toews}
\references{
  Wilby, R.L., Dawson, C.W. and Barrow, E.M. 2002,
  \sQuote{SDSM - a decision support tool for the assessment of regional
    climate change impacts}, \emph{Environmental Modelling Software}, \bold{77}, 473--471.
  
  SDSM can be downloaded free-of-charge for Windows platforms from
  \url{http://www-staff.lboro.ac.uk/~cocwd/SDSM/}
  
  CGCM1 and HADCM3 model data for SDSM can be downloaded from the
  Canadian Climate Impacts and Scenarios website:
  \url{http://www.cics.uvic.ca/scenarios/sdsm/select.cgi}
}
\seealso{\code{\link{read.msc}}}
\examples{
\dontrun{
# reading
gf50 <- read.sdsm("GF_2050s_precip.OUT")
gf50.ss <- seas.sum(gf50,var=paste("V",1:20,sep=""),name="Grand Forks")
# analysis
image(gf50.ss,var="V1")
image(gf50.ss,var="V2")
image(gf50.ss,var="V3")

# writing
data(mscdata)
hj <- mksub(mscdata,id=2100630)
write.sdsm(hj,"precip",1961,2000,"HJ_Obs_prcp.DAT")
}
}
\keyword{file}
\keyword{connection}
\keyword{utilities}
