\name{summerland}
\alias{summerland}
\alias{summerland.sr}
\alias{summerland.dat}
\docType{data}
\title{Example LARS-WG data file of synthetic data from Summerland, BC}
\description{
  Example LARS-WG data file of synthetic data from Summerland, BC.
}
%\usage{data(mscdata)}
\format{
  Both files are ASCII-based, and can be viewed in any text editor
  \description{
    \item \file{summerland.sr} is the \sQuote{site file}, which contains
    the meta-data
    \item \file{summerland.dat} is the data file
  }
  Details of these file formats can be found in the LARS-WG manual and
  help documentation.
}
\details{
  The sample file name was generated in LARS-WG from calibration of data
  from Summerland (MSC ID: 1127800). Thirty-years were generated.
}
\seealso{\code{\link{read.lars}}}
\author{M.W. Toews}
\examples{
stfile <- system.file("data/summerland.st",package="seas")
print(stfile)
summ <- read.lars(stfile,year.offset=1960)
head(summ)
str(summ)

# plot temperature
summ$t_mean <- rowMeans(summ[,c("t_min","t_max")])
plot.seas.temp(summ)

# plot solar radiation
plot.seas.var(summ,"solar")

# plot precipitation
summ.ss <- seas.sum(summ)
image(summ.ss)
plot(seas.norm(summ.ss))
}
\keyword{datasets}
