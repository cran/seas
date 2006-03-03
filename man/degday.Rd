\name{degday}
\alias{degday}
\alias{growday}
\title{Calculate the degree- and grow-days from temperature data}
\description{
  Calculate the degree-days or grow-days from daily temperature data.
}
\synopsis{
degday(dat, cut=18, param="mean_t")
growday(dat, temp=10, trim=30, cday=5, param="mean_t")
}
\usage{
# degree-days
degday(dat, cut = 18, param = "mean_t")

# grow-days
growday(dat, temp = 10, trim = 30, cday = 5, param = "mean_t")
}
\arguments{
  \item{dat}{a \code{data.frame} with temperature data}
  \item{cut}{temperature cut-off, default is 18 degrees C}
  \item{param}{name of temperature parameter}
  \item{temp}{similar to \code{cut}}
  \item{trim}{trim parameter}
  \item{cday}{continuous days}
}
\details{
  This function is not complete. Nor is the documentation.
}
\value{
  A \code{data.frame}.
}
\author{M.W. Toews}
\seealso{\code{\link{interarrival}}}
\examples{
data(mscdata)

}
\keyword{datagen}
\keyword{ts}