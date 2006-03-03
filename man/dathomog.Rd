\name{dathomog}
\alias{dathomog}
\title{Homogenize climate data from two weather stations}
\description{
  Homogenizes daily climate data from two data sets into one data set.
}
\synopsis{
dathomog(dat1, dat2, dat, by = "date", rm.id = TRUE, plot = FALSE)
}
\usage{
dathomog(dat1, dat2, dat, by = "date", rm.id = TRUE, plot = FALSE)

# where both dat1 and dat2 are data.frame objects
dathomog(dat1, dat2, \dots)

# where either dat1 or dat2 is a data.frame, and the other an id
dathomog(dat1, dat2, dat, \dots)

# both dat1 and dat2 are an id in dat
dathomog(dat1, dat2, dat, \dots)
}
\arguments{
  \item{dat1}{\code{data.frame} or an \code{id} within \code{dat}; 1st
    selection}
  \item{dat2}{\code{data.frame} or an \code{id} within \code{dat}; 2nd
    selection}
  \item{dat}{\code{data.frame} with \code{id} column (supply if any of
    \code{dat1} or \code{dat2} are IDs)}
  \item{by}{name of common column, usually \sQuote{date}, which is of
    class \code{\link{Date}}}
  \item{rm.id}{\code{logical}; strip \code{id} column out of resulting
    data frame}
  \item{plot}{\code{logical}; produce cross-plots and correlation
    statistics of the variables between the two data sets}
  \item{...}{optional args}
}
\details{
  Data from \code{dat1} has priority over \code{dat2}.  Where data from
  \code{dat1} is either \code{NA} or missing (outside of range), data
  from \code{dat2} will be used (if available). Parameters will be
  homogenized where their names are identical, found using
  \code{\link{names}}.

  The cross-plots of the data are shown only for interest. They show
  useful correlation statistics, and a best-fit line using perpendicular
  offsets (which are preferred in this case over traditional linear
  regression). At some point, the equations for this line may be used to
  adjust the values from \code{dat2}, however this can always be done
  externally to this function by pre-processing \code{dat2}.
}
\value{
  Returns a \code{data.frame} of climate data required by most functions
  in \pkg{seas}. Parameter names of the structure are found by a
  \code{\link{union}} of the names of \code{dat1} and \code{dat2}. The
  \code{id} column is removed if \code{rm.id = TRUE}.
}
\references{http://mathworld.wolfram.com/LeastSquaresFittingPerpendicularOffsets.html}
\author{M.W. Toews}
\section{Warning}{
  Weather stations should be sufficiently close enough to approximate
  the same weather. This distance depends on the spatial distance and
  local climatology.
}
\examples{
data(mscdata)
plot.year(mscdata,id=2100630)
plot.year(mscdata,id=1108447)
newdata <- dathomog(2100630,1108447,mscdata)
plot.year(newdata)
message("This is a rather poor example, since the two stations are")
message("nowhere near each other")
}
\keyword{utilities}
\keyword{datagen}