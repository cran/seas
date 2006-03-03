\name{mksub}
\alias{mksub}
\title{Make a subset of climate data}
\description{
  Creates a subset of a \code{data.frame} of climate data.
}
\usage{
  mksub(dat, start, end, id, rm.id = TRUE)
}
\arguments{
  \item{dat}{climate data frame}
  \item{start}{start year; if omitted minimum year will be used}
  \item{end}{end year; if omitted will use same as \code{start}, and if
    \code{start} is omitted, will use maximum year}
  \item{id}{unique station identifier used to extract a subset of data
    from \code{dat}}
  \item{rm.id}{\code{\link{logical}}; removes \code{id} field from
    \code{dat}, default is \code{TRUE}}
}
\details{
  This utility function is useful for creating temporal subsets of
  climate data and for extracting a single station out of a
  \code{data.frame} with multiple climate stations. The \code{dat}
  object can have other fields (other than the standard set of fields),
  which will be returned.
  
  If \code{id} is used, that station will be extracted from
  \code{dat}. The returned object will no longer have an \code{id}
  field, unless \code{rm.id = FALSE}. If \code{id} is not provided, but
  there are more than one unique IDs in \code{dat}, the first unique ID
  will be extracted, with a warning.
}
\value{
  Returns a subset of a \code{\link{data.frame}} with the same fields as
  \code{dat}, except for \code{id} (if it is present and \code{rm.id =
    TRUE}).
}
\author{M.W. Toews}
\seealso{\code{\link{read.msc}}}
\examples{
data(mscstn)
data(mscdata)

dim(mksub(mscdata, id=1108447))
dim(mksub(mscdata, id=1108447, start=1980))
dim(mksub(mscdata, id=1108447, start=1980, end=1989))
}
\keyword{datagen}
\keyword{utilities}
