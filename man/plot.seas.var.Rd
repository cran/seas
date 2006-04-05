\name{plot.seas.var}
\alias{plot.seas.var}
\title{Plot seasonal normals of a given variable}
\description{
  Plot seasonal normals using boxplots of a given variable.
}
\synopsis{
plot.seas.var(x, start, end, width=11, var,
  col = "lightgrey", id, ylab, ylim, add.alt, logy=FALSE,...)
}
\usage{
plot.seas.var(x, var)

plot.seas.var(x, start, end, width=11, var, 
  col = "lightgrey", id, ylab, ylim, add.alt, logy=FALSE,...)
}
\arguments{
  \item{x}{a \code{data.frame}}
  \item{start}{start year; if omitted minimum year will be used}
  \item{end}{end year; if omitted will use same as \code{start}, and if
    \code{start} is omitted, will use maximum year}
  \item{width}{size of bin; see \code{\link{mkfact}}}
  \item{var}{a variable; a column name in \code{x}}
  \item{col}{colour for the boxplots; default is \code{"lightgrey"}}
  \item{id}{unique station identifier used to extract a subset of data
    from \code{x}}
  \item{ylab}{label for the y-axis; if there is an alternative y-axis,
    this can have a length of two for the second label}
  \item{ylim}{\code{c(min,max)} range for y-axis}
  \item{add.alt}{this adds an alternative axis, and is specified by
    \code{c(slope,inter)}; for example, if the primary measure is in
    degrees C, a secondary scale in K would be \code{c(1, 273.15)}, or
    in degrees F would be \code{c(5/9, 32)}}
  \item{logy}{\code{logical}; makes a log y-scale}
  \item{...}{other arguments passed to \code{\link{.seastitle}} for
    title style, and other customizations to the appearance}
}
\details{
  Shows normals of a seasonal variable using boxplots.
}
\value{
  Returns values from \code{\link{boxplot}} statistics on the variable.
}
\author{M.W. Toews}
\seealso{\code{\link{plot.seas.norm}}, \code{\link{plot.year}}}
\examples{
data(mscdata)

plot.seas.var(mscdata, var="max_t", col="tomato", id=1108447,
  add.alt=c(5/9,32), ylab=c("Maximum temperature, C","F"))
abline(h=0)

plot.seas.var(subset(mscdata,precip>0), var="precip",
  col="azure", id=1108447)
}
\keyword{hplot}
