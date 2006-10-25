\name{plot.seas.var}
\alias{plot.seas.var}
\encoding{latin1}
\title{Plot seasonal normals of a given variable}
\description{
  Plot seasonal normals of a variable using boxplots.
}
\synopsis{
plot.seas.var(x, var, width=11, start=1, rep=0, start.day=1,
              col="lightgrey", ylim, add.alt, alt.ylab, main,
              ...)
}
\usage{
plot.seas.var(x, var)

plot.seas.var(x, var, width=11, start=1, rep=0, start.day=1,
              col="lightgrey", ylim, add.alt, alt.ylab, main,
              \ldots)
}
\arguments{
  \item{x}{a \code{data.frame} with \code{\link{Date}} and \code{var}
    columns of data; \code{x} can also have \code{id} or \code{name}
    \link{attributes} to help give a title for the plot}
  \item{var}{a variable; a column name in \code{x}; this can also have
    attributes of \code{units} and \code{long.name} to help give a title
    for the y-axis}
  \item{width}{size of bin; see \code{\link{mkseas}}}
  \item{start}{starting bin number; e.g., if \code{width="mon"} and
    \code{start=5}, the plot will start on "May" at the left-hand side}
  \item{rep}{repetition of the bins in the boxplot}
  \item{start.day}{when \code{width} is numeric, this is the starting
    day of the year for the fist bin, or it can be a \code{Date} to
    specify a month and day (year is ignored)}
  \item{col}{colour for the boxplots; the default is \code{"lightgrey"}}
  \item{ylim}{\code{c(min,max)} range for y-axis}
  \item{add.alt}{this adds an alternative axis, and is specified by
    \code{c(slope,inter)}; for example, if the primary measure is in
    \enc{°}{degrees }C, a secondary scale in K would be
    \code{c(1,273.15)}, or in \enc{°}{degrees }F would be
    \code{c(5/9,32)}}
  \item{alt.ylab}{label for the alternate y-axis (the primary y-axis
    label is set through \link{attributes} for \code{var} in \code{x})}
  \item{main}{main title for plot, otherwise this will be automatically generated}
  \item{\ldots}{ignored}
}
\details{
  Shows normals of a seasonal variable using boxplots.
}
\value{
  Returns values from \code{\link{boxplot}} statistics on the variable.
}
\author{M.W. Toews}
\seealso{
  \code{\link{plot.seas.norm}}, \code{\link{plot.year}}.

  Use \code{mksub} to make a subset of \code{x}.
}
\examples{
data(mscdata)
dat <- mksub(mscdata,id=1108447)
mar <- par("mar")

plot.seas.var(dat, var="t_max", col="tomato",
  add.alt=c(5/9,32), alt.ylab="F")
abline(h=0)

par(mar=mar) # return margin back
plot.seas.var(dat, var="t_min",
  start=18,rep=16)

pdat <- dat[dat$precip>0,]
attr(pdat$precip,"long.name") <- "precipitation intensity"
attr(pdat$precip,"units") <- "mm/day"

plot.seas.var(pdat, var="precip", col="azure")

}
\keyword{hplot}
