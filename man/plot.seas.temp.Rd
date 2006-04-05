\name{plot.seas.temp}
\alias{plot.seas.temp}
\title{Plot seasonal temperature normals}
\description{
  Plot seasonal temperature normals using boxplots, and also plot
  seasonal diurnal variability between minimum and maximum temperature.
}
\synopsis{
plot.seas.temp(x, start, end, width=11, id,
names=c("min_t","max_t","mean_t"), unit="C", add.alt=FALSE, ylim, ...)
}
\usage{
plot.seas.temp(x)

plot.seas.temp(x, start, end, width=11, id,
               names=c("min_t","max_t","mean_t"),
               unit="C", add.alt=FALSE, ylim, ...)
}
\arguments{
  \item{x}{\code{data.frame} of climate data}
  \item{start}{start year; if omitted minimum year will be used}
  \item{end}{end year; if omitted will use same as \code{start}, and if
    \code{start} is omitted, will use maximum year}
  \item{width}{size of bin; see \code{\link{mkfact}}}
  \item{id}{unique station identifier used to extract a subset of data
    from \code{x}}
  \item{names}{array specifying the names of the columns in \code{x}
    which relate to the \emph{minimum}, \emph{maximum} and \emph{mean
      temperatures}}
  \item{unit}{native temperature units of the data; one of either C, F or K}
  \item{add.alt}{\code{logical}; add an alternative scale: if
    \code{unit} is C, the alternative is F; if \code{unit} is F, the
    alternative is C; and if \code{unit} is K, the alternative is C}
  \item{ylim}{\code{c(min,max)} range for y-axis}
  \item{...}{other arguments passed to \code{\link{.seastitle}} and
    \code{\link{.seascols}} for title style, colours, and other
    customizations to the appearance}
}
\details{
  Plots boxplots for seasonal temperature normals from mean daily
  temperature, and diurnal variability with the mean difference of daily
  minimum and maximum temperatures (red vertical lines).
}
\value{
  Returns values from \code{\link{boxplot}} statistics on mean temperature.
}
\author{M.W. Toews}
\seealso{\code{\link{plot.seas.var}}, \code{\link{plot.seas.norm}},
  \code{\link{plot.year}}}
\examples{
data(mscdata)

plot.seas.temp(mscdata,id=1108447)
plot.seas.temp(mscdata,id=1108447, width="mon", add.alt=TRUE)
}
\keyword{hplot}
