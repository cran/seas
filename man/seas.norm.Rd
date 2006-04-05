\name{seas.norm}
\alias{seas.norm}
\alias{precip.norm}
\title{Calculate annual and seasonal `normal' statistics, including
  precipitation normals}
\description{
  Calculates annual and seasonal \sQuote{normal} statistics on a
  \code{\link{seas.sum}} object, including precipitation normals for
  rain, snow and total precipitation.
}
\synopsis{
seas.norm(dat, start, end, var, norm = "days", 
          fun = "median", ann.only = FALSE, precip.norm = FALSE)
precip.norm(dat, start, end, fun="median", \ldots)
}
\usage{
# minimum
seas.norm(dat)

precip.norm(dat)

# all options
seas.norm(dat, start, end, var, norm = "days",
          fun = "median", ann.only = FALSE, precip.norm = FALSE)

precip.norm(dat, start, end, fun="median",\ldots)
}
\arguments{
  \item{dat}{\code{\link{seas.sum}} object}
  \item{start}{start year; if omitted minimum year will be used}
  \item{end}{end year; if omitted will use same as \code{start}, and if
    \code{start} is omitted, will use maximum year}
  \item{var}{variable name for the \sQuote{normal}; if omitted will
    use \code{dat$prime} (the prime variable of the
    \code{\link{seas.sum}} object), or if \code{precip.norm=TRUE} will
    be \code{"precip"}}
  \item{norm}{variable for normalization of the sum, usually the number
    of "days" in each bin, but it can also be "active" to estimate the
    precipitation normal for days of active precipitation}
  \item{fun}{\code{\link{character}} of an existing \code{function}
    object, or a \code{\link{function}} to operate across the number of
    years of observations, usually \code{"mean"} or \code{"median"}
    (default); details described below}
  \item{ann.only}{only annual statistics returned (saves time from other
    calculations)}
  \item{precip.norm}{\code{\link{logical}}; computes precipitation
    normal statistics, which is done slightly differently since it
    involves rain, snow and total precipitation; if TRUE,
    \code{dat$var} must include "rain", "snow" and "precip" summed
    variables}
  \item{\ldots}{parameters to pass to \code{seas.norm} from
    \code{precip.norm}}
}
\details{
  This function calculates the statistics of precipitation data on an
  \emph(annual) and \emph{seasonal} scope from a \code{\link{seas.sum}}
  object.
   
  The seasonal input data are normalized by the number of days in each
  bin, to produce a precipitation rate in \sQuote{mm/day}. This is
  because the number of days in each bin is not equal. The function
  \code{fun} is then applied to the normalized precipitation, and
  operates along each bin, across multiple years of data. The supplied
  function is usually \code{"\link{median}"} or \code{"\link{mean}"},
  but it can also be a built in \R function, such as \code{"\link{var}"}
  for variance, or a composite such as:
\itemize{
  \item \code{function(i,na.rm)(quantile(i,.2,na.rm=na.rm,names=F))} for
  the 20\% quantile, or
  \item
  \code{function(i,na.rm)(mean(i,na.rm=na.rm)/(sd(i,na.rm=na.rm)^3))}
  for skewness.
}

   If \code{fun = "\link{mean}"}, then the statistics are
   straightforward (using \code{\link{apply}}), however if \code{fun =
     "median"} and there are more than 2 years of data, a different
   approach is taken. The \dfn{median} is a special case of the
   \dfn{quantile} function, where the probability is 50\% of the
   population. The \code{\link{median}} and \code{\link{quantile}}
   functions are more resistant to outliers than \code{\link{mean}}, and
   can have advantages on precipitation data.  Precipitation occurring
   at a given time of year does not have a normal distribution since it
   is a value that is not always occurring. It often has a left-skewed
   distribution, consisting of many zero measurements, and few extreme
   precipitation events.
  
  In this function, if \code{fun = "median"} (default) the
  \code{\link{median}} function is only used to calculate the median
  annual precipitation. The \code{\link{quantile}} function is used to
  calculate the seasonal statistics, since the sum of medians applied in
  each bin are less than the median annual precipitation. This is
  because there are usually many measurements of no rain, which skew the
  distribution to the left.  The percentile for the quantile function is
  found using a secant method (Cheny and Kincaid, 1999) such that the
  sum of the quantiles from each bin are equal to the median of the
  annual precipitation.
  
  Snow and rain (which are the two components of precipitation) are
  calculated similarly (if \code{fun = "median"}).  The annual total
  rain and snow amounts are determined by finding the percentile of a
  quantile function where the sum is equal to the median of the annual
  precipitation. The seasonal snow and rain amounts are independently
  found using the same method to find the seasonal precipitation. The
  fraction of the snow in each bin,
  \eqn{snow.frac.b=snow.b/(snow.b+rain.b)} is multiplied by the seasonal
  precipitation to determine the seasonal rain and snow amounts.  This
  is because the sum of rain and snow in each bin does not equal the
  seasonal precipitation. This way, a figure with \code{precip.only =
    TRUE} and \code{= FALSE} will have identical daily precipitation
  rates in each bin.
  
  The pitfalls of calculating precipitation \sQuote{normals} is that it
  assumes that precipitation occurs \emph{every} day at a constant rate
  within each bin. This is not realistic, as the precipitation rates are
  much higher when it is actually occurring.
}
\value{
  Returns a \code{precip.norm} object, which is a \code{\link{list}}
  with the following properties:
  \item{$seas}{
    \code{\link{array}} of seasonal precipitation
    statistics. \code{precip}, \code{rain} and \code{snow} (if
    \code{precip.only = FALSE}) are in \sQuote{mm/day}; \code{freq} and
    \code{na} are the fraction of a day in which precipitation is
    occurring and that data is missing.
  }
  \item{$ann}{
    Annual precipitation statistics. \code{precip}, \code{rain} and
    \code{snow} (if \code{precip.only = FALSE}) are in \sQuote{mm/year};
    \code{active} and \code{na} are the number of days per year which
    are active (for example, days with precipitation) and that data are
    missing.
  }
  \item{$fun}{\code{\link{function}} used in analysis}
  \item{$width}{from \code{dat}}
  \item{$precip.only}{from \code{dat}}
  \item{$range}{\code{c(start,end)} years from \code{dat}}
  \item{$bins}{from \code{dat}}
  \item{$id}{from \code{dat}}
  \item{$name}{from \code{dat}; unless overrided by \code{name} argument}
}
\references{
  Cheny, E. W. and Kincaid, D. 1999, \emph{Numerical Mathematics and
    Computing}, Pacific Grove: Brooks/Cole Pub., 671 p.
  
  Guttman, N.B. 1989, \sQuote{Statistical descriptors of climate},
  \emph{American Meteorological Society}, \bold{70}, 602--607.
}
\author{M.W. Toews}
\note{
  Seasonal data are explicitly normalized to a rate \emph{per day}
  (i.e., mm/day), and not \emph{per month} (i.e., mm/month). This is
  because a time-derivative \emph{per month} has unequal intervals of
  time, ranging between 28 to 31 days. This directly creates up to 10\% error in the analysis between months.
  
  Units for annual normals, however, remain \emph{per year}, since a
  year is a suitable time derivative.
}
\seealso{\code{\link{plot.seas.norm}}, \code{\link{plot.seas.var}},
  \code{\link{precip.dep}}}
\examples{
data(mscdata)

# calculate precipitation normal
dat.ss <- seas.sum(mscdata,id=1108447)
dat.nm <- precip.norm(dat.ss,fun="mean")

# use precipitation normal
dat.dep <- precip.dep(mscdata, dat.nm, id=1108447)
plot(dep ~ date, dat.dep, type="l",
  main="CPD from mean normals")

# plot precipitation normal
plot(dat.nm) # this is the same as plot.precip.norm(dat.nm)
}
\keyword{datagen}
