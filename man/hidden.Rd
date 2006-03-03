\name{hidden}
\alias{.seasxlab}
\alias{.seastitle}
\alias{.seascols}
\alias{.seasmonthgrid}
\title{Get labels and properties for seasonal graphs}
\description{
  Get title, x-axis labels and colours for seasonal graphs. Also, draw
  the month grid.
}
\usage{
.seasxlab(width)
.seastitle(main=NULL, id=NULL, name=NULL, orig=NULL, fun=NULL,
  range=NA, show.range=TRUE, show.id=TRUE, style=c(1,NA), ...)
.seascols(precip.only=NULL, style=c(NA,1), lwd=NULL, ...)
.seasmonthgrid(month.abb=TRUE, month.len=NULL, month.force=FALSE,
  month.draw=TRUE, width, num, month.col="lightgrey", month.lwd=1, ...)
}
\arguments{
  \item{width}{size of bin; see \code{\link{mkfact}}}
  \item{num}{number of bins; if omitted, will calculate from \code{width}}
  \item{main}{main title of plot; overrides any other title, but appends
    year range if \code{show.range=TRUE}}
  \item{id}{station ID, which is used to fetch a station name using
    \code{\link{getstnname}}}
  \item{name}{a name, which is used in preference to the name retrieved
    from \code{getstnname}}
  \item{orig}{original object name, which is used if no other name can
    be found from \code{id} or \code{name}}
  \item{fun}{function, if applicable}
  \item{range}{year range in the format \code{c( start , end )} years}
  \item{show.range}{\code{logical}}
  \item{show.id}{\code{logical}}
  \item{style}{specified as \code{c(title,colours)}, which are
    pre-determined styles for plot, see details below}
  \item{precip.only}{\code{logical}; specify if colours need to be
    produced for precipitation only, or for rain and snow}
  \item{lwd}{line width to be used for plots: for
    \code{\link{plot.seas.temp}}, this changes the width of the diurnal
    variability lines; for \code{\link{image.seas.sum}}, this changes
    the thickness of the median and mean lines}
  \item{month.abb}{\code{logical}; abbreviate month names (using the
    \code{"\%b"} format), or use the normal month names (using the \code{"\%B"})}
  \item{month.len}{length of the month names, for instance, will use
    J-F-M-\ldots (in English locale) if \code{month.len=1}}
  \item{month.force}{\code{logical}; force month labels to be drawn for
    each month (uses \code{\link{mtext}}; labels may overlap); or let
    \code{\link{axis}} place labels (default)}
  \item{month.draw}{\code{logical}; place month labels (grid will be
    drawn regardless)}
  \item{month.col}{colour for month grid; default is \code{"lightgrey"}}
  \item{month.lwd}{line width for month grid; default is \code{1}}
  \item{\ldots}{not used}
}
\details{
  These functions are called by the seasonal plot functions in
  \pkg{seas} for consistent display of title, x-axis labels and
  colours. These functions are ready to have translations for
  non-English languages built into them. Month names currently translate
  according to your operating system and locale settings.
  
  \code{style} can be passed to the \sQuote{\ldots} parameter for most
  plotting functions in \pkg{seas} to modify the look. It can be passed
  in the format \code{c(title,colours)}, where each are integers,
  explained below.

  Title styles:
  \itemize{
    \item \code{1} \sQuote{[\emph{name}] [\emph{id}] [from
      \emph{start} to [\emph{end}]]}
    \item \code{2} \sQuote{[\emph{name}] [\emph{id}] [\code{\n}
      \emph{start} - [\emph{end}]]} (spans two lines)
    \item \code{3} \emph{no title}, \code{par(mar)} is also adjusted to
    acomodate more space for the figure
  }
  Colour styles:
  \itemize{
    \item \code{1}, colour theme: precipitation or rain are
    \sQuote{lightblue}, snow is \sQuote{grey}, temperature boxplots are
    \sQuote{lightgrey}, diurnal temperature lines are \sQuote{red}, wet
    boxplots are \sQuote{lightblue}, dry boxplots are \sQuote{orange},
    and \code{NA} marks are \sQuote{red}
    \item \code{2}, grey-scale theme: precipitation or rain are
    \sQuote{grey80}, snow is \sQuote{grey60}, temperature boxplots are
    \sQuote{grey70}, diurnal temperature lines are \sQuote{grey20}, wet
    boxplots are \sQuote{grey70}, dry boxplots are \sQuote{grey50},
    and \code{NA} marks are \sQuote{grey30}
    \item \code{3} black & white theme: all box-plots are clear, lines
    are all black, precipitation or snow have a line density fill of 20
    and angle of 45, rain has a line density of 10 and an angle of -45
  }

  Also, other details, such as the back-ground colour, font family, may
  be adjusted by setting parameters using the  \code{\link{par}}
  function before plotting. For example, setting \code{par(cex=0.75)}
  will reduce the font size in the active device by  75\% of the
  original size.
}
\value{
  .seasxlab returns a \code{character} label.
  
  .seastitle returns a \code{list}.
}
\author{M.W. Toews}
\seealso{\code{\link{getstnname}}}
\examples{
.seasxlab(11)
.seasxlab("mon")

data(mscdata)
plot.seas.temp(mscdata, width=11, id=1108447,
  style=c(1,1), month.len=1)
plot.seas.temp(mscdata, width=365/12, id=1108447,
  style=c(2,2), month.abb=FALSE)
plot.seas.temp(mscdata, width="mon",  id=1108447,
  style=c(1,3))
}
\keyword{utilities}
