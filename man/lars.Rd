\name{lars}
\alias{lars}
\alias{write.lars}
\alias{read.lars}
\alias{lars2help}
\alias{write.lars.scenario}
\title{Write and read data from LARS-WG}
\description{
  Write and read data from the LARS-WG stochastic weather generator;
  also convert to a format for HELP
}
\synopsis{
write.lars(file = "", dat, var, start, end)
read.lars(file,head=c("year","yday","t_max","t_min","precip","solar"),
           year.offset=0)
lars2help(infile,outfile, year.offset, region)
write.lars.scenario(file, dat1, dat2, name="anomaly")
}
\usage{
write.lars(file = "", dat, var, start, end)
# write observed climate data (*.sr)
write.lars(file = "", dat, var, start, end)

# read synthetic (*.dat) or observed data (*.sr)
read.lars(file,
	head=c("year","yday","t_max","t_min","precip","solar"),
        year.offset=0)

# experimental functions (may not work great; or at all!)
lars2help(infile, outfile, year.offset, region)
write.lars.scenario(file, dat1, dat2, name="anomaly")
}
\arguments{
  \item{file}{name of file; use extension \file{*.sr} for writing
    \sQuote{observed} weather, and \code{read.lars} can read either
    \file{*.sr} observed files or \file{*.dat} synthetically generated
    files; \code{file=""}, will print to console}
  \item{infile}{input file}
  \item{outfile}{output file}
  \item{dat}{\code{data.frame} of climate data}
  \item{dat1}{same as \code{dat}}
  \item{dat2}{same as \code{dat}}
  \item{var}{variables for each column (e.g.,
    \code{c("MAX","MIN","RAIN","RAD")}); if missing, will try and use
    \code{t_min}, \code{t_max}, \code{precip}, and \code{solar} within
    \code{names(dat)}}
  \item{start}{either an \code{integer} starting year (assumed to be 1
    of January), or a \code{\link{Date}} object of the exact date}
  \item{end}{either an \code{integer} ending year (assumed to be 31 of
    December), or a \code{Date} object of the exact date}
  \item{head}{names from \code{file} to be used for \code{dat}}
  \item{year.offset}{offset of years between \code{file} and what is
    needed in \R using \code{Date}}
  \item{region}{region name for HELP file}
  \item{name}{name of scenario}
}
\details{
  These functions interface with the LARS-WG files. The variable headers
  are not located within the \file{*.sr} or \file{*.dat} files, so they
  need to be explicitly defined using the \code{var} parameter. These
  should be the same found in the station file, with \file{*.st}
  extension, which is used exclusively be LARS-WG. However, the
  \sQuote{YEAR} and \sQuote{JDAY} variables do \emph{not} need to be
  defined, since they are assumed to be in the first and second columns,
  respectively, and are automatically converted to and from the \R
  \code{\link{Date}} object.
  
  Data exported from \code{write.lars} always has legal (according to
  the Gregorian calendar) and increasing sequence of days between
  \code{start} and \code{end} (even if there are gaps in
  \code{dat}). Missing data values are written as \code{-99.}, as
  specified in the LARS-WG manual, Version 4.0.

  \code{lars2help} and \code{write.lars.scenario} are experimental
  functions to translate data between LARS and HELP (see
  \code{\link{write.help}} for more info).
}
\author{M.W. Toews}
\references{
  LARS-WG was developed by M.A. Semenov, and can be downloaded for
  academic and research interests from
  \url{http://www.rothamsted.bbsrc.ac.uk/mas-models/larswg.php}
}
\seealso{\code{\link{write.help}}, \code{\link{read.sdsm}}}
%\dontrun{
%# add later
%}
%}
\keyword{file}
\keyword{connection}
\keyword{utilities}
