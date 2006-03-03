"mksub" <-
  function(dat, start, end, id, rm.id = TRUE) {
    orig <- as.character(substitute(dat))
    if(!inherits(dat,"data.frame"))
      stop(gettextf("%s is not a %s object",
                    sQuote(orig),sQuote("data.frame")))
    if(!"date" %in% names(dat))
      stop(gettextf("a %s column must exist in %s",
                    sQuote("date"), sQuote(orig)))
    if(!inherits(dat$date,c("POSIXct","Date")))
      stop(gettextf("%s must be either %s or %s class",
                    sQuote(sprintf("%s$date",orig)),
                    sQuote("Date"), sQuote("POSIXct")))
    if("id" %in% names(dat)) {
      if(!missing(id) && !is.null(id)){
        dat <- dat[dat$id == id,]
        if(nrow(dat) <= 0)
          warning(gettextf("no data at %s",
                           sQuote(sprintf("id=%s",id))))
      } else if(length(unique(dat$id)) != 1) {
        id <- unique(dat$id)[1]
        warning(gettextf("multiple IDs found in %s; using first ID: %s",orig,id))
        dat <- dat[dat$id == id,]
        rm.id <- FALSE
      }
    }
    if(rm.id) dat$id <- NULL
    year <- as.integer(format(dat$date,"%Y"))
    if(missing(end)) end <- 0
    if(missing(start) || is.null(start)) {
      start <- min(year)
      end <- max(year)
    }
    if(start > end)
      dat <- dat[year == start,]
    else
      dat <- dat[start <= year & year <= end,]
    return(dat)
  }
