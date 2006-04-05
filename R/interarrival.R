"interarrival" <-
  function(dat, start, end, var="precip", p.cut = 0.3, inv = FALSE, id) {
    orig <- as.character(substitute(dat))
    if(!inherits(dat,"data.frame"))
      stop(gettextf("%s is not a %s object",
                    sQuote(orig),sQuote("data.frame")))
    if(!var %in% names(dat))
      stop(gettextf("could not find %s in %s",
                    sQuote(var),
                    sQuote(sprintf("names(%s)",orig))))
    if(missing(id)) id <- unique(dat$id)[1]
    if(missing(start)) start <- NULL
    if(missing(end)) end <- start
    dat <- mksub(dat,start,end,id,rm.id=FALSE)
    dat <- dat[!is.na(dat[,var]),]
    dat$diff <- c(0,as.integer(diff(dat$date)))
    n.dat <- nrow(dat)
    dat$wet <- dat[,var] > p.cut
    if(inv) dat$wet <- !dat$wet
    j <- 0
    date <- dat$date[length=0]
    wet <- dry <- integer(0)
    was.wet <- NA
    for(i in 1:n.dat) {
      if(dat$diff[i] != 1) {
        mode <- TRUE
        if(!is.na(was.wet)) {
          if(was.wet) {
            wet[j] <- NA
          } else {
            dry[j] <- NA
          }
        }
      } else {
        if(mode) {
          j <- j + 1
          mode <- FALSE
        }
        if(dat$wet[i]) {
          if(!was.wet) {
            date[j] <- dat$date[i]
            wet[j] <- 0
          }
          wet[j] <- wet[j] + 1
        } else {
          if(was.wet) {
            j <- j + 1
            date[j] <- NA
            dry[j] <- 0
            wet[j] <- NA
          }
          dry[j] <- dry[j] + 1
        }
      }
      was.wet <- dat$wet[i]
    }
    if(was.wet) wet[j] <- NA
    else dry[j] <- NA
    if(inv){
      inv <- wet
      wet <- dry
      dry <- inv
    }
    s <- !is.na(date) & !(is.na(wet) & is.na(dry))
    d <- data.frame(id=NA,date=date[s],dry=dry[s],wet=wet[s])
    if(is.null(id)) d$id <- NULL
    else d$id <- id
    class(d) <- c("interarrival","data.frame")
    d
  }
