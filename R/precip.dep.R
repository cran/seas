"precip.dep" <-
  function(dat, norm, id) {
    if(!inherits(dat,"data.frame"))
      stop(gettextf("%s is not a %s object",
                    sQuote(orig),sQuote("data.frame")))
    if(!inherits(norm,"seas.norm"))
      stop(gettextf("%s is not a %s object",
                    sQuote(orig), sQuote("seas.norm")))
    if(missing(id)) id <- dat$id[1]
    dat <- mksub(dat,id=id)
    dat$fact <- mkfact(dat,norm$width)
    dat$dep <- dat$precip
    na <- is.na(dat$precip)
    dat$dep[na] <- 0
    for(i in levels(dat$fact)) {
      sl <- dat$fact==i & !na
      n <- norm$seas[i,"precip"]
      dat$dep[sl] <- dat$dep[sl] - n
    }
    dat$dep <- cumsum(dat$dep)
    dat
  }

