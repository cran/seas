"degday" <-
  function(dat,cut=18,param="mean_t") {
    orig <- substitute(dat)[1]
    if(!(param %in% names(dat)))
      stop(gettextf("%s not found in %s",param,orig))
    d <- data.frame(year=unique(dat$year),cooling=NA,heating=NA,na=NA)
    dat$tyear <- factor(dat$year)
    dat$t <- dat[,param] - cut
    dd <- dat[dat$t > 0 & dat$t > -cut,]
    d$cooling <- tapply(dd$t,dd$tyear,sum,na.rm=T)
    dd <- dat[dat$t < 0 & dat$t > -cut,]
    d$heating <- -tapply(dd$t,dd$tyear,sum,na.rm=T)
    d$na <- sapply(d$year,days) - tapply(dat$t,dat$year,function(n) sum(!is.na(n)))
    d$tyear <- NULL
    d
  }
"growday" <-
  function(dat,temp=10,trim=30,cday=5,param="mean_t") {
    orig <- substitute(dat)[1]
    if(!(param %in% names(dat)))
      stop(gettextf("%s not found in %s",param,orig))
    d <- data.frame(year=unique(dat$year),start=NA,end=NA,gdd=NA,na=NA)
    dat$tyear <- factor(dat$year)
    dat$t <- dat[,param]
    dat$t[dat$t > trim] <- trim
    dat$t <- dat$t - temp
    dd <- dat[dat$t > 0 & dat$t > -temp,]
    d$gdd <- tapply(dd$t,dd$tyear,sum,na.rm=T)
    d$na <- sapply(d$year,days) - tapply(dat$t,dat$year,function(n) sum(!is.na(n)))
    d$tyear <- NULL
    dat$t <- dat[,param]
    for (i in d$year) {
      dd <- dat[dat$year == i,]
      dd$up <- cumsum(dd)
    }
    d
  }
