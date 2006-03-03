"mkfact" <-
  function (dat, width = 11, year) {
    if(missing(dat)) {
      n <- 365 + ifelse(year%%4 == 0 & year%%100 != 0,1,0) + 
        ifelse(year%%400 == 0,1,0)
      dat <- 1:n
    }
    orig <- as.character(substitute(dat))[[1]]
    if(is.character(width)) {
      if(inherits(dat,c("POSIXct","Date"))) {
        date <- dat
      } else if(inherits(dat,"data.frame")) {
        cls <- sapply(dat,class) %in% c("POSIXct","Date")
        if(any(cls))
          date <- dat[,cls,drop=FALSE][[1]]
        else # last try
          date <- as.Date(paste(dat$year,dat$jday),"%Y %j")
      } else if(inherits(dat,c("numeric","integer"))) { # assumed to be jday
        date <- as.Date(paste(year,dat),"%Y %j")
      } else
      stop(gettextf("cannot find a date within %s",orig))
      if(width == "mon") { # locale specific month names
        month.abb <- months(as.Date(paste(2000,1:12,1,sep="-")),TRUE)
        mon <- format(date,"%b")
        return(factor(mon,levels=month.abb))
      } else if(width == "month") {
        month.name <- months(as.Date(paste(2000,1:12,1,sep="-")),FALSE)
        mon <- format(date,"%B")
        return(factor(mon,levels=month.name))
      } else if(width == "DJF") {
        return(factor(floor((as.integer(format(date,"%m"))%%12)/3),labels=c("DJF","MAM","JJA","SON")))
      } else if(width == "JF") {
        return(factor(floor((as.integer(format(date,"%m"))%%12)/2),labels=c("JF","MA","MJ","JA","SO","ND")))
      } else if(width == "JFM") {
        return(factor(floor(((as.integer(format(date,"%m"))-1)%%12)/3),labels=c("JFM","AMJ","JAS","OND")))
      } else if(width %in% c("zod","zodiac")) {
        lab <- c("Aries","Taurus","Gemini","Cancer","Leo","Virgo","Libra","Scorpio","Sagittarius","Capricornus","Aquarius","Pisces")
        if (width == "zod") lab <- abbreviate(lab,3)
        # upper dates for each sign in `mmdd' form; offset by 10 positions
        zod.b <- c(119,218,320,419,520,620,722,823,922,1022,1121,1221)
        md <- as.integer(format(date,"%m%d"))
        zod <- md*NA
        zod[md <= zod.b[1]] <- lab[10] # Capricornus begins
        for(i in 1:length(zod.b))
          zod[md > zod.b[i]] <- lab[(i+9)%%12+1]
        return(factor(zod,lab))
      } else # add other calendar divisions if needed
      stop(gettextf("%s width argument not supported",
                    sQuote(width)))
    }
    if(inherits(dat,c("POSIXct","Date"))) {
      jday <- as.integer(format(dat,"%j"))
    } else if(is.data.frame(dat)) {
      if("jday" %in% names(dat)) {
        jday <- dat$jday
      } else {
        jday <- as.integer(format(dat$date,"%j"))
      }
    } else
    jday <- dat # assumed to be integer
    b <- floor((jday-1)/width + 1) # turn jdays to 0-based, b is 1-based
    bins <- floor(365/width) # 0-based
    if (365/width - bins < 0.2)
      b[b > bins] <- bins # trim last bin
    else
      bins <- bins + 1
    return(factor(b,levels=1:bins))
  }
