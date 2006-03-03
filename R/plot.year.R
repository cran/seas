"plot.year" <-
  function(x, start, end, id, precip.only = FALSE, maxy, ylim,
           units=c("mm","C"), na.cut = 10, ...) {
    orig <- as.character(substitute(x))
    if(!inherits(x,"data.frame"))
      stop(gettextf("%s is not a %s",
                    sQuote(orig),sQuote("data.frame")))
    old.par <- par(no.readonly=TRUE)
    on.exit(par(old.par))
                                        #mar=c(2.1,4.1,4.1,2.1)
                                        #mai=c(.3,.8,.3,.1)
    par(mfrow=c(2,1),bty="n")
    if(missing(id)) id <- x$id[1]
    if(missing(start)) start <- NULL
    if(missing(end)) end <- start
    dat <- mksub(x,start,end,id=id)
    if(nrow(x) > 0) {
      range <- as.integer(format(range(x$date),"%Y"))
      start <- range[1]
      end <- range[2]
    }
    main <- .seastitle(id=id,orig=orig,range=range,...)
    col <- .seascols(precip.only=precip.only,...)
    if (missing(ylim)) ylim <- range(dat$mean_t,na.rm=TRUE)
    dat$year <- factor(dat$year,start:end)
    years <- end-start+1
    precip <- array(dim=c(years,3),
                    dimnames=list(start:end,c("rain","snow","precip")))
    precip[,2] <- tapply(dat$rain,dat$year,sum,na.rm=TRUE)
    precip[,1] <- tapply(dat$snow,dat$year,sum,na.rm=TRUE)
    precip[,3] <- tapply(dat$precip,dat$year,sum,na.rm=TRUE)
    days <- function(y) (365 + ifelse(y%%4 == 0 & y%%100 != 0,1,0) + ifelse(y%%400 == 0,1,0))
    ndays <- sapply(as.numeric(levels(dat$year)),days)
    sum.is.num <- function(d) return(sum(!is.na(d)))
    precip.na <- tapply(dat$precip,dat$year,sum.is.num)
    temp.na <- tapply(dat$mean_t,dat$year,sum.is.num)
    precip.na[is.na(precip.na)] <- 0
    temp.na[is.na(temp.na)] <- 0
    precip.na <- ndays - precip.na
    temp.na <- ndays - temp.na
    precip.na[precip.na < na.cut] <- NA
    temp.na[temp.na < na.cut] <- NA
    precip.na <- sqrt(precip.na/ndays)
    temp.na <- sqrt(temp.na/ndays)
    if(missing(maxy)) maxy <- max(precip,na.rm=TRUE)
    if(precip.only)
      pl <- t(precip[,3])
    else
      pl <- t(precip[,1:2])
    ylab1 <- gettextf("total precipitation (%s/year)",units[1])
    par(mar=c(1.1,4.1,4.6,0.6))
    barplot(pl,space=0,names.arg=levels(dat$year),ylab=ylab1,ylim=c(0,maxy),
            col=col$precip,density=col$precip.d,angle=col$precip.a)
    title(main=main$title,line=main$line)
    axis(1,1:years-.5,FALSE)
    na.h <- maxy/60
    rect(1:years-.5-precip.na/2,0,1:years-.5+precip.na/2,na.h,
         col=col$na,border=FALSE)
    null.bin <- is.na(tapply(dat$mean_t,dat$year,mean))
    for (i in names(null.bin[null.bin])) {
      dat <- rbind(dat,NA)
      dat$year[nrow(dat)] <- i
      dat$mean_t[nrow(dat)] <- 0 # put 0 in NULL bins to plot boxplots properly
    }
    degSymb <- iconv("\272","latin1","")
    ylab2 <- gettextf("temperature %s%s",degSymb,units[2])
    par(mar=c(1.6,4.1,2.1,0.6))
    boxplot(mean_t ~ year, dat, varwidth=TRUE,ylim=ylim,ylab=ylab2,
            xaxt="n",bty="n",col=col$temp)
    axis(1,1:years,FALSE)
    axis(3,1:years,FALSE)
    abline(h=0)
    na.h <- diff(ylim)/80
    rect(1:years-temp.na/2,-na.h,1:years+temp.na/2,na.h,
         col=col$na,border=FALSE)
  }
