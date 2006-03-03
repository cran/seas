"plot.interarrival" <-
  function(x, start, end, width = 11, logy=FALSE, maxy, id, ...){
    orig <- as.character(substitute(x))
    if(!inherits(x,"interarrival"))
      stop(gettextf("%s is not an %s object",
                    sQuote(orig),sQuote("interarrival")))
    old.par <- par(no.readonly=TRUE)
    on.exit(par(old.par))
    if(missing(id)) id <- x$id[1]
    if(missing(start)) start <- NULL
    if(missing(end)) end <- start
    dat <- mksub(x,start,end,id=id)
    if(nrow(dat) > 0)
      trange <- as.integer(format(range(dat$date),"%Y"))
    xlab <- .seasxlab(width)
    ylab1 <- gettext("wet days"); ylab2 <- gettext("dry days")
    main <- .seastitle(id=id,orig=orig,range=trange,...)
    col <- .seascols(...)
    wet.ylim <- range(dat$wet,na.rm=TRUE)
    dry.ylim <- range(dat$dry,na.rm=TRUE)
    if(nrow(dat) <= 0) {
      frame(); title(main$title); text(.5,.5,gettext("no data"))
      return(NA)
    }
    if(logy) logy <- "y" else {
      logy <- ""
      wet.ylim[1] <- 0
      dry.ylim[1] <- 0
    }
    if(!missing(maxy)) {
      if(length(maxy) == 1)
        maxy[2] <- maxy
      wet.ylim[2] <- maxy[1]
      dry.ylim[2] <- maxy[2]
    }
    dat$fact <- mkfact(dat,width)
    num <- length(levels(dat$fact))
    par(mfrow=c(2,1)) # does plot.new()
    frame()
    mar.wet <- c(2.1,4.1,4.1,2.1)
    mar.dry <- c(5.1,4.1,1.1,2.1)
    if(is.null(main$title))
      mar.wet[3] <- 2.1
    par(mar=mar.wet,yaxs="i",xaxs="r",bty="l")
    plot.window(c(0.5,num+0.5),ylim=wet.ylim,log=logy)
    .seasmonthgrid(width=width,num=num,...)
    boxplot(wet ~ fact,dat,add=TRUE,ylab=ylab1,
            varwidth=TRUE,col=col$wet)
    lines(1:num,tapply(dat$wet,dat$fact,mean,na.rm=TRUE),lwd=2)
    title(main=main$title,line=main$line)
    frame()
    par(mar=c(5.1,4.1,1.1,2.1),yaxs="i",xaxs="r",bty="c")
    plot.window(c(0.5,num+0.5),ylim=rev(dry.ylim),log=logy)
    .seasmonthgrid(width=width,num=num,month.draw=FALSE,...)
    boxplot(dry ~ fact,dat,add=TRUE,ylab=ylab2,xlab=xlab,
            varwidth=TRUE,col=col$dry)
    lines(1:num,tapply(dat$dry,dat$fact,mean,na.rm=TRUE),lwd=2)
    axis(3,at=1:num,labels=FALSE)
  }

