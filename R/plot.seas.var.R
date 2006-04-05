"plot.seas.var" <-
  function(x, start, end, width=11, var, col = "lightgrey", id,
           ylab, ylim, add.alt, logy=FALSE, ...) {
    orig <- as.character(substitute(x))
    if(!inherits(x,"data.frame"))
  		stop(gettextf("%s is not a %s",
                              sQuote(orig),sQuote("data.frame")))
    if(missing(id)) id <- x$id[1]
    if(missing(start)) start <- NULL
    if(missing(end)) end <- start
    dat <- mksub(x,start,end,id=id)
    if(nrow(dat) > 0)
      trange <- as.integer(format(range(dat$date),"%Y"))
    main <- .seastitle(id=id,orig=orig,range=trange,...)
    xlab <- .seasxlab(width)
    if(nrow(dat) <= 0) {
      frame(); title(main$title); text(.5,.5,gettext("no data"))
      warning("no data")
      invisible(NA)
    }
    if(missing(ylab)) ylab <- var
    dat$fact <- mkfact(dat,width)
    num <- length(levels(dat$fact))
    dat$val <- dat[,var]
    plot.new()
    par(yaxs="i",xaxs="r")
    if(missing(add.alt))
      add.alt <- FALSE
    else {
      if(is.numeric(add.alt) && length(add.alt) == 2) {
        slope <- add.alt[1]
        inter <- add.alt[2]
        add.alt <- TRUE
      } else {
        warning(gettextf("%s must give the slope and intercept\nbetween the primary and secondary axis;\n use %s\n",
                         sQuote("add.alt"),sQuote("add.imp=c(slope,inter)")))
        add.alt <- FALSE
      }
    }
    if(add.alt) {
      mar <- c(5.1,4.1,4.1,4.1)
      bty <- "u"
    } else {
      mar <- c(5.1,4.1,4.1,2.1)
      bty <- "l"
    }
    if(is.null(main$title))
      mar[3] <- 2.1
    par(mar=mar,bty=bty)
    if(missing(ylim)) {
      ylim <- range(dat$val,na.rm=TRUE)
      ylim <- ylim+diff(ylim)*0.04*c(-1,1) # simulate yaxs="r"
      if(logy)
        ylim[1] <- min(dat$val,na.rm=TRUE)
    }
    plot.window(xlim=c(0.5,num+0.5),ylim,log=ifelse(logy,"y",""))
    .seasmonthgrid(width=width,num=num,...)
    pl <- suppressWarnings(boxplot(by(dat,dat$fact,function(x)x$val),
                                   xlab=xlab,ylab=ylab[1],varwidth=TRUE,
                                   add=TRUE,col=col,log=logy))
    title(main=main$title,line=main$line)
    if(add.alt) {
      alt.ax <- pretty(ylim*slope+inter)
      axis(side=4,at=(alt.ax-inter)/slope,lab=alt.ax,srt=90)
      if(!is.na(ylab[2]))
        mtext(ylab[2],side=4,line=2.8)
    }
    invisible(pl)
  }
