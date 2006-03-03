"plot.seas.temp" <-
  function(x, start, end, width=11, id, names=c("min_t","max_t","mean_t"),
           unit="C", add.alt=FALSE, ylim, ...) {
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
    col <- .seascols(...)
    if(nrow(dat) <= 0) {
      frame(); title(main$title); text(.5,.5,gettext("no data"))
      warning("no data")
      invisible(NA)
    }
    if(!unit[1] %in% c("C","F","K"))
      warning(paste(gettextf("%s not recognized",
                             sQuote(sprintf("unit=\"%s\"",unit[1]))),
                    gettext("must be C, F or K"),sep="\n ... "))
    degSymb <- iconv("\272","latin1","")
    ylab <- sprintf("%s %s%s",gettext("temperature"),degSymb,unit)
    names <- intersect(names,names(dat))
    if(length(names) == 3) {
      dat$mean_t <- dat[,names[3]]
    } else if(length(names) == 2) {
      warning("calculating mean temperatures from min and max temperatures")
      dat$mean_t <- rowMeans(dat[,names[1:2]])
    } else
    stop(gettextf("temperature data not found in %s",sQuote(orig)))
    if(nrow(dat) <= 0 || sum(!is.na(dat$mean_t)) <= 0 ) {
      frame(); title(main$title,main$line); text(.5,.5,gettext("no data"))
      return(NA)
    }
    dat$fact <- mkfact(dat,width)
    num <- length(levels(dat$fact))
    plot.new()
    par(yaxs="i",xaxs="r")
    if(add.alt){
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
      ylim <- range(dat$mean_t,na.rm=TRUE)
      ylim <- ylim+diff(ylim)*0.04*c(-1,1) # simulate yaxs="r"
    }
    plot.window(xlim=c(0.5,num+0.5),ylim)
    .seasmonthgrid(width=width,num=num,...)
    pl <- suppressWarnings(boxplot(by(dat,dat$fact,function(x)x$mean_t),
                                   xlab=xlab,ylab=ylab,varwidth=TRUE,add=TRUE,
                                   col=col$temp))
    title(main=main$title,line=main$line)
    # compute mean diurnal variability
    dmin <- tapply(dat[,names[1]],dat$fact,mean,na.rm=TRUE)
    dmax <- tapply(dat[,names[2]],dat$fact,mean,na.rm=TRUE)
    sx <- 1:length(levels(dat$fact))
    segments(sx,dmax,sx,dmin,col=col$dtemp,lwd=col$dtemp.lwd)
    if(unit=="C")
      abline(h=0)
    else if (unit=="F")
      abline(h=32)
    else if (unit=="K")
      abline(h=273.15)
    if(add.alt) {
      f2c <- function(v)(5*(v-32)/9)
      c2f <- function(v)(32+9*v/5)
      if(unit=="C") {
        alt.ax <- pretty(c2f(ylim))
        alt.at <- f2c(alt.ax)
        alt.unit <- "F"
      } else if(unit=="F") {
        alt.ax <- pretty(f2c(ylim))
        alt.at <- c2f(alt.ax)
        alt.unit <- "C"
      } else { # degK
        alt.ax <- pretty(ylim-273.15)
        alt.at <- alt.ax+273.15
        alt.unit <- "C"
      }
      axis(side=4,at=alt.at,lab=alt.ax,srt=90)
      mtext(sprintf("%s%s",degSymb,alt.unit),side=4,line=2.8)
    }
    invisible(pl)
  }
