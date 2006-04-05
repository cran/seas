"plot.seas.norm" <-
  function(x, maxy, varwidth=FALSE, normwidth=FALSE,
           show.na=TRUE, leg, add.alt=FALSE, ...) {
    orig <- as.character(substitute(x))[[1]]
    if(!inherits(x,"seas.norm"))
      stop(gettextf("%s is not a %s object",
                    sQuote(orig), sQuote("seas.norm")))
    dat <- x
    fun <- dat$fun
    precip.norm <- dat$precip.norm
    width <- dat$width
    seas <- dat$seas
    ann <- dat$ann
    var <- dat$var
    main <- .seastitle(id=dat$id,orig=orig,fun=fun,range=dat$range,...)
    xlab <- .seasxlab(width)
    col <- .seascols(...)
    num <- length(dat$bins)
    if(dat$a.cut){
      active <- seas$active
      active[is.na(active)] <- 0
      active[active == 0] <- 0.2 # to show at least a sliver of data
      if(varwidth){ # normalize width of bars
        maxf <- max(seas$active,na.rm=TRUE)
        if(normwidth){
          if(is.logical(normwidth))
            active <- active/max(active,na.rm=TRUE)
          else {
            med.days <- median(table(mkfact(width=width,y=2001)))
            active <- active*med.days/normwidth # assumed numeric
            if (normwidth < maxf)
              warning(paste(gettextf("%s < maximum width value; not a good plot",
                                     sQuote("normwidth")),
                            gettextf(" ... should be no less than %.1f",
                                     round(maxf,1)),sep="\n"))
          }
        }
      } else active <- 1
    } else {
      active <- 1
    }
    unit <- dat$unit
    alt.unit <- ifelse(unit=="mm","in",ifelse(unit=="in","mm",""))
    if(is.null(fun) || fun %in% c("mean","sd","median","min","max","var")){
      ylab <- gettextf("%s/day",unit)
      alt.ylab <- gettextf("%s/day",alt.unit)
    } else alt.ylab <- ylab <- ""
    if(!is.null(fun) && fun == "var") {
      squareSym <- iconv("\262","latin1","")
      ylab <- sprintf("(%s)%s",ylab,squareSym)
      alt.ylab <- sprintf("(%s)%s",alt.ylab,squareSym)
    }
    if(!precip.norm) ylab <- paste(var,ylab)
    if (missing(maxy))
      ylim <- range(pretty(range(0,seas[,var],na.rm=TRUE)))
    else ylim <- c(0,maxy)
    par(yaxs="i",xaxs="r")
    if(add.alt){
      mar <- c(5.1,4.1,4.1,4.1)
      bty <- "u"
    } else {
      mar <- c(5.1,4.1,4.1,2.1)
      bty <- "l"
    }
    if (is.null(main$title))
      mar[3] <- 2.1
    par(mar=mar,bty=bty)
    plot.new()
    plot.window(c(0.5,num+0.5),ylim=ylim)
    .seasmonthgrid(width=width,num=num,...)
    lx <- 1:num - active/2
    rx <- 1:num + active/2
    bot <- 1:num*0
    if(varwidth) border <- FALSE else border <- TRUE
    if(precip.norm){
      # snow boxes
      rect(lx,bot,rx,seas$snow,border=border,
           col=col$precip[1],density=col$precip.d[1],angle=col$precip.a[1])
      # rain boxes
      rect(lx,seas$snow,rx,seas$snow+seas$rain,,border=border,
           col=col$precip[2],density=col$precip.d[2],angle=col$precip.a[2])
    } else {# precipitation only
      rect(lx,bot,rx,seas[,var],border=border,col=col$precip[1])
    }
    if(missing(leg))
      leg <- ifelse(is.null(fun) || fun %in% c("mean","median"),TRUE,FALSE)
    else if(substitute(leg) == "locator") {
      leg <- "locator"
      xy <- locator(1)
      leg.x <- xy$x
      leg.y <- xy$y
    } else if (!is.logical(leg))
      leg <- FALSE
    if(is.character(leg) || leg) {
      if(is.logical(leg)) {
        leg.x <- 0.5+num*.02
        leg.y <- max(ylim)*0.98
      }
      annrate <- paste(unit,gettext("year"),sep="/")
      if(!precip.norm)
        leg.text <- paste(gettext("Total"),var,round(ann[1,var],1),annrate)
      else
        leg.text <- c(gettextf("Rain %.1f",ann$rain),
                      gettextf("Snow %.1f",ann$snow),
                      sprintf("%s %.1f %s",
                              gettext("Total"),ann$precip,annrate))
      if(dat$a.cut) # attach number of active days before rest of legend
        leg.text <- c(gettextf("%s days with %s",round(ann$active,1),
                               ifelse(precip.norm,gettext("precipitation"),
                                      var)),leg.text)
      text(leg.x,leg.y,paste(leg.text,collapse="\n"),adj=c(0,1))
    }
    if(show.na) {
      na <- seas$na
      na.h = max(ylim)/150
      na[na < 0.05] <- NA # don't plot red box if there is < 5% data missing
      rect(1:num - na/2,0,1:num + na/2,na.h,col=col$na,border=FALSE)
    }
    abline(h=0)
    axis(1,1:num,dat$bins)
    axis(2)
    title(main=main$title,line=main$line)
    title(xlab=xlab,ylab=ylab)
    if(add.alt) {
      mm2in <- function(v)(v/25.4)
      in2mm <- function(v)(v*25.4)
      if(unit=="mm") {
        alt.ax <- pretty(mm2in(ylim))
        axis(side=4,at=in2mm(alt.ax),lab=alt.ax,srt=90)
        mtext(alt.ylab,side=4,line=2.8)
      } else if(unit=="in") {
        alt.ax <- pretty(in2mm(ylim))
        axis(side=4,at=mm2in(alt.ax),lab=alt.ax,srt=90)
        mtext(alt.ylab,side=4,line=2.8)
      }
    }
  }
