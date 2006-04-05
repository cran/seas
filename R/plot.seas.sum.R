"plot.seas.sum" <-
  function(x, var, norm = "days", maxy, logy = FALSE, col = "lightgrey",
           ...){
    orig <- as.character(substitute(x))[[1]]
    if(!inherits(x,"seas.sum"))
      stop(gettextf("%s is not a %s object",
                    sQuote(orig), sQuote("seas.sum")))
    dat <- x
    if(missing(var))
      var <- dat$prime
    else if(!(var %in% dat$var))
      stop(gettextf("%s not found in %s",sQuote(var),
                    sQuote(sprintf("%s$var",orig))))
    if(inherits(norm,"matrix")) {
      if(!all(dim(norm) == dim(dat$seas)))
        stop(gettextf("%s does not have the same dimensions as %s",
                      sQuote(orig),sQuote("dat$seas[1:2]")))
      dat$norm <- norm
    } else {
      if(norm == "active") {
        if(!"active" %in% names(dat))
          stop("there is no %s in %s",sQuote("active"),sQuote("orig"))
        dat$norm <- dat$active[,,var]
      } else {
        dat$norm <- dat$days
      }
    }
    val.m <- dat$seas[,,var]/dat$norm # normalize and deconstruct array
    val.m[!is.finite(val.m)] <- NA
    val.d <- data.frame(val.m)
    names(val.d) <- dat$bins
    val <- stack(val.d)
    val$ind <- factor(val$ind,levels=dat$bins) # correct order
    num <- length(dat$bins)
    main <- .seastitle(id=dat$id,orig=orig,range=range(dat$years),...)
    xlab <- .seasxlab(dat$width)
    plot.new()
    par(yaxs="i",xaxs="r")
    ylab <- gettextf("%s/day",dat$unit)
    #if(add.alt) {
    #  mar <- c(5.1,4.1,4.1,4.1)
    #  bty <- "u"
    #  alt.ylab <- gettextf("%s/day",alt.unit)
    #} else {
    mar <- c(5.1,4.1,4.1,2.1)
    bty <- "l"
    #}
    if(is.null(main$title))
      mar[3] <- 2.1
    par(mar=mar,bty=bty)
    if (missing(maxy))
      ylim <- range(pretty(range(0,val$values,na.rm=TRUE)))
    else ylim <- c(0,maxy)
    plot.window(xlim=c(0.5,num+0.5),ylim=ylim,log=ifelse(logy,"y",""))
    .seasmonthgrid(width=dat$width,num=num,...)
    pl <- suppressWarnings(boxplot(by(val,val$ind,function(x)x$values),
                                   xlab=xlab,ylab=ylab[1],varwidth=TRUE,
                                   add=TRUE,col=col))
    title(main=main$title,line=main$line)
    #if(add.alt) {
    #  alt.ax <- pretty(ylim*slope+inter)
    #  axis(side=4,at=(alt.ax-inter)/slope,lab=alt.ax,srt=90)
    #  if(!is.na(ylab[2]))
    #    mtext(ylab[2],side=4,line=2.8)
    #}
    invisible(pl)
  }
