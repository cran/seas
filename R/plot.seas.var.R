"plot.seas.var" <-
  function(x, var, width=11, start=1, rep=0, start.day=1,
           col="lightgrey", ylim, add.alt, alt.ylab, main, ...) {
    orig <- as.character(substitute(x))[[1]]
    if(missing(var))
      stop(gettextf("%s must be supplied",sQuote("var")))
    sc <- seas.df.check(x,orig,var)
    x$var <- x[[var[1]]]
    if(missing(main))
      main <- sc$main
    x$fact <- mkseas(x,width,start.day)
    days <- attr(x$fact,"bin.lengths")
    num.fact <- length(levels(x$fact))
    num <- num.fact + rep
    if(start<1||start>=num.fact) {
      warning("'start' should be >= 1 and < to the number of bins")
      start <- 1
    }
    if(start > 1) # re-order
      x$fact <- factor(x$fact,levels(x$fact)[(1:num.fact-2+start)%%num.fact+1])
    xlab <- .seasxlab(width,start.day)
    at.label <- ((1:num)+start-2)%%num.fact+1
    if(!is.numeric(width))
      at.label <- levels(x$fact)[at.label]
    ylab <- sc$ylab
    if(missing(add.alt))
      add.alt <- FALSE
    else {
      if(is.numeric(add.alt) && length(add.alt) == 2) {
        slope <- add.alt[1]
        inter <- add.alt[2]
        add.alt <- TRUE
      } else {
        warning(gettextf("'add.alt' must give the slope and intercept\nbetween the primary and secondary axis;\n use %s\n",
                         sQuote("add.imp=c(slope,inter)")))
        add.alt <- FALSE
      }
    }
    mar <- par("mar")
    ylog <- par("ylog")
    if(add.alt) {
      if(missing(alt.ylab))
        alt.ylab <- NULL
      else
        mar[4] <- mar[2] # copy left-hand side margin
      bty <- "u"
    } else {
      bty <- "l"
    }
    par(mar=mar,bty=bty,yaxs="i",xaxs="r")
    if(missing(ylim)) {
      ylim <- range(x$var,na.rm=TRUE)
      ylim <- ylim+diff(ylim)*0.04*c(-1,1) # simulate yaxs="r"
      if(ylog)
        ylim[1] <- min(x$var,na.rm=TRUE)
    }
    frame()
    plot.window(xlim=c(0.5,num+0.5),ylim)
    .seasmonthgrid(width,days,start,rep,start.day)
    varwidth = TRUE
    seas.boxplot <- function(at){
      pl <- boxplot(by(x,x$fact,function(x)x$var),at=at,
                    col=col,log=ylog,varwidth=varwidth,
                    names=NA,add=TRUE)
      invisible(pl)
    }
    pl <- seas.boxplot(1:num.fact)
    if(rep>0) {
      n <- 1
      if(rep>=num.fact) { # whole repetitions
        for(n in 1:floor(rep/num.fact))
          seas.boxplot((num.fact*n+1):(num.fact*(n+1)))
        n <- n+1
      }
      if(rep%%num.fact!=0) { # non-whole repetitions
        lv <- levels(x$fact)[1:(rep%%num.fact)]
        x <- x[x$fact %in% lv,]
        x$fact <- factor(x$fact,lv)
        seas.boxplot((num.fact*n+1):(num.fact*n+rep%%num.fact))
      }
    }
    axis(1,1:num,at.label)
    title(main,xlab=xlab,ylab=ylab)
    if(add.alt) {
      alt.ax <- pretty(ylim*slope+inter)
      axis(side=4,at=(alt.ax-inter)/slope,lab=alt.ax,srt=90)
      if(!is.null(alt.ylab))
        mtext(alt.ylab,side=4,line=par("mgp")[1])
    }
    invisible(pl)
  }
