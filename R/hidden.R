".seasxlab" <- 
  function(width){
    if(is.numeric(width))
      return(gettextf("%s-day group",round(width,1)))
    if(any(grep("mon",width)))
      return(gettext("Monthly"))
    if(any(grep("zod",width)))
      return(gettext("Zodiac"))
    if(width == "DJF")
      return(gettext("Quarterly (seasonal)"))
    if(width == "JFM")
      return(gettext("Quarterly (annual)"))
    if(width == "JF")
      return(gettext("Two months"))
    return(as.character(width[1]))
  }

".seastitle" <- 
  function(main=NULL, id=NULL, name=NULL, orig=NULL, fun=NULL, range=NA,
           show.range=TRUE, show.id=TRUE, style=c(1,NA),...){
    style = style[1] # first position is for title
    if(is.null(main)) {
      if(is.null(name) && !is.null(id))
        name <- getstnname(id)
      if(!is.null(name) && !is.null(id) && show.id)
        main <- paste(name,id)
      else if(!is.null(name) && !show.id)
        main <- name
      else if(!is.null(id) && show.id)
        main <- id
      else if(!is.null(orig))
        main <- orig[[1]]
      else
        main <- ""
    }
    if(!is.null(fun))
      main <- paste(main,fun,sep=ifelse(main == "",""," "))
    title <- list(title=main)
    title$height <- 1 # for image.seas.sum, in cm
    if(show.range) {
      if(style==1) {
        from <- "\n"
        to <- " - "
        title$line <- 1.5
        title$cex <- 1.5
      } else if(style==2) {
        from <- sprintf(" %s ",gettext("from"))
        to <- sprintf(" %s ",gettext("to"))
        title$line <- 1
        title$cex <- 2
      } else {
        from <- ""
        to <- ""
      }
      if(main =="") {
        from <- ""
        title$line <- 1
        title$cex <- 2
      }
      range <- ifelse(range[1] == range[2],
                      paste(range[1]),
                      paste(range[1],range[2],sep=to))
      title$title <- paste(main,range,sep=from)
    }
    if(style==3){
      title$title <- NULL
      title$height <- .1
    }
    title
  }

".seascols" <-
  function(precip.only=NULL,style=c(NA,1),lwd=NULL,...){
    if(length(style) >= 2)
      style <- style[2] # second position is for colours
    col <- list()
    if(!is.null(precip.only) &&precip.only) {
      if(style == 1 || style == 2)
        col$precip <- "grey"
      else if(style == 3) {
        col$precip <- 1
        col$precip.d <- 20
        col$precip.a <- 45
      }
    }else{ # in the format c(snow,rain)
      if(style == 1) # colour theme ... see colours() for a list of all
        col$precip <- c("grey","lightblue")
      else if(style == 2) # grey-scale theme
        col$precip <- c("grey60","grey80")
      else if(style == 3){ # b/w line theme
        col$precip <- c(1,1) # black lines
        col$precip.d <- c(20,10) # line density
        col$precip.a <- c(45,-45) # line angle
      }
    }
    if(is.null(lwd)) {
      col$med.lwd <- col$mean.lwd <- 1
      col$dtemp.lwd <- 2
    } else {
      col$med.lwd <- col$mean.lwd <- col$dtemp.lwd <- lwd
    }
    if(style == 1) {
      col$temp <- "grey" # for boxplots
      col$dtemp <- "red" # temperature diurnal bars
      col$na <- "red"
      col$na.p <- "x" # pch or symbol
      col$wet <- "lightblue"
      col$dry <- "orange"
      col$med <- "red"
      col$mean <- "orange"
    }else if(style == 2){
      col$temp <- "grey70"
      col$dtemp <- "grey20"
      col$na <- "grey30"
      col$na.p <- "x"
      col$wet <- "grey70"
      col$dry <- "grey50"
      col$med <- "grey30"
      col$mean <- "grey50"
    }else if(style == 3){
      col$temp <- 0
      col$dtemp <- 1
      col$na <- 1
      col$na.p <- "x"
      col$wet <- col$dry <- 0
      col$med <- col$mean <- 1
    }
    col
  }

".seasmonthgrid" <-
  function(month.abb=TRUE, month.len=NULL, month.force=FALSE,
  month.draw=TRUE, width, num, month.col="lightgrey", month.lwd=1, ...){
    if(is.numeric(width)) {
      month <- months(as.Date(paste(2000,1:12,1,sep="-")),month.abb)
      if(is.numeric(month.len))
        month <- strtrim(month,month.len)
      if(missing(num))
        num <- length(levels(mkfact(width=width,year=2000)))
      l <- seq(0.5,num+0.5,length.out=13)
      abline(v=l,col=month.col,lwd=month.lwd)
      m <- l + diff(l)[1]/2
      m <- m[-length(m)]
      if(month.draw){
        if(month.force)
          mtext(month,at=m)
        else
          axis(3,m,month,tick=FALSE,line=-1)
      }
    } 
  }
