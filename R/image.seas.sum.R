"image.seas.sum" <-
  function(x, start, end, param, norm = "days", maxz, nlevels = 128,
           maxa, col = .65, dark = 0, gamma = 0.8, sqrt=FALSE, 
           show.na=TRUE, show.median=TRUE, contour=TRUE, ...) {
    old.par <- par(no.readonly=TRUE); on.exit(par(old.par))
    orig <- as.character(substitute(dat))
    if(!inherits(x,"seas.sum"))
      stop(gettextf("%s is not a %s object",
                    sQuote(orig),sQuote("seas.sum")))
    dat <- x
    if(missing(param))
      param <- dat$prime
    else if(!(param %in% dat$param))
      stop(gettextf("%s not found in %s",sQuote(param),
                    sQuote(sprintf("%s$param",orig))))
    if(norm != "days") {
      if (!missing(show.median))
        warning(gettextf("option %s only works if %s",
                         sQuote("show.median = TRUE"),
                         sQuote("norm = \"days\"")))
      show.median <- FALSE
    } else if(missing(show.median)) show.median <- TRUE
    if(inherits(norm,"matrix")) {
      if(!all(dim(norm) == dim(dat$seas[1:2])))
        stop(gettextf("%s does not have the same dimensions as %s",
                      sQuote(orig),sQuote("dat$seas[1:2]")))
      dat$days <- norm
    }
    if(inherits(norm,"array")) {
      if(!all(dim(norm) == dim(dat$seas)))
        stop(gettextf("%s does not have the same dimensions as %s",
                      sQuote(orig),sQuote("dat$seas")))
      dat$norm <- norm
    } else {
      if(norm == "active") {
        if(!"active" %in% names(dat))
          stop("there is no %s in %s",sQuote("active"),sQuote("orig"))
        dat$norm <- dat$active
      } else {
        dat$norm <- array(dat$days,dim=dim(dat$seas),
                          dimnames=dimnames(dat$seas))
      }
    }
    if(missing(start)) start <- NULL
    else {
      if(missing(end)) end <- start
      s <- dat$years %in% start:end
      dat$ann <- dat$ann[s,,drop=FALSE]
      dat$seas <- dat$seas[s,,,drop=FALSE]
      dat$norm <- dat$norm[s,,,drop=FALSE]
      dat$na <- dat$na[s,,drop=FALSE]
      dat$days <- dat$active <- NULL
      dat$years <- dat$years[s]
    }
    fun <- sprintf("%s/%s",param,norm)
    if(sqrt)
      fun <- sprintf("sqrt(%s)",fun)
    width <- dat$width
    n.years <- length(dat$years)
    if(n.years == 0) {
      main <- .seastitle(id=dat$id,orig=orig,fun=fun,range=c(start,end),...)
      frame(); title(main$title); text(.5,.5,gettext("no data"))
      warning("no data")
      return(NA)
    }
    dat$range <- range(dat$years)
    start <- dat$range[1]
    end <- dat$range[2]
    main <- .seastitle(id=dat$id,orig=orig,fun=fun,range=dat$range,...)
    num <- length(dat$bins)
    # .b suffix is a matrix of sums in the width of the bin for each year
    param.b <- dat$seas[,,param,drop=TRUE]
    norm.b <- dat$norm[,,param,drop=TRUE]
    seas <- param.b/norm.b
    seas[!is.na(param.b) & norm.b==0] <- 0 # avoid Inf
    if(!missing(maxz)) {
      seas[seas > maxz] <- maxz # trim upper values of maximum
      maxl <- TRUE
    } else {
      maxl <- FALSE
      maxz <- max(seas,na.rm=TRUE)
    }
    levels <- pretty(c(0,maxz),nlevels)
    nlevels <- length(levels)-1
    col.img <- hsv(col,0:nlevels/nlevels,1-dark*0:nlevels/nlevels,gamma)
    col <- .seascols(...)
    if(sqrt) {
      seas.p <- sqrt(seas)
      maxz.p <- sqrt(maxz)
    } else {
      seas.p <- seas
      maxz.p <- maxz
    }
    if(show.median) {
      param.a <- dat$ann[,param]
      nm <- seas.norm(dat,param=param,fun="median")
      nm.quan <- nm$quantile[1,param]
      pr <- 0.8 # percent of window is the main plot on left
      nf <- layout(matrix(c(1,1,1,2,5,4,3,6,4),nrow=3,byrow=TRUE),
                   widths=c(pr,1-pr,lcm(1.6)),heights=lcm(main$height))
      mar2 <- c(1.0,4.1,2.6,0)
      mar3 <- c(2.6,4.1,2.0,0)
      mar4 <- c(1.0,0,2.6,0.6)
      mar5 <- c(2.6,0,2.0,0.6)
    } else {
      nf <- layout(matrix(c(1,1,2,4,3,4),nrow=3,byrow=TRUE),
                   widths=c(1,lcm(1.6)),heights=lcm(main$height))
      mar2 <- c(1.0,4.1,2.6,0.6)
      mar3 <- c(2.6,4.1,2.0,0.6)
    }
    par(mar=c(0,0,0,0))
    frame()
    text(0.5,0.5,main$title,cex=main$cex)
    par(mar=mar2)
    ylab2 <- gettext("year")
    image(1:num,start:end,t(seas.p),col=col.img,xlab=NA,ylab=ylab2,
          xaxt="n",zlim=c(0,maxz.p))
    axis(1,1:num,labels=FALSE)
    if(show.na) {
      na.xy <- which(is.na(t(seas)),TRUE)
      if(!is.null(dim(na.xy)))
        points((1:num)[na.xy[,1]],(start:end)[na.xy[,2]],
               pch=col$na.p,col=col$na)
    }
    box()
    .seasmonthgrid(width=width,num=num,...)
    seas.s <- apply(seas,2,sort,na.last=FALSE)
    if(maxl) seas.s[seas.s > maxz] <- maxz
    if(sqrt) seas.p <- sqrt(seas.s)
    else seas.p <- seas.s
    sam.q <- (1:n.years-1)/(n.years-1)*100
    par(mar=mar3)
    xlab3 <- .seasxlab(width)
    ylab3 <- gettext("sample quantiles (%)")
    image(1:num,sam.q,t(seas.p),xaxt="n",
          zlim=c(0,maxz.p),col=col.img,xlab=xlab3,ylab=ylab3)
    .seasmonthgrid(width=width,num=num,month.draw=FALSE,...)
    if(contour) {
      if(sqrt) seas.p <- seas.p^2
      contour(1:num,sam.q,t(seas.p),add=TRUE)
    }
    if(show.median) {
      abline(h=nm.quan*100,col=col$med,lwd=col$med.lwd)
    }
    axis(3,1:num,dat$bins)
    title(xlab=xlab3,line=0.5)
    box()
    par(mar=c(2.6,.1,2.6,4.1),xaxs="i",yaxs="i",bty="o")
    plot.new()
    plot.window(c(0,1),range(levels))
    rect(0,levels[-length(levels)],1,levels[-1],col=col.img,border=NA)
    if(sqrt) {
      l <- pretty(levels,20)
      ll <- sqrt(l)
      axis(4,ll*max(levels)/max(ll[-length(ll)]),l)
    } else axis(4)
    mtext(gettextf("%s/day",dat$unit),4,2.5)
    box()
    if(show.median) {
      nm.mean <- mean(param.a,na.rm=TRUE)
      nm.median <- nm$ann[1,param]
      ann.a <- apply(seas.s,2,quantile,probs=sam.q/100,na.rm=TRUE)
      days.a <- dat$days
      ann.at <- rowSums(ann.a*days.a,na.rm=TRUE)
      sam.q <- c(sam.q,sam.q[length(sam.q)])
      ann.at <- c(ann.at,ann.at[length(ann.at)])
      if(missing(maxa))
        xlim.a <- c(0,max(param.a,na.rm=TRUE)*0.9+max(ann.at,na.rm=TRUE)*0.1)
      else xlim.a <- c(0,maxa)
      param.a <- c(param.a,param.a[length(param.a)])
      years.a <- dat$years
      years.a <- c(years.a,years.a[length(years.a)]+1)
      par(mar=mar4,bg="white")
      plot(param.a,years.a,type="S",xaxt="n",yaxt="n",xaxs="i",yaxs="i",
           xlim=xlim.a,ylim=c(start,end+1))
      abline(v=nm.mean,col=col$mean,lwd=col$mean.lwd)
      abline(v=nm.median,col=col$med,lwd=col$med.lwd)
      axis(1,labels=FALSE)
      mtext(gettextf("annual (%s)",dat$unit),1,line=1,cex=.8)
      par(mar=mar5,bg="white")
      sam.qp <- seq(0,100,length.out=length(ann.at))
      plot(ann.at,sam.qp,type="S",xaxt="n",yaxt="n",xaxs="i",
           yaxs="i",xlim=xlim.a,ylim=c(0,100))
      abline(v=nm.mean,col=col$mean,lwd=col$mean.lwd)
      abline(v=nm.median,col=col$med,lwd=col$med.lwd)
      abline(h=(nm.quan+(0.5-nm.quan)/n.years)*100,col=col$med,lwd=col$med.lwd)
      axis(1)
      axis(3,labels=FALSE)
    }
  }

