"dathomog" <-
  function (dat1, dat2, dat, by = "date", rm.id = TRUE, plot = FALSE) {
    if(is.numeric(dat1)) {
      xlab <- paste(getstnname(dat1),dat1)
      dat1 <- mksub(dat=dat,id=dat1, rm.id=rm.id)
    } else xlab <- substitute(dat1)
    if(is.numeric(dat2)) {
      ylab <- paste(getstnname(dat2),dat2)
      dat2 <- mksub(dat=dat,id=dat2, rm.id=rm.id)
    } else ylab <- substitute(dat2)
    if(rm.id) dat1$id <- dat2$id <- NULL
    n1 <- names(dat1)
    n2 <- names(dat2)
    if(!(by %in% n1 && by %in% n2))
      stop(gettextf("problems encountered while trying to find %s in names of dat1 and dat2 for %s argument\n",
                    sQuote(by),sQuote("by")))
    vars.all <- union(n1,n2) # keep `by' in this one
    vars <- vars.all[!vars.all %in% by] # stip `by' out
    vars.x <- paste(vars,".x",sep="")
    vars.y <- paste(vars,".y",sep="")
    dat <- merge(dat1,dat2,by=by,all=TRUE)
    
    if(plot) {
      require(MASS)
      par(ask=TRUE)
      var <- 1:length(vars)
      var <- var[sapply(dat[,vars.x],is.numeric)]
      RsqrSym <- paste("R",iconv("\272","latin1",""),sep="")
      for(v in var) {
        var <- vars[v]
        x <- as.numeric(dat[,vars.x[v]])
        y <- as.numeric(dat[,vars.y[v]])
        cr <- !is.na(x) & !is.na(y)
        x <- x[cr]; y <- y[cr]
        n <- length(x)
        suppressWarnings(lim <- range(x,y))
        if(n < 1 || diff(lim) == 0) {
          frame()
          title(var,xlab=xlab,ylab=ylab)
          if(n < 1)
            text(.5,.5,gettext("no overlap of data"))
          else
            text(.5,.5,paste(gettext("no variance to data"),
                             sprintf("n = %i; value = %f",x[1]),sep="\n"))
        } else { # calculate least-squares perpendicular offsets line
          B <- 0.5*((sum(y^2) - n*mean(y)^2) - (sum(x^2) - n*mean(x)^2))/
            (n*mean(x)*mean(y) - sum(x*y))
          slope <- (-B + sqrt(B^2 + 1))
          inter <- mean(y) - slope*mean(x)
          rsq <- cor(x,y)^2
          lim <- lim + diff(lim)*(4*c(-1,1)/100) # expand range +/-4%
          image(kde2d(x,y,c(3,3),50,c(lim,lim)),
                col=rev(terrain.colors(30)),xlim=lim,ylim=lim,
                xlab=xlab,ylab=ylab,main=var)
          if (n > 200) { x <- jitter(x); y <- jitter(y) }
          if (n < 1000) points(x,y)
          abline(inter,slope)
          legend(lim[1],lim[2],paste(c("inter. ","slope",paste(RsqrSym,"  "),
                                       "n       "),
                                     round(c(inter,slope,rsq,n),3),sep=": "),
                 bg="white")
        }
      }
    }
    
    by.c <- dat[,by]
    dat.x <- dat[,vars.x]
    dat.y <- dat[,vars.y]
    dat <- data.frame(by=by.c)
    dat$by <- NULL
    dat[,vars.all] <- NA
    dat[,by] <- by.c
    names(dat.x) <- vars
    dat[,vars] <- dat.x
    for(v in 1:length(vars)) {
      dat.na <- is.na(dat[,vars[v]])
      dat[dat.na,vars[v]] <- dat.y[dat.na,vars.y[v]]
    }
    invisible(dat)
  }
