"seas.sum" <-
  function(x, start, end, width = 11, var, prime,
           a.cut = 0.3, na.cut = 0.2, unit = "mm", id, name) {
    cl <- match.call()
    orig <- as.character(substitute(x))[[1]]
    if(!inherits(x,"data.frame"))
      stop(gettextf("%s is not a %s object",
                    sQuote(orig),sQuote("data.frame")))
    if(missing(id)) id <- x$id[1]
    if(missing(start)) start <- NULL
    if(missing(end)) end <- start
    dat <- mksub(x,start,end,id=id)
    n <- names(dat)
    if(!"year" %in% n)
      dat$year <- as.integer(format(dat$date,"%Y"))
    if(!"jday" %in% n)
      dat$jday <- as.integer(format(dat$date,"%j"))
    if(nrow(dat) > 0)
      trange <- as.integer(format(range(dat$date),"%Y"))
    else {
      warning("no data")
      invisible(NA)
    }
    if(missing(var)) # try and find something useful
      var <- c("precip","rain","snow","leak","evap","ezwat","et","runoff","air","soil")
    var <- names(dat)[names(dat) %in% var]
    if(length(var) == 0)
      stop(paste(gettextf("no sum variables were found in %s",orig),
                 gettextf("specify using %s parameter",sQuote("var")),collapse="\n"))
    if(missing(prime)) # use 'precip', or the first variable as 'prime'
      prime <- ifelse(any("precip" %in% var),"precip",var[1])
    else if(!any(prime %in% var))
      warning(paste(gettextf("%s not found in %s",sQuote(prime),sQuote(var)),
                    gettextf("using %s",prime <- var[1]),collapse="\n"))
    if(is.na(a.cut) || a.cut <= 0)
      a.cut = FALSE
    start <- trange[1]
    end   <- trange[2]
    dat$fact <- mkfact(dat,width)
    bins <- levels(dat$fact)
    num <- length(bins)
    years <- as.integer(start:end)
    yearf <- factor(dat$year,levels=years)
    ann <- data.frame(year=years,active=NA,days=NA,na=NA)
    seas <- array(dim=c(length(years),num,length(var)),
                  dimnames=list(years,levels(dat$fact),var))
    days <- array(dim=c(length(years),num),
                  dimnames=list(years,levels(dat$fact)))
    na <- days
    if(is.na(a.cut) || !a.cut) {
      a.cut <- FALSE
    } else {
      active <- seas # copy array
      is.active <- function(test){ # test to count the number of active days
        tot <- numeric(length(test))
        tot[is.na(test)] <- NA # keep NAs
        tot[test > a.cut] <- 1 # find only days where the prime > a.cut
        na.rm <- ifelse(sum(is.na(tot))/length(tot) < na.cut[2],TRUE,FALSE)
        return(sum(tot,na.rm=na.rm))
      }
    }
    if (length(na.cut) == 1) na.cut <- rep(na.cut,2)
    sum.is.num <- function(d) return(sum(!is.na(d),na.rm=TRUE)) # missing days
    ndays<-function(y)(365+ifelse(y%%4==0&y%%100!=0,1,0)+ifelse(y%%400==0,1,0))
    ann$days <- sapply(years,ndays)
    if(a.cut)
      ann$active <- tapply(dat[,prime],yearf,is.active)
    else
      ann$active <- NULL
    ann$na <- tapply(dat[,prime],yearf,sum.is.num)
    for(p in var)
      ann[,p] <- tapply(dat[,p],yearf,sum,na.rm=TRUE)
    td <- function(y) table(mkfact(width=width,year=y))
    days[,] <- t(sapply(years,td))
    for(y in as.character(years)) {
      s <- mksub(dat,as.integer(y))
      if(nrow(s) > 0) {
        na[y,] <- tapply(s[,prime],s$fact,sum.is.num)
        for(p in var) {
          seas[y,,p] <- tapply(s[,p],s$fact,sum,na.rm=TRUE)
          if(a.cut)
            active[y,,p] <- tapply(s[,p],s$fact,is.active)
        }
      }
    }
    ann$na[is.na(ann$na)] <- 0
    ann$na <- ann$days - ann$na
    na[is.na(na)] <- 0
    na <- days - na
    ann.na <- ann$na/ann$days > na.cut[1]
    seas.na <- na/days > na.cut[2]
    ann[ann.na,var] <- NA
    seas[,,var][seas.na] <- NA
    if(a.cut) {
      ann[ann.na,"active"] <- NA
      active[,,var][seas.na] <- NA
    }
    l <- list(ann=ann,seas=seas,active=NA,days=days,na=na)
    if(a.cut)
      l$active <- active
    else
      l$active <- NULL
    l$call <- cl
    l$years <- years
    l$var <- var
    l$prime <- prime
    l$unit <- unit
    l$width <- width
    l$bins <- bins
    l$na.cut <- na.cut
    l$a.cut <- a.cut
    attr(l,"class") <- "seas.sum"
    if(!is.null(id)) {
      l$id <- as.character(id)
      l$name <- getstnname(id)
    }
    if(!missing(name)) l$name <- name
    return(l)
  }
