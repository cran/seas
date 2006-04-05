"seas.norm" <-
  function(dat, start, end, var, norm="days", fun="median",
           ann.only=FALSE, precip.norm=FALSE) {
    orig <- substitute(dat)
    if(!inherits(dat,"seas.sum"))
      stop(gettextf("%s is not a %s object",
                    sQuote(orig), sQuote("seas.sum")))
    if(missing(var))
      var <- ifelse(precip.norm,"precip",dat$prime)
    if(!(var %in% dat$var))
      stop(gettextf("%s not found in %s",
                    sQuote(var), sQuote(sprintf("%s$var",orig))))
    if(!ann.only){
      if(inherits(norm,"matrix")) {
        if(!all(dim(norm) == dim(dat$seas[1:2])))
          stop(gettextf("%s does not have the same dimensions as %s",
                        sQuote(orig),sQuote("dat$days")))
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
        } else if(!"norm" %in% names(dat)){
          dat$norm <- array(dat$days,dim=dim(dat$seas),
                            dimnames=dimnames(dat$seas))
        }
      }
    }
    if(precip.norm && (any(!c("rain","snow") %in% dat$var))) {
      warning(paste(gettextf("%s does not have either %s or %s variables, or both",
                             sQuote(orig),sQuote("rain"),sQuote("snow")),
                    gettextf("using only %s",var),sep="\n"))
      precip.norm <- FALSE
    }
    if(missing(start) || is.null(start)) start <- NULL
    else {
      if(missing(end)) end <- start
      s <- dat$years %in% start:end
      dat$ann <- dat$ann[s,]
      if(!ann.only){
        dat$seas <- dat$seas[s,,,drop=FALSE]
        dat$norm <- dat$norm[s,,,drop=FALSE]
        dat$na <- dat$na[s,,drop=FALSE]
        dat$days <- dat$days[s,,drop=FALSE]
        if(dat$a.cut)
          dat$active <- dat$active[s,,,drop=FALSE]
      }
      dat$years <- dat$years[s]
    }
    n.years <- length(dat$years)
    if(n.years == 0) {
      warning("no data")
      invisible(NA)
    }
    if(is.function(fun)) fun <- as.character(substitute(fun))[[1]]
    width <- dat$width
    dat$range <- range(dat$years)
    num <- length(dat$bins)
    # determine annual normals
    ann <- data.frame(var=NA)
    var.a <- dat$ann[,var]
    if(any(!is.na(var.a))) { # are values to calculate annual normals?
      ann$var <- eval(call(fun,var.a,na.rm=TRUE))
      if(precip.norm) {
        rain.a <- dat$ann$rain
        snow.a <- dat$ann$snow
        ann$rain <- eval(call(fun,rain.a,na.rm=TRUE))
        ann$snow <- eval(call(fun,snow.a,na.rm=TRUE))
      }
      ann$days <- eval(call(fun,dat$ann$days,na.rm=TRUE))
      ann$active <- NA
      ann$na <- eval(call(fun,dat$ann$na,na.rm=TRUE))
    } else { # an incomplete year
      ann$var <- NA
      if(precip.norm)
        ann$snow <- ann$rain <- NA
      ann$active <- ann$days <- NA
      ann$na <- NA
      warning(gettext("not enough data to determine annual normals"))
      if (fun == "median") {
        warning(gettextf("changing %s from %s to %s",
                         sQuote("fun"),sQuote(fun),sQuote("mean")))
        fun <- "mean"
      }
    }
    if(dat$a.cut) {
      active.a <- dat$ann$active
      ann$active <- ifelse(any(!is.na(active.a)),
                           eval(call(fun,active.a,na.rm=TRUE)),0)
    } else {
      ann$active <- NULL
    }
    if(!ann.only) {
      # .b suffix is a matrix of sums of bin for each year
      var.b <- dat$seas[,,var] # drop=TRUE is default
      norm.b <- dat$norm[,,var]
      var.n <- dat$seas/dat$norm # normalized
      if(dim(var.n)[1] == 1)
        var.n <- var.n[1,,]
      var.n[dat$norm <= 0] <- 0
      if(precip.norm) {
        rain.b <- dat$seas[,,"rain"]
        snow.b <- dat$seas[,,"snow"]
      }
      seas <- data.frame(var=1:num*NA,row.names=dat$bins)
      if(fun == "median" && n.years <= 2) {
        if(n.years != 1)
          warning("not enough years to find the median; using mean")
        fun <- "mean"
      }
      if(n.years == 1) {
        seas$var <- var.n[,var]
        if(precip.norm) {
          seas$rain <- var.n[,"rain"]
          seas$snow <- var.n[,"snow"]
        }
        if(dat$a.cut)
          seas$active <- dat$active[1,,var]/norm.b
        seas$days <- dat$days[1,]
        seas$na <- dat$na[1,]/norm.b
      }else if(fun == "median" && n.years > 2) {
        quan <- data.frame(var=NA)
        secant <- function (f) {
        # Secant method to find a root; f is a function which needs to
        # be zero, specifically a quantile in [0,1]
        # Cheny, E. W. and Kincaid, D. 1999
          a <- 0.45; b <- 0.55 # start around the 50% quantile or true median
          fa <- f(a); fb <- f(b)
          while (fa == fb && b < 1) # if there is no slope around initial guess
            fb <- f(b <- b + 0.05)
          for (i in 1:50) {
            if (a == b) stop("can not converge; outside quantile bounds [0,1]")
            if (abs(fa) > abs(fb)) { # swap
              z <- a;   a <- b;   b <- z
              fz <- fa; fa <- fb; fb <- fz
            }
            d <- (b - a)/(fb - fa)
            b <- a; fb <- fa
            d <- d*fa
            if (abs(fa) < 0.0001) return(a) # normal exit point for function
            a <- a - d
            if(a < 0) a <- 0 # fix the bounds between [0,1]
            if(a > 1) a <- 1
            fa <- f(a)
          }
          warning(gettextf("no convergence of quantile; %f > 0.0001",abs(fa)))
          return(a)
        }
        quan$var <- secant(function(qu)
                             return(ann$var - sum(apply(var.b,2,
                                                          quantile,qu,
                                                          na.rm=TRUE,
                                                          names=FALSE))))
        seas$var <- apply(var.n[,,var],2,
                            quantile,quan$var,
                            na.rm=TRUE,names=FALSE)
        if(precip.norm) {
          quan$snow <- quan$rain <- NA
          # calculate the quantile of annual rain + snow, which equals
          # the anual precipitation
          quan$rainsnow <- secant(function(qu)
                                  return(ann$var
                                         - quantile(rain.a,qu,
                                                    na.rm=TRUE,
                                                    names=FALSE)
                                         - quantile(snow.a,qu,
                                                    na.rm=TRUE,
                                                    names=FALSE)))
          rain.am <- quantile(ann$rain,quan$rainsnow,
                              na.rm=TRUE,names=FALSE)
          # calculate the quantile of all rain data in the bins to
          # equal the annual rain volume
          quan$rain <- secant(function(qu)
                              return(rain.am - sum(apply(rain.b,2,
                                                         quantile,qu,
                                                         na.rm=TRUE,
                                                         names=FALSE))))
          seas$rain <- apply(var.n[,,"rain"],2,
                             quantile,quan$rain,
                             na.rm=TRUE,names=FALSE)
          snow.am <- quantile(ann$snow,quan$rainsnow,
                              na.rm=TRUE,names=FALSE)
          if (snow.am > 0) { # median may be zero
            quan$snow <- secant(function(qu)
                                return(snow.am - sum(apply(snow.b,2,
                                                           quantile,qu,
                                                           na.rm=TRUE,
                                                           names=FALSE))))
            seas$snow <- apply(var.n[,,"snow"],2,quantile,
                               quan$snow,
                               na.rm=TRUE,names=FALSE)
          } else seas$snow <- seas$rain*0
          # calculate the fraction of rain
          rs.f <- seas$rain/(seas$rain+seas$snow)
          if(any(is.nan(rs.f)))
            rs.f[seas$rain+seas$snow==0] <- 1 # avoid divide by zero
          seas$rain <- seas$var * rs.f
          seas$snow <- seas$var * (1 - rs.f)
          # these two operations make the bars equal for
          # precip.norm=TRUE and FALSE
        }
        if(dat$a.cut && ann$active > 0) {
          quan$active <- secant(function(qu)
                                return(ann$active - sum(apply(dat$active,2,
                                                              quantile,qu,
                                                              na.rm=TRUE,
                                                              names=FALSE))))
          seas$active <- apply(dat$active[,,var]/norm.b,2,
                               quantile,quan$active,
                               na.rm=TRUE,names=FALSE)
        }
        if(all(apply(dat$days,1,function(r)(r==dat$days[1,]))))
          quan$days <- .5 # secant method fails if all same value
        else
          quan$days <- secant(function(qu)
                              return(ann$days - sum(apply(dat$days,2,
                                                          quantile,qu,
                                                          na.rm=TRUE,
                                                          names=FALSE))))
        seas$days <- apply(dat$days,2,quantile,quan$days,
                           na.rm=TRUE,names=FALSE)
        if(ann$na > 0) {
          quan$na <- secant(function(qu)
                            return(ann$na - sum(apply(dat$na,2,
                                                      quantile,qu,
                                                      na.rm=TRUE,
                                                      names=FALSE))))
          seas$na <- apply(dat$na/norm.b,2,quantile,quan$active,
                           na.rm=TRUE,names=FALSE)
        } else seas$na <- quan$days*0
      } else { # calculate stats conventionally (much faster)
        seas$var <- apply(var.n[,,var],2,fun,na.rm=TRUE)
        if(precip.norm) {
          seas$rain <- apply(var.n[,,"rain"],2,fun,na.rm=TRUE)
          seas$snow <- apply(var.n[,,"snow"],2,fun,na.rm=TRUE)
        }
        if(dat$a.cut)
          seas$active <- apply(dat$active[,,var]/norm.b,2,fun,na.rm=TRUE)
        seas$days <- apply(dat$days,2,fun,na.rm=TRUE)
        seas$na <- apply(dat$na/norm.b,2,mean,na.rm=TRUE)
      }
    } # end if (!ann.only)
    fixnames <- function(n){
      n[n %in% "var"] <- var
      return(n)
    }
    names(ann) <- fixnames(names(ann))
    l <- list(ann=ann)
    if(!ann.only) {
      attr(l,"class") <- "seas.norm"
      names(seas) <- fixnames(names(seas))
      l$seas <- seas
      l$width <- dat$width
      l$bins <- dat$bins
    }
    l$var <- var
    l$prime <- dat$prime
    l$unit <- dat$unit
    l$norm <- norm
    l$ann.only <- ann.only
    l$precip.norm <- precip.norm
    l$a.cut <- dat$a.cut
    if(n.years > 1)
      l$fun <- fun
    if(!ann.only && fun == "median") {
      names(quan) <- fixnames(names(quan))
      l$quantile <- quan
    }
    l$range <- dat$range
    l$id <- dat$id
    l$name <- dat$name
    l
  }

"precip.norm" <-
  function(dat, start, end, fun="median",...) {
    if(missing(start)) start <- NULL
    if(missing(end)) end <- start
    if(is.function(fun)) fun <- as.character(substitute(fun))
    seas.norm(dat=dat,start=start,end=end,fun=fun,precip.norm=TRUE,...)
  }
