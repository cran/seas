"write.help" <-
  function(file, dat, var, name="", region="", lat, visual.help=FALSE, metric=TRUE) {
    if(!var %in% c("t_mean","precip","solar"))
      stop(gettextf("%s must be one of %s, %s or %s for daily mean temperature, precipitation and global solar radiation",
                    sQuote("var"),sQuote("t_mean"),sQuote("precip"),sQuote("solar")))
    orig <- as.character(substitute(dat))[[1]]
    sc <- seas.df.check(dat,orig,var)
    header <- formatC(c(3,ifelse(metric,2,1),NA,NA),0,2)
    header[3] <- sprintf("%-20s %-20s",name,region)
    if(var == "solar"){
      header[4] <- sprintf("%9.2f",lat)
      type <- 5
    }
    if(var %in% c("precip","t_mean")) {
      if(var == "precip") {
        seas.nm <- seas.norm(seas.sum(dat,var="precip",width="mon"),fun=mean)$seas
        nm <- seas.nm$precip * seas.nm$days # in mm/month
        type <- 1
      } else { # t_mean
        nm <- tapply(dat$t_mean,mkseas(dat,width="mon"),mean,na.rm=TRUE)
        type <- 3
      }
      if(visual.help) dw <- 9
      else dw <- 6
      header[4] <- paste(formatC(nm,1,dw,"f"),collapse="")
    }
    header <- paste(header,collapse="\n")
    if(visual.help)
      type <- type+1
    yearrange <- range(dat$year)
    years <- seq(yearrange[1],yearrange[2],by=1)
    days <- rep(365,length(years))
    days[years%%4==0&years%%100!=0|years%%400==0] <- 366
    val <- dat[[var]]
    if(sum(days) != length(val))
      stop("Number of values should correspond to the number of possible days between whole years")
    if(any(!is.finite(val)))
      stop("Missing values are not allowed")
    invisible(.C("writeHELP",file,header,as.integer(type),
                 as.integer(yearrange[1]),as.integer(diff(yearrange)+1),
                 as.double(val),PACKAGE="seas"))
  }
