"write.help" <-
  function(file = "", dat, var, name, region, lat, visual.help=FALSE, metric=TRUE) {
    if(!var %in% c("t_mean","precip","solar"))
      stop(gettextf("%s must be one of %s, %s or %s for daily mean temperature, precipitation and global solar radiation",
                    sQuote("var"),sQuote("t_mean"),sQuote("precip"),sQuote("solar")))
    if(file == ""){
      con <- stdout()
    } else {
      con <- file(file,"w")
      on.exit(close(con))
    }
    writeLines(formatC(c(3,ifelse(metric,2,1)),0,2),con) # write data entered (9) and metric flags (2)
    writeLines(sprintf("%-20s %-20s",name,region),con)
    if(var == "solar")
      writeLines(sprintf("%9.2f",lat),con)
    if(var %in% c("precip","t_mean")) {
      if(var == "precip") {
        seas.nm <- seas.norm(seas.sum(dat,var="precip",width="mon"))$seas
        nm <- seas.nm$precip * seas.nm$days # in mm/month
      } else
      nm <- tapply(dat$t_mean,mkseas(dat,width="mon"),mean,na.rm=TRUE)
      if(visual.help) dw <- 9
      else dw <- 6
      cat(formatC(nm,1,dw,"f"),sep="",file=con)
      writeLines("",con)
    }
    dat$fact <- mkseas(dat,width=10)
    years <- range(dat$year)
    start <- years[1]
    end <- years[2]
    yf <- "%5i"
    dw <- 9
    ds <- 1
    if(var == "precip") { # make it identical to Visual HELP format
      yf <- "%10i"
      dw <- 8
    }
    if(!visual.help) {
      dw <- 6
      if (var == "precip") {
        ds <- 2
        dw <- 5
      } else if(var == "solar") {
        ds <- 2
      }
    }
    for(year in start:end) {
      for(sl in 1:37) {
        cat(sprintf(yf,year),file=con)
        ln <- dat[dat$year==year&dat$fact==sl,var]
        if((sl != 37 && length(ln) != 10) || (sl == 37 & length(ln) < 5))
          warning(paste("Incomplete data: year=",year,"i=",i,"slice=",sl))
        if(sl == 37) ln <- c(ln,rep(0,10-length(ln)))
        cat(formatC(ln,ds,dw,"f"),sep="",file=con)
        writeLines(sprintf(yf,sl),con)
      }
    }
  }
