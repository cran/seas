"write.lars" <-
  function(file="",dat,var,start,end) {
    if(file == ""){
      con <- stdout()
    } else {
      con <- file(file,"w")
      on.exit(close(con))
    }
    if(!inherits(start,"Date"))
      start <- as.Date(paste(start,1,1,sep="-"))
    if(!inherits(end,"Date"))
      end <- as.Date(paste(end,12,31,sep="-"))
    sdat <- data.frame(date=seq(start,end,by="day"))
    sdat$year <- as.integer(format(sdat$date,"%Y"))
    sdat$yday <- as.integer(format(sdat$date,"%j"))
    if(missing(var))
      var <- c("t_min","t_max","precip","solar") %in% names(dat)
    dat <- merge(sdat,dat[,c("date",var)],by="date",all.x=TRUE,sort=FALSE)
    dat$date <- NULL
    write.table(dat,file,quote=FALSE,row.names=FALSE,col.names=FALSE,
		na="-99.",dec=".",sep="\t",eol = "\r\n")
  }

"read.lars" <-
  function(file,head=c("year","yday","t_max","t_min","precip","solar"),
           year.offset=0) {
    dat <- read.table(file,col.names=head,na.strings="-99.")
    dat$year <- dat$year+year.offset
    dat$date <- as.Date(paste(dat$year,dat$yday,sep="-"),"%Y-%j")
    return(dat)
  }

"write.lars.scenario" <-
  function(file,dat1,dat2,name="anomaly") {
    t_max <- seas.change(dat1,dat2,"t_max")
    t_min <- seas.change(dat1,dat2,"t_min")
    if(!"t_mean" %in% names(dat1))
      dat1$t_mean <- apply(dat1[,c("t_min","t_max")],1,mean,na.rm=T)
    if(!"t_mean" %in% names(dat2))
      dat2$t_mean <- apply(dat2[,c("t_min","t_max")],1,mean,na.rm=T)
    t_mean <- seas.change(dat1,dat2,"t_mean")
    precip <- seas.change(dat1,dat2,"precip",disc=TRUE)
    solar <- seas.change(dat1,dat2,"solar")
    tb <- data.frame(row.names=month.abb,rain=rep(1,12),wet=1,dry=1,min=0,max=0,tsd=1,rad=1)
    tb$rain <- precip$cent.rel[,"precip"]
    tb$wet <- precip$wet.rel
    tb$dry <- precip$dry.rel
    tb$min <- t_min$cent.abs
    tb$max <- t_max$cent.abs
    tb$tsd <- t_mean$sprd.rel
    tb$rad <- solar$cent.rel
    if(file == ""){
      con <- stdout()
    } else {
      con <- file(file,"w")
      on.exit(close(con))
    }
    writeLines(c("// mon\train\twet\tdry\tmin\tmax\ttsd\trad",
                 "[NAME]",name,"[GCM PREDICTIONS]"),con)
    write.table(round(tb,4),con,quote=FALSE,sep="\t",col.names=FALSE)
    writeLines("[END]",con)
    invisible(tb)
  }

"lars2help" <-
  function(infile,outfile,year.offset,region){
    require(seas)
    
    fp <- file(infile,"r")
    l <- readLines(fp)
    close(fp)
    
    name <- l[2]
    lat <- as.numeric(substr(l[4],1,9))
    infile2 <- l[6]
    message("name: ",name,"\nlat: ",lat)
    
    message("reading ",infile2)
    dat <- read.lars(infile2,year.offset=year.offset)
    dat$t_mean <- rowMeans(dat[,c("t_min","t_max")])
    
    outfiles <- paste(outfile,c(4,7,13),sep=".D")
    names(outfiles) <- c("precip","temp","solar")
    
    message("writing ",outfiles["precip"])
    write.help(outfiles["precip"],dat,"precip",name,region,lat)
    message("writing ",outfiles["temp"])
    write.help(outfiles["temp"],dat,"t_mean",name,region,lat)
    message("writing ",outfiles["solar"])
    write.help(outfiles["solar"],dat,"solar",name,region,lat)
    message("done")
  }
