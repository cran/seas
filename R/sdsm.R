"write.sdsm" <-
  function(dat,var,start,end,file="") {
    start <- as.Date(paste(start,1,1,sep="-"))
    end <- as.Date(paste(end,12,31,sep="-"))
    sdat <- data.frame(date=seq(start,end,by="day"),var=NA)
    dat <- merge(sdat,dat[,c("date",var)],by="date",all.x=TRUE)[,var]
    dat <- sprintf("%9.3f",ifelse(is.na(dat),-999,dat))
    if (is.character(file)) {
      file <- file(file, "w")
      on.exit(close(file))
    }
    if (!inherits(file, "connection"))
      stop("'file' must be a character string or connection\n")
    if (!isOpen(file)) {
      open(file, "w")
      on.exit(close(file))
    }
    write.table(dat,file,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }

"read.sdsm" <-
  function(file,start=1961,end=2000,year.length=366) {
    if(!year.length %in% c(365,366)) { # HadCM3 -- 30 days in each month
    	days <- floor(seq(1,365,length.out=year.length))
    	date <- sprintf("%04i-%03i",rep(start:end,each=year.length),days)
    	date <- as.Date(date,"%Y-%j")
    }else{
    	start <- as.Date(paste(start,1,1,sep="-"))
    	end <- as.Date(paste(end,12,31,sep="-"))
    	date <- seq(start,end,by="day")
    }
    attr(date,"year.length") <- year.length
    dat <- read.table(file,na.strings="-999.000")
    dat$date <- date
    dat
  }
