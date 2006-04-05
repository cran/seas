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
    if(missing(start)) {
      sc <- data.frame(sc=I(c("Cal","Cur","2020","2050","2080")),
                       start=c(1961,1961,2010,2040,2070),
                       end=  c(2000,2000,2039,2069,2099),
                       year.length=c(366,365,365,365,365))
      for(i in 1:nrow(sc)) {
        if(length(grep(sc[i,"sc"],file)) == 1) {
          scl <- as.list(sc[i,])
          start <- scl$start
          end <- scl$end
          year.length <- scl$year.length
          message(sprintf("%s : %i - %i",scl$sc,start,end))
          break
        }
      }
    }
    start <- as.Date(paste(start,1,1,sep="-"))
    end <- as.Date(paste(end,12,31,sep="-"))
    dat <- read.table(file,na.strings="-999.000")
    dates <- seq(start,end,by="day")
    dat$date <- dates[format(dates,"%j") <= year.length]
    dat
  }
