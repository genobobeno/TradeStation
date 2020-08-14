ScrapeCalendar<-function() {
  Curr.URL<-"https://cdn-nfs.faireconomy.media/ff_calendar_thisweek.csv"
  NEWS<-read.csv(Curr.URL)
  HOURS<-ifelse(grepl("p",NEWS$Time),
                (as.numeric(sapply(NEWS$Time,function(x) strsplit(x,":")[[1]][1]))+12)%%24,
                sapply(NEWS$Time,function(x) strsplit(x,":")[[1]][1]))
  HOURS[nchar(HOURS)==1]<-paste0("0",HOURS[nchar(HOURS)==1])  
  TIMES<-paste0(HOURS,":",substr(strsplit(NEWS$Time,":")[[1]][2],1,2),":00")
  NEWS$TimeStamp<-as.POSIXct(paste0(substr(NEWS$Date,7,10),"-",substr(NEWS$Date,1,5)," ",TIMES),tz="America/New_York")
  NEWS[,-c(3,4,6,7)]
}
