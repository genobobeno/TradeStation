GetWeeklyNewsData<-function() {
  FXNews<-ScrapeCalendar()
  Sunday<-Sys.Date()-which(weekdays(Sys.Date()-0:6)=="Sunday")+1
  Friday<-Sunday+5
  print(paste("We are capturing the week of",Sunday,"thru",Friday))
  write.csv(FXNews,paste0("News/ForexFactory_",Sunday,".csv"),row.names = FALSE)
}

GetWeeklyPriceData<-function(dateInWeek=NA) {
  StartDate<-ifelse(is.na(dateInWeek),Sys.Date(),dateInWeek)
  Sunday<-StartDate - which(weekdays(dateInWeek-0:6)=="Sunday")-1
  Friday<-Sunday+5*24*60
  if (m>0) {
    if (as.numeric(substr(Sys.time(),start = 12,stop = 13))>17) {
      print(paste("We are capturing the week of",Sunday,"thru",Friday))
      write.csv(FXNews,paste0("PriceData/Prices_",Sunday,".rds"),row.names = FALSE)
    } else {
      print("This ")
    }
  }
}



