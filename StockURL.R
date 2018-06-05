StockURL = function(symbol,
                    startdate=paste0(strsplit(as.character(Sys.Date()-730),"-")[[1]][c(2,3,1)],collapse = "/"),
                    enddate=paste0(strsplit(as.character(Sys.Date()),"-")[[1]][c(2,3,1)],collapse = "/")) {
  # Create URL to fetch date specific range for symbol. Default two years from 9/16/2014-9/16/2016
  stopifnot(is.character(symbol),nchar(symbol)>0,!grepl("[0-9]",symbol))
  symbol=toupper(symbol)
  start=strsplit(startdate,"/")[[1]]
  end=strsplit(enddate,"/")[[1]]
  return(paste0("https://real-chart.finance.yahoo.com/table.csv?s=",symbol,
                "&a=",ifelse(nchar(start[1])==1,paste0("0",start[1]),start[1]),
                "&b=",ifelse(nchar(start[2])==1,paste0("0",start[2]),start[2]),
                "&c=",ifelse(nchar(start[3])==2,ifelse(as.numeric(start[3])>16,paste0("19",start[3]),paste0("20",start[3])),start[3]),
                "&d=",ifelse(nchar(end[1])==1,paste0("0",end[1]),end[1]),
                "&e=",ifelse(nchar(end[2])==1,paste0("0",end[2]),end[2]),
                "&f=",ifelse(nchar(end[3])==2,ifelse(as.numeric(end[3])>16,paste0("19",end[3]),paste0("20",end[3])),end[3]),
                "&g=d&ignore=.csv"))
}
