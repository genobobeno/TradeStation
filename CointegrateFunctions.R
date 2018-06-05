rm(list=ls())
#############################################
###   FUNCTIONS
#############################################
setwd("~/QuantFinance/Cointegration")

# Time Series Plotting
library(ggplot2)
library(xts)
library(dygraphs)

# Create URL to fetch date specific range for symbol. Default two years from 9/16/2014-9/16/2016
StockURL = function(symbol,startdate="09/16/2014",enddate="09/16/2016") {
  stopifnot(is.character(symbol),nchar(symbol)>0,!grepl("[0-9]",symbol))
  symbol=toupper(symbol)
  start=strsplit(startdate,"/")[[1]]
  end=strsplit(enddate,"/")[[1]]
  return(paste0("http://real-chart.finance.yahoo.com/table.csv?s=",symbol,
                "&a=",ifelse(nchar(start[1])==1,paste0("0",start[1]),start[1]),
                "&b=",ifelse(nchar(start[2])==1,paste0("0",start[2]),start[2]),
                "&c=",ifelse(nchar(start[3])==2,ifelse(as.numeric(start[3])>16,paste0("19",start[3]),paste0("20",start[3])),start[3]),
                "&d=",ifelse(nchar(end[1])==1,paste0("0",end[1]),end[1]),
                "&e=",ifelse(nchar(end[2])==1,paste0("0",end[2]),end[2]),
                "&f=",ifelse(nchar(end[3])==2,ifelse(as.numeric(end[3])>16,paste0("19",end[3]),paste0("20",end[3])),end[3]),
                "&g=d&ignore=.csv"))
}

# Get Yahoo csv and make it a data.frame
yahoo.read <- function(url){return(read.csv(url,header=TRUE))}

# Call previous functions, parse data.frame, fill parent environment with col.[date,sec.A,sec.B]
GetPair = function(s1,s2=NA,days=252) {
  if (length(s1)==2) { s2 = s1[2]; s1 = s1[1] }
  if (length(s1)<3) {
    df1 = yahoo.read(StockURL(s1));    df2 = yahoo.read(StockURL(s2))    
    col.date <<- as.Date(df1[1:days,"Date"], "%Y-%m-%d")
    col.sec.A.adj.price <<- df1[1:days,"Adj.Close"]
    col.sec.B.adj.price <<- df2[1:days,"Adj.Close"]
    TS = eval(parse(text = paste0("data.frame(",s1,"=col.sec.A.adj.price,",s2,"=col.sec.B.adj.price)")))
  } else {
    for (i in 1:length(s1)) {
      df = yahoo.read(StockURL(s1[i]))
      if (i>1) {
        TS[,s1[i]] = df[1:days,'Adj.Close']
        if (i==2) col.sec.B.adj.price <<- df[1:days,"Adj.Close"]
      } else {
        eval(parse(text = paste0("TS = data.frame(",s1[i],"=df[1:days,'Adj.Close'])")))
        col.sec.A.adj.price <<- df[1:days,"Adj.Close"]
      } 
    }
  }
  col.date <<- TS$DATE <- as.Date(df1[1:days,"Date"], "%Y-%m-%d")
  TS = TS[nrow(TS):1,]
  return (TS)
}

# O-U function
fn.ou <- function (theta, x) {
    mu <- theta[1];
    kappa <- theta[2];
    sigma <- theta[3];
    
    dt <- 1/252;
    n <- length(x);
    logl <- 0;
    v <- sigma*sigma*(1-exp(-2*kappa*dt))/(2*kappa);
    for (i in 2:n) {
        logl <- logl + (x[i] - x[i-1]*exp(-kappa*dt) - mu*(1-exp(-kappa*dt)))^2;
    }
    logl <- -0.5*log(2*pi) - log(v) - (1/(2*n*v))*logl;

    return (-logl);
}    


