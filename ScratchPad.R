source("Initialize.R")
PlotPair("AUD_USD",hours = NA,minutes =15,HoursBack = 40 ,vols=4)

IND<-GetIndexes(Price=GetAllPrices(Minutes = 2,LookBackHours = 8),pca.plot = TRUE)

for (i in 1:100) {
  print(Sys.time())
  IND<-GetIndexes(Price=GetAllPrices(Minutes = 2,LookBackHours = 11),pca.plot = TRUE)
  CurrStrength(IND)
  print("Done")
  Sys.sleep(150)
}

ind.cont<-PCA_Continue(IND,LookBackHours = 1)

IND$Ind.df<-rbind(IND$Ind.df,ind.cont)

IND$Ind.df<-IND$Ind.df[!duplicated(IND$Ind.df$TimeStamp),]

INDS<-data.frame(TimeStamp=IND$Ind.df$TimeStamp[-1],
           USD=diff(IND$Ind.df$USD),
           EUR=diff(IND$Ind.df$EUR),
           GBP=diff(IND$Ind.df$GBP),
           JPY=diff(IND$Ind.df$JPY),
           CHF=diff(IND$Ind.df$CHF),
           CAD=diff(IND$Ind.df$CAD),
           AUD=diff(IND$Ind.df$AUD),
           NZD=diff(IND$Ind.df$NZD))

PlotIndices(INDS)

pacf(diff(IND$Ind.df$USD))

# Overnights
IND<-GetIndexes(Price=GetAllPrices(Minutes = 1,LookBackHours = 7.5),pca.plot = TRUE)
CurrStrength(IND)

PlotPair("NZD_JPY",hours = NA,minutes =15,HoursBack = 100 ,vols=4)

Impulsive()


