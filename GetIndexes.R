# Run Indexes
source("Initialize.R")

t.Start<-Sys.time()-3.5*3600
t.Stop<-Sys.time()  #-12*3600
CheckTimeTicks(t.start = t.Start,t.stop = t.Stop)
Price<-GetAllPrices(t.start = t.Start,t.stop = t.Stop, Seconds=30)
Price<-CleanOpen(Price)
Price<-CleanPriceList(Price)

df.Price<-Price

USD<-IndexCurrency("USD",price.list=df.Price,ticker="Close")
EUR<-IndexCurrency("EUR",price.list=df.Price,ticker="Close")
GBP<-IndexCurrency("GBP",price.list=df.Price,ticker="Close")
JPY<-IndexCurrency("JPY",price.list=df.Price,ticker="Close")
CHF<-IndexCurrency("CHF",price.list=df.Price,ticker="Close")
CAD<-IndexCurrency("CAD",price.list=df.Price,ticker="Close")
AUD<-IndexCurrency("AUD",price.list=df.Price,ticker="Close")
NZD<-IndexCurrency("NZD",price.list=df.Price,ticker="Close")

USDh<-IndexCurrency("USD",price.list=df.Price,ticker="High")
EURh<-IndexCurrency("EUR",price.list=df.Price,ticker="High")
GBPh<-IndexCurrency("GBP",price.list=df.Price,ticker="High")
JPYh<-IndexCurrency("JPY",price.list=df.Price,ticker="High")
CHFh<-IndexCurrency("CHF",price.list=df.Price,ticker="High")
CADh<-IndexCurrency("CAD",price.list=df.Price,ticker="High")
AUDh<-IndexCurrency("AUD",price.list=df.Price,ticker="High")
NZDh<-IndexCurrency("NZD",price.list=df.Price,ticker="High")

USDl<-IndexCurrency("USD",price.list=df.Price,ticker="Low")
EURl<-IndexCurrency("EUR",price.list=df.Price,ticker="Low")
GBPl<-IndexCurrency("GBP",price.list=df.Price,ticker="Low")
JPYl<-IndexCurrency("JPY",price.list=df.Price,ticker="Low")
CHFl<-IndexCurrency("CHF",price.list=df.Price,ticker="Low")
CADl<-IndexCurrency("CAD",price.list=df.Price,ticker="Low")
AUDl<-IndexCurrency("AUD",price.list=df.Price,ticker="Low")
NZDl<-IndexCurrency("NZD",price.list=df.Price,ticker="Low")

INDS<-data.frame(TimeStamp=Price$EUR_CHF$TimeStamp,USD=100*USD[,1],
                 EUR=100*EUR[,1],GBP=100*GBP[,1],JPY=100000*JPY[,1],
                 CHF=100*CHF[,1],CAD=100*CAD[,1],AUD=100*AUD[,1],NZD=100*NZD[,1])
INDS.c<-data.frame(lapply(INDS[,-1],function(x) (x-x[1])))
INDS[,2:length(INDS)]<-INDS.c

par(mfrow=c(1,1),mar=c(3,3,2,1))
PlotIndices(INDS)

ALL<-princomp(cbind(USD[,1],EUR[,1],GBP[,1],JPY[,1],CHF[,1],CAD[,1],AUD[,1],NZD[,1]))

r.c<-cor(cbind(ALL$scores[,1],USD[,1],EUR[,1],GBP[,1],JPY[,1],CHF[,1],CAD[,1],AUD[,1],NZD[,1]))
row.names(r.c)<-c("ALL","USD","EUR","GBP","JPY","CHF","CAD","AUD","NZD")
colnames(r.c)<-c("ALL","USD","EUR","GBP","JPY","CHF","CAD","AUD","NZD")
v.pc<-r.c[,1]
sort(v.pc)
r.c

# VLine<-0
# par(mfrow=c(4,2),mar=c(3,3,2,1))
# PlotIndex(100*USD[,1],title = "USD"); lines(100*USDh[,1],col=3);lines(100*USDl[,1],col=2);abline(v=VLine)
# PlotIndex(100*EUR[,1],title="EUR"); lines(100*EURh[,1],col=3);lines(100*EURl[,1],col=2);abline(v=VLine)
# PlotIndex(100*GBP[,1],title="GBP"); lines(100*GBPh[,1],col=3);lines(100*GBPl[,1],col=2);abline(v=VLine)
# PlotIndex(100000*JPY[,1],title="JPY"); lines(100000*JPYh[,1],col=3);lines(100000*JPYl[,1],col=2);abline(v=VLine)
# PlotIndex(100*CHF[,1],title="CHF"); lines(100*CHFh[,1],col=3);lines(100*CHFl[,1],col=2);abline(v=VLine)
# PlotIndex(100*CAD[,1],title="CAD"); lines(100*CADh[,1],col=3);lines(100*CADl[,1],col=2);abline(v=VLine)
# PlotIndex(100*AUD[,1],title="AUD"); lines(100*AUDh[,1],col=3);lines(100*AUDl[,1],col=2);abline(v=VLine)
# PlotIndex(100*NZD[,1],title="NZD"); lines(100*NZDh[,1],col=3);lines(100*NZDl[,1],col=2);abline(v=VLine)


