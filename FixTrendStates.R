
#########   Pulling Price data
t.Start<-Sys.time()-86400*5-18*3600-28*86400
t.Stop<-Sys.time()-86400*1-5*3600-28*86400
CheckTimeTicks(t.start = t.Start,t.stop = t.Stop)
Price<-GetCorrelations(t.start = t.Start,t.stop = t.Stop, Minutes = 15)
saveRDS(Price,"Week1.rds")


df.Price<-readRDS("Week2.rds")
df.Price$EUR_USD$Open

############################################

#################################################
# -- Rquired Packages in order to use the R API
#library("downloader")


# r.c<-cor(cbind(ALL$scores[300:nrow(ALL$scores),1],USD[300:nrow(ALL$scores),1],EUR[300:nrow(ALL$scores),1],GBP[300:nrow(ALL$scores),1],JPY[300:nrow(ALL$scores),1],CHF[300:nrow(ALL$scores),1],CAD[300:nrow(ALL$scores),1],AUD[300:nrow(ALL$scores),1],NZD[300:nrow(ALL$scores),1]))
# row.names(r.c)<-c("ALL","USD","EUR","GBP","JPY","CHF","CAD","AUD","NZD")
# colnames(r.c)<-c("ALL","USD","EUR","GBP","JPY","CHF","CAD","AUD","NZD")
# r.c


par(mfrow=c(3,1))
plot(ALL$scores[,1])
idxs<-cbind(USD[,1],EUR[,1],GBP[,1],JPY[,1],CHF[,1],CAD[,1],AUD[,1],NZD[,1])
mx<-apply(idxs,2,function(x) x/ALL$scores[,1])
plot(c(1,nrow(mx)),range(mx),main="PCS",type="n",xlab="time",ylab="price")
for (j in 1:ncol(mx)) {lines(mx[,j],col=j)}
legend("topright",c("USD","EUR","GBP","JPY","CHF","CAD","AUD","NZD"),col=1:8,lty=1)
mx<-apply(idxs,2,function(x) x*ALL$scores[,1])
plot(c(1,nrow(mx)),range(mx),main="PCS",type="n",xlab="time",ylab="price")
for (j in 1:ncol(mx)) {lines(mx[,j],col=j)}
legend("topright",c("USD","EUR","GBP","JPY","CHF","CAD","AUD","NZD"),col=1:8,lty=1)




Price$EUR_USD
Price$EUR_USD$Close[2:nrow(Price$EUR_USD)]>=Price$EUR_USD$Low[2:nrow(Price$EUR_USD)-1] & 
  Price$EUR_USD$Close[2:nrow(Price$EUR_USD)]<=Price$EUR_USD$High[2:nrow(Price$EUR_USD)-1]
Price$EUR_USD$TimeStamp - Price$AUD_USD$TimeStamp

p <- Price$EUR_USD  %>%
  plot_ly(x = ~TimeStamp, type="candlestick",
          open = ~Open, close = ~Close,
          high = ~High, low = ~Low) %>%
  layout(title = "Basic Candlestick Chart")

print(p)


