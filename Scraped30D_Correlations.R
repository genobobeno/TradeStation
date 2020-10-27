library(rvest)

ws<-read_html(x = "https://www.mrci.com/special/corr030.php")
tb<-html_table(ws,fill=TRUE)
df<-tb[[4]]
row.names(df)<-names(df)<-df[1,]
df<-df[,-1]
df<-df[-1,]
for (i in 1:ncol(df)) df[i,i]<-100
df[]<-lapply(df,as.numeric)
cols<-c(rainbow(50,start = 0.02,end = 0.35),"#FFFFFFFF")
image(as.matrix(df[9:15,9:15]),col = cols,xaxt='n',yaxt='n')
axis(1,at = 0:6*1/6,c("CHF","EUR","JPY","CAD","GBP","AUD","USD"))
axis(2,at = 0:6*1/6,c("CHF","EUR","JPY","CAD","GBP","AUD","USD"),las=2)
for (ix in 1:7) {text(x=(ix-1)*1/6,y=0:6*1/6,df[8+ix,9:15]/100)}
