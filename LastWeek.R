foundMonday<-FALSE; m=4
while (!foundMonday) {
  if (weekdays(Sys.Date()-m)=="Monday") {
    foundMonday<-TRUE
  } else {
    m<-m+1
  }
}
Monday<-Sys.Date()-m
print(Monday)
# A week of sessions
par(mfrow=c(5,3))
for (i in 0:4) {
  Asia.Indices<-GetIndexes(Price=GetSessionPrices(Minutes = 1,Date = Monday+i,Session = "Asia"),pca.plot = FALSE)
  Europe.Indices<-GetIndexes(Price=GetSessionPrices(Minutes = 1,Date = Monday+i,Session = "Europe"),pca.plot = FALSE)
  America.Indices<-GetIndexes(Price=GetSessionPrices(Minutes = 1,Date = Monday+i,Session = "America"),pca.plot = FALSE)
  YLIM<-range(c(range(Asia.Indices$Ind.df[,-1]),range(Europe.Indices$Ind.df[,-1]),range(America.Indices$Ind.df[,-1])))
  if (i==0) {
    par(mar=c(0,4,2,0))
    PlotIndices(Asia.Indices$Ind.df,Legend = FALSE,newYLim=TRUE,main="Asia",ylab=weekdays(Monday+i),ylim=YLIM,xaxt="n")
  } else if (i==4) {
    par(mar=c(4,4,0,0))
    PlotIndices(Asia.Indices$Ind.df,Legend = FALSE,newYLim=TRUE,ylab=weekdays(Monday+i),ylim=YLIM)
  } else {
    par(mar=c(0,4,0,0))
    PlotIndices(Asia.Indices$Ind.df,Legend = FALSE,newYLim=TRUE,ylim=YLIM,ylab=weekdays(Monday+i),xaxt="n")
  }
  if (i==0) {
    par(mar=c(0,0,2,0))
    PlotIndices(Europe.Indices$Ind.df,Legend = FALSE,newYLim=TRUE,main="Europe",ylim=YLIM,xaxt="n",yaxt="n")
    PlotIndices(America.Indices$Ind.df,Legend = TRUE,newYLim=TRUE,main="America",ylim=YLIM,xaxt="n",yaxt="n")
  } else if (i==4) {
    par(mar=c(4,0,0,0))
    PlotIndices(Europe.Indices$Ind.df,Legend = FALSE,newYLim=TRUE,ylim=YLIM,yaxt="n")
    PlotIndices(America.Indices$Ind.df,Legend = FALSE,newYLim=TRUE,ylim=YLIM,yaxt="n")
  } else {
    par(mar=c(0,0,0,0))
    PlotIndices(Europe.Indices$Ind.df,Legend = FALSE,newYLim=TRUE,ylim=YLIM,xaxt="n",yaxt="n")
    PlotIndices(America.Indices$Ind.df,Legend = FALSE,newYLim=TRUE,ylim=YLIM,xaxt="n",yaxt="n")
  }
}
