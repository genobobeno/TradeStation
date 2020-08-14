PlotPair<-function(pair,minutes=2,HoursBack=18,SR=TRUE,hours=NA,vols=6,plot.ufo=TRUE) {
  #pair="GBP_CAD";minutes=1;HoursBack=12;SR=TRUE;hours=NA;vols=6
  t.Start<-Sys.time()-HoursBack*3600
  t.Stop<-Sys.time() #-24*7.5*3600
  CheckTimeTicks(t.start = t.Start,t.stop = t.Stop)
  if (!is.na(minutes)) {
    df.Price<-GetPairPrices(Pair=pair,t.start = t.Start,t.stop = t.Stop, Minutes = minutes)
  } else if (!is.na(hours)) {
    df.Price<-GetPairPrices(Pair=pair,t.start = t.Start,t.stop = t.Stop, Hours = hours)
  }
  ############################################
  #pair<-names(df.Price)[1]
  JW<-CleanOpen(df.Price)
  JW<-LabelUpDown(price=JW,multiple.pairs = FALSE)
  JW<-LegBaseCount(price=JW)
  JW<-BuildHistory(price=JW)
  JW<-AppendFeatures(price=JW)
  JW<-AppendIndicators(price=JW)
  i=nrow(JW)
  DrawChart(JW[1:i,],SR=SR,Pair=pair,Vols=vols,PlotUFO=plot.ufo)
}

DrawCandles<-function(PriceData,...) {
  #PriceData<-JW[1:207,]
  YLIM<-c(0.9998,1.0002)*c(min(PriceData$Low),max(PriceData$High))
  XLIM<-range(c(PriceData$TimeStamp,(PriceData$TimeStamp[nrow(PriceData)])+c(-1,2)*diff(PriceData$TimeStamp)[1]))
  CSpace<-0.4*diff(PriceData$TimeStamp)[1]
  #par(mar=c(3,2,1,1))
  plot(XLIM,YLIM,type="n",ylim=YLIM,xlim=XLIM,...)
  arrows(PriceData$TimeStamp,PriceData$Low,PriceData$TimeStamp,PriceData$High,length = 0)
  rect(PriceData$TimeStamp-CSpace,ifelse(PriceData$Open>PriceData$Close,PriceData$Close,PriceData$Open),
       PriceData$TimeStamp+CSpace,ifelse(PriceData$Open<PriceData$Close,PriceData$Close,PriceData$Open),
       col=ifelse(PriceData$Open<PriceData$Close,"green","red"))
  if ("TrendState" %in% names(PriceData)) {
    text(PriceData$TimeStamp[nrow(PriceData)],YLIM[1]+0.95*abs(diff(YLIM)),PriceData$TrendState[nrow(PriceData)])
  }
}

DrawChart<-function(PriceData,SR=TRUE,Pair=NA,Vols=6,PlotUFO=FALSE,PlotADX=TRUE,...) {
  candles<-floor(nrow(PriceData)/Vols)
  mult<-ifelse(floor(PriceData$Low[1]/10)>1,0.001,0.00001)
  VOLS<-list()
  for (v in 1:Vols) { #v=1
    ROWS<-(v-1)*candles+1:candles
    TICKS<-as.data.frame(do.call(rbind,apply(cbind(PriceData$Low[ROWS],PriceData$High[ROWS],PriceData$TickVolume[ROWS]),1,function(x) cbind(seq(from=x[1],to=x[2],by=mult),x[3]))))
    TICKS<-aggregate(V2~V1,TICKS,sum)
    XY<-density(TICKS$V1, weights=TICKS$V2/sum(TICKS$V2))
    VOLS[[paste0("vol",v)]]<-list(TS=PriceData$TimeStamp[max(ROWS)],DENSITY=XY,TICKS=max(TICKS$V2))
  }
  nf<-layout(matrix(c(1,2),nrow=2,ncol=1),1,c(1,3))
  #layout.show(nf)
  par(mar=c(0,3,2,1))
  plot(PriceData$TimeStamp,PriceData$rsi14,type="n",ylim=c(0,100),xaxt="n",main=Pair)
  lines(PriceData$TimeStamp,PriceData$rsi14,col=2,lwd=1)
  lines(PriceData$TimeStamp,PriceData$adx14,col=6,lwd=1.5)
  lines(PriceData$TimeStamp,PriceData$adx5,col=4,lwd=1.5)
  abline(h=c(40,60),col=4,lty=2)
  lines(PriceData$TimeStamp,50+30*PriceData$cci13/max(abs(PriceData$cci13),na.rm = T),col=3)
  legend("topleft",legend = c("RSI(14)","CCI(13)","ADX(5)","ADX(14)"),col=c(2,3,4,6),lty=1)
  par(mar=c(3,3,0,1))
  YLIM<-c(0.9998,1.0002)*c(min(PriceData$Low),max(PriceData$High))
  XLIM<-range(c(PriceData$TimeStamp,(PriceData$TimeStamp[nrow(PriceData)])+c(-1,8)*diff(PriceData$TimeStamp)[1]))
  CSpace<-0.4*min(diff(PriceData$TimeStamp))
  alph<-0.25*((0.9*PriceData$TickVolume)/max(PriceData$TickVolume))^3
  plot(XLIM,YLIM,type="n",ylim=YLIM,xlim=XLIM,...)
  if (mult==0.001) {
    abline(h=c(floor(range(PriceData$Open)[1]):ceiling(range(PriceData$Open)[2])),col=4)
    abline(h=c(floor(range(PriceData$Open*10)[1]):ceiling(range(PriceData$Open*10)[2]))/10,col=5)
  } else {
    abline(h=c(floor(range(PriceData$Open*10)[1]):ceiling(range(PriceData$Open*10)[2]))/10,col=4)
    abline(h=c(floor(range(PriceData$Open*100)[1]):ceiling(range(PriceData$Open*100)[2]))/100,col=5)
  }
  
  for (j in 1:nrow(PriceData)) {
    rect(xleft = PriceData$TimeStamp[j],ybottom = PriceData$Low[j],xright =PriceData$TimeStamp[nrow(PriceData)],ytop = PriceData$High[j],border = NA,
         col = rgb(red = 0,green = 0,blue = 1,alpha = alph[j]))
  }
  arrows(PriceData$TimeStamp,PriceData$Low,PriceData$TimeStamp,PriceData$High,length = 0)
  rect(PriceData$TimeStamp-CSpace,ifelse(PriceData$Open>PriceData$Close,PriceData$Close,PriceData$Open),
       PriceData$TimeStamp+CSpace,ifelse(PriceData$Open<PriceData$Close,PriceData$Close,PriceData$Open),
       col=ifelse(PriceData$Open<PriceData$Close,"green","red"))
  if ("TrendState" %in% names(PriceData)) {
    text(PriceData$TimeStamp[ceiling(0.9*nrow(PriceData))],YLIM[1]+0.95*abs(diff(YLIM)),PriceData$TrendState[nrow(PriceData)])
  }
  lines(PriceData$TimeStamp,PriceData$ema8,col="red",lwd=3)
  lines(PriceData$TimeStamp,PriceData$ema21,col="green",lwd=3)
  lines(PriceData$TimeStamp,PriceData$ema50,col="blue",lwd=3)
  VOL.Height<-difftime(VOLS[[2]]$TS,VOLS[[1]]$TS,units = "secs")
  MAXV<-max(sapply(VOLS,function(x) x$TICKS))
  for (v in 1:length(VOLS)) {
    Height.mult<-VOL.Height/1.5*(VOLS[[v]]$TICKS/MAXV)
    X<-VOLS[[v]]$TS+ceiling(Height.mult*VOLS[[v]]$DENSITY$y/max(VOLS[[v]]$DENSITY$y))#*as.numeric(VOL.Height))
    Y<-VOLS[[v]]$DENSITY$x
    polygon(x = c(X,X[1]),y = c(Y,Y[1]),col = rgb(red = 0,green = 0,blue = 1,alpha = 0.5),border = NA)
  }
  if (SR) {
    ### Run Loop from bottom of price range to top
    l.sr<-c()
    for (i in 1:(diff(range(c(PriceData$Low,PriceData$High))/mult))) {
      ### Choose price to test
      if (mult==0.001) {
        l.test<-round(min(PriceData$Low)+i*mult,digits=3) #l.test=0.6096
        ### Get Lows/Highs that are within acceptable distance (This took some playing)
        l.ind.l<-which(abs((PriceData$Low-l.test)^3)<mult^2.6)  
        l.ind.h<-which(abs((PriceData$High-l.test)^3)<mult^2.6)
      } else {
        l.test<-round(min(PriceData$Low)+i*mult,digits=5)
        l.ind.l<-which(abs((PriceData$Low-l.test)^3)<mult^2.8)  
        l.ind.h<-which(abs((PriceData$High-l.test)^3)<mult^2.8)
      }
      ### If there's a few good lows away from the edges
      if (length(l.ind.l)>0 && (sum(l.ind.l>5)>0 & sum(l.ind.l[length(l.ind.l)]<(nrow(PriceData)-5))>0)) {
        ### Test that it's a local Low (Lowest of +/- 3 local wicks)
        l.ind.l<-l.ind.l[l.ind.l>5]
        l.ind.l<-l.ind.l[l.ind.l<(nrow(PriceData)-5)]
        TF<-sapply(l.ind.l,function(x) PriceData$Low[x]<min(c(PriceData$Low[x+c(-5,-4,-3,-2,-1,1:5)])))
        l.ind.l<-l.ind.l[TF]
      } else l.ind.l<-c()
      ### If there's a few good highs away from the edges
      if (length(l.ind.h)>0 && (l.ind.h[1]>5 & l.ind.h[length(l.ind.h)]<(nrow(PriceData)-5))) {
        ### Test that it's a local High (Highest of +/- 3 local wicks)
        l.ind.h<-l.ind.h[l.ind.h>5]
        l.ind.h<-l.ind.h[l.ind.h<(nrow(PriceData)-5)]
        TF<-sapply(l.ind.h,function(x) PriceData$High[x]>max(c(PriceData$High[x+c(-5,-4,-3,-2,-1,1:5)])))
        l.ind.h<-l.ind.h[TF]
      } else l.ind.h<-c()
      ### Combine all indexes of good lows and highs
      l.ind<-c(l.ind.l,l.ind.h) 
      ### If there's at least 2 and less than 10, 
      #   separated by at last 10 candles over a range of at least 40 candles
      if (length(l.ind)>=2 && (length(l.ind)<=9 & diff(range(l.ind))>40 & all(abs(diff(l.ind))>10))) {
        # Draw the line
        l.sr<-c(l.sr,l.test)
        cat(l.test,":")
      } else next
    }
    l.sr<-l.sr[c(TRUE,diff(l.sr)>mult)]
    print(l.sr)
    abline(h=l.sr,col="gray50")
  }
  if (PlotUFO) {
    UFO<-UFOs(PairPrice=PriceData,PlotPair=Pair,minutes=30,hours=NA) 
    rect(xleft = PriceData$TimeStamp[nrow(PriceData)]+difftime(PriceData$TimeStamp[4],PriceData$TimeStamp[1]),
         ybottom = UFO$low[UFO$counts==1],
         xright = PriceData$TimeStamp[nrow(PriceData)]+difftime(PriceData$TimeStamp[12],PriceData$TimeStamp[1]),
         ytop = UFO$high[UFO$counts==1],
         col = ifelse(UFO$low[UFO$counts==1]>PriceData$Open[nrow(PriceData)],4,2),border = NA)
    UFO<-UFOs(PairPrice=PriceData,PlotPair=Pair,minutes=NA,hours=4) 
    rect(xleft = PriceData$TimeStamp[nrow(PriceData)]+difftime(PriceData$TimeStamp[12],PriceData$TimeStamp[1]),
         ybottom = UFO$low[UFO$counts==1],
         xright = PriceData$TimeStamp[nrow(PriceData)]+difftime(PriceData$TimeStamp[20],PriceData$TimeStamp[1]),
         ytop = UFO$high[UFO$counts==1],
         col = ifelse(UFO$low[UFO$counts==1]>PriceData$Open[nrow(PriceData)],4,2),border = NA)
  }
}

# ?strptime
# library(stringr)
# TimeInSeconds<-2000
# OA_F1Px<-str_replace(paste0(as.character(Sys.time()-TimeInSeconds),"-05:00")," ","T")
# OA_F2Px<-str_replace(paste0(as.character(Sys.time()),"-05:00")," ","T")
# str(format(as.POSIXct(OA_F1Px),format = "%Y-%m-%d %H:%M:%S"))
# TimeStamp
# TimeStamp <- as.POSIXct(strptime(OA_F1Px, "%Y-%m-%d %H:%M:%OS"),
#                                origin="1970-01-01",tz = "UTC")
# attributes(TimeStamp)$tzone <- TimeAlign

PlotIndex<-function(ind,title="index") {
  plot(ind,type="n",main=title,xlab="time",ylab="strength")
  lines(ind,lty=1,lwd=2)
} 

PlotIndices<-function(inds,hinds=NA,linds=NA,maxpair=FALSE,Legend=TRUE,newYLim=FALSE,...) {
  library(combinat)
  width<-combn(2:ncol(inds),2)
  int.diff<-c()
  for (i in 1:ncol(width)) {
    int.diff<-c(int.diff,sum(inds[,width[1,i]]-inds[,width[2,i]]))
  }
  widePair<-width[,which.max(abs(int.diff))]
  if (int.diff[which.max(abs(int.diff))]<0) {
    pdiff<-inds[,widePair[2]]-inds[,widePair[1]]
    pdiff.title<-paste(names(inds)[widePair[c(2,1)]],collapse = "-")
  } else {
    pdiff<-inds[,widePair[1]]-inds[,widePair[2]]
    pdiff.title<-paste(names(inds)[widePair],collapse = "-")
  }
  if (maxpair) par(mfrow=c(2,1))
  if (!newYLim) {
    YLIM<-range(inds[,2:ncol(inds)])
    plot(inds[,1],inds[,2],type="n",xlab="time",ylim=YLIM,...)
  } else {
    plot(inds[,1],inds[,2],type="n",xlab="time",...)
  }
  lines(inds[,1],inds[,2],lty=1,lwd=2)
  #text(inds[nrow(inds),1],inds[nrow(inds),2]+0.03*diff(YLIM),names(inds)[2])
  if (!is.na(hinds)[1] & !is.na(linds)[1]) polygon(c(inds[1,1],inds[,1],inds[nrow(inds):1,1]),
                                                   c(linds[1,2],hinds[,2],linds[nrow(inds):1,2]),
                                                   density = 40)
  cols<-c("darkgreen","blue","red","orange","orangered","lightblue","purple")
  for (j in 3:ncol(inds)) {
    lines(inds[,1],inds[,j],lty=1,lwd=2,col=cols[j-2])
    #text(inds[nrow(inds),1],inds[nrow(inds),j]+0.03*diff(YLIM),names(inds)[j])
    if (!is.na(hinds)[1] & !is.na(linds)[1]) polygon(c(inds[1,1],inds[,1],inds[nrow(inds):1,1]),
                                                     c(linds[1,j],hinds[,j],linds[nrow(inds):1,j]),
                                                     density = 40)
  } 
  if (maxpair) plot(pdiff,main=pdiff.title,lty=1)
  if (Legend) legend("bottomleft",legend = names(inds)[2:ncol(inds)],col = c(1,cols),lty=1,lwd=2)
}
