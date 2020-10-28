PlotPair<-function(pair,minutes=2,HoursBack=18,SR=TRUE,hours=NA,vols=6,plot.ufo=TRUE) {
  #pair="GBP_USD";minutes=NA;HoursBack=2000;SR=TRUE;hours=4;vols=4
  t.Start<-Sys.time()-HoursBack*3600
  t.Stop<-Sys.time() #-24*7.5*3600
  #CheckTimeTicks(t.start = t.Start,t.stop = t.Stop)
  if (!is.na(minutes)) {
    df.Price<-GetPairPrices(Pair=pair,t.start = t.Start,t.stop = t.Stop, Minutes = minutes)
  } else if (!is.na(hours)) {
    df.Price<-GetPairPrices(Pair=pair,t.start = t.Start,t.stop = t.Stop, Hours = hours)
  }
  ############################################
  #pair<-names(df.Price)[1]
  suppressMessages(JW<-CleanOpen(df.Price))
  cat("\nCleaned")
  suppressWarnings(JW<-LabelUpDown(price=JW,multiple.pairs = FALSE))
  cat(" : Labeled")
  suppressMessages(JW<-LegBaseCount(price=JW))
  cat(" : Counted")
  suppressMessages(JW<-BuildHistory(price=JW))
  cat(" : History")
  suppressMessages(JW<-AppendIndicators(price=JW))
  cat(" : Indicators")
  suppressMessages(JW<-AppendFeatures(price=JW))
  cat(" : Features")
  i=nrow(JW)
  DrawChart(PriceData = JW[1:i,],SR=SR,Pair=pair,Vols=vols,PlotUFO=plot.ufo)
  cat(" : Drawn")
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

DrawChart<-function(PriceData,SR=TRUE,Pair=NA,Vols=6,PlotUFO=FALSE,PlotADX=TRUE,EntryModel=NA,EntryTarget=NA,ExitModel=NA,ExitTarget=NA,...) {
  #PriceData=JW[1:i,];SR=TRUE;Pair='GBP_USD';Vols=4;PlotUFO=TRUE;PlotADX=TRUE;EntryModel=NA;EntryTarget=NA;ExitModel=NA;ExitTarget=NA
  mult<-ifelse(floor(PriceData$Low[1]/10)>1,0.001,0.00001)
  mult2<-ifelse(floor(PriceData$Low[1]/10)>1,100,10000)
  VOLS<-list()
  if (Vols>1) {
    candles<-floor(nrow(PriceData)/Vols)
    for (v in 1:Vols) { #v=1
      ROWS<-(v-1)*candles+1:candles
      TICKS<-as.data.frame(do.call(rbind,apply(cbind(PriceData$Low[ROWS],PriceData$High[ROWS],PriceData$TickVolume[ROWS]),1,function(x) cbind(seq(from=x[1],to=x[2],by=mult),x[3]))))
      TICKS<-aggregate(V2~V1,TICKS,sum)
      XY<-density(TICKS$V1, weights=TICKS$V2/sum(TICKS$V2))
      VOLS[[paste0("vol",v)]]<-list(TS=max(ROWS),DENSITY=XY,TICKS=max(TICKS$V2))  #list(TS=PriceData$TimeStamp[max(ROWS)],DENSITY=XY,TICKS=max(TICKS$V2))
    }
  } else {
    ROWS<-1:nrow(PriceData)
    TICKS<-as.data.frame(do.call(rbind,apply(cbind(PriceData$Low[ROWS],PriceData$High[ROWS],PriceData$TickVolume[ROWS]),1,function(x) cbind(seq(from=x[1],to=x[2],by=mult),x[3]))))
    TICKS<-aggregate(V2~V1,TICKS,sum)
    XY<-density(TICKS$V1, weights=TICKS$V2/sum(TICKS$V2))
    VOLS[[paste0("vol",v)]]<- list(TS=max(ROWS),DENSITY=XY,TICKS=max(TICKS$V2)) #list(TS=PriceData$TimeStamp[max(ROWS)],DENSITY=XY,TICKS=max(TICKS$V2))
  }
  if (is.na(EntryModel)[1] & is.na(ExitModel)[1]) {
    nf<-layout(matrix(c(1,2),nrow=2,ncol=1),1,c(2,5))
  } else {
    PriceData$Move4<-mult2*c(PriceData$Close[5:nrow(PriceData)]-PriceData$Open[5:nrow(PriceData)-3],NA,NA,NA,NA)
    PriceData$Move3<-mult2*c(PriceData$Close[4:nrow(PriceData)]-PriceData$Open[4:nrow(PriceData)-2],NA,NA,NA)
    PriceData$Move2<-mult2*c(PriceData$Close[3:nrow(PriceData)]-PriceData$Open[3:nrow(PriceData)-1],NA,NA)
    PriceData$Move1<-mult2*c(PriceData$Close[2:nrow(PriceData)]-PriceData$Open[2:nrow(PriceData)],NA)
    Nnf<-(0+!is.na(EntryModel)[1])+(!is.na(ExitModel)[1]+0)
    nf<-layout(matrix(c(1:(2+Nnf)),nrow=(2+Nnf),ncol=1),1,c(1,rep(1,Nnf),3))
  }
  TimeX<-1:nrow(PriceData)
  #layout.show(nf)
  XLIM<-c(TimeX[1],nrow(PriceData))+c(ifelse(Vols==1,-50,-1),30)
  par(mar=c(0,3,2,1))
  plot(TimeX,PriceData$rsi14,type="n",ylim=c(0,100),xaxt="n",main=Pair,xlim=XLIM)
  lines(TimeX,PriceData$rsi14,col=2,lwd=3)
  lines(TimeX,PriceData$adx14,col=6,lwd=1.5)
  lines(TimeX,PriceData$adx5,col=4,lwd=1.5)
  abline(h=c(40,60),col=4,lty=2)
  lines(TimeX,50+30*PriceData$cci13/max(abs(PriceData$cci13),na.rm = T),col=3)
  legend("topleft",legend = c("RSI(14)","CCI(13)","ADX(5)","ADX(14)"),col=c(2,3,4,6),lty=1)
  CSpace<-0.4*min(diff(TimeX))
  alph<-0.35*((0.9*PriceData$TickVolume)/max(PriceData$TickVolume))^2.6
  vCol<-col2rgb(rainbow(length(TimeX),start = 0.65,end=0.7)[length(TimeX):1])
  
  if (!is.na(EntryModel)[1] & EntryTarget %in% names(PriceData)) {
    PriceData$UpDown<-factor(PriceData$UpDown)
    PriceData$TrendState<-factor(PriceData$TrendState)
    PriceData$LegBase<-factor(PriceData$LegBase)
    if (grepl("lm",EntryModel$call)) {
      EntryMoves<-predict(EntryModel,newdata = PriceData)
    } else if (grepl("gbm",EntryModel$call)) {
      EntryMoves<-predict(EntryModel,newdata = PriceData,n.trees = 100)
    }
    YLIM = c(-1,1)*1.2*max(abs(PriceData[,EntryTarget]),na.rm = T)
    par(mar=c(0,3,0,1))
    plot(XLIM,YLIM,type="n",ylim=YLIM,xlim=XLIM,xaxt="n")
    rect(TimeX-CSpace,ifelse(PriceData[,EntryTarget]>0,0,PriceData[,EntryTarget]),
         TimeX+CSpace,ifelse(PriceData[,EntryTarget]>0,PriceData[,EntryTarget],0),
         col=ifelse(PriceData[,EntryTarget]>0,"green","red"),border = NA)
    TF <- !is.na(EntryMoves*PriceData[,EntryTarget])
    points(TimeX[TF],EntryMoves[TF],pch=16,col=c(1,7)[1+(EntryMoves[TF]*PriceData[TF,EntryTarget]>0)]) #,col=c(1,7)[1+(EntryMoves*PriceData[,EntryTarget])>1])
  }
  if (!is.na(ExitModel)[1] & ExitTarget %in% names(PriceData)) {
    PriceData$UpDown<-factor(PriceData$UpDown)
    PriceData$TrendState<-factor(PriceData$TrendState)
    PriceData$LegBase<-factor(PriceData$LegBase)
    if (grepl("lm",ExitModel$call)) {
      ExitMoves<-predict(ExitModel,newdata = PriceData)
    } else if (grepl("gbm",ExitModel$call)) {
      ExitMoves<-predict(ExitModel,newdata = PriceData,n.trees = 100)
    }
    YLIM = c(-1,1)*1.2*max(abs(PriceData[,ExitTarget]),na.rm = T)
    par(mar=c(0,3,0,1))
    plot(XLIM,YLIM,type="n",ylim=YLIM,xlim=XLIM,xaxt="n")
    rect(TimeX-CSpace,ifelse(PriceData[,ExitTarget]>0,0,PriceData[,ExitTarget]),
         TimeX+CSpace,ifelse(PriceData[,ExitTarget]>0,PriceData[,ExitTarget],0),
         col=ifelse(PriceData[,ExitTarget]>0,"green","red"),border = NA)
    TF<-!is.na(ExitMoves*PriceData[,EntryTarget])
    points(TimeX[TF],ExitMoves[TF],pch=16,col=c(1,7)[(ExitMoves[TF]*PriceData[TF,ExitTarget]>0)+1]) #,col=c(1,7)[1+(EntryMoves*PriceData[,EntryTarget])>1])
  }
  YLIM<-c(0.9998,1.0002)*c(min(PriceData$Low),max(PriceData$High))
  par(mar=c(3,3,0,1))
  plot(XLIM,YLIM,type="n",ylim=YLIM,xlim=XLIM,xaxt="n",...)
  PriceAxis(PriceData=PriceData)
  if (mult==0.001) {
    abline(h=c(floor(range(PriceData$Open)[1]):ceiling(range(PriceData$Open)[2])),col=4)
    abline(h=c(floor(range(PriceData$Open*10)[1]):ceiling(range(PriceData$Open*10)[2]))/10,col=5)
  } else {
    abline(h=c(floor(range(PriceData$Open*10)[1]):ceiling(range(PriceData$Open*10)[2]))/10,col=4)
    abline(h=c(floor(range(PriceData$Open*100)[1]):ceiling(range(PriceData$Open*100)[2]))/100,col=5)
  }
  
  for (j in 1:nrow(PriceData)) {
    rect(xleft = TimeX[j],ybottom = PriceData$Low[j],xright =XLIM[2],ytop = PriceData$High[j],border = NA,
         col = rgb(red = vCol[1,j]/255,green = vCol[2,j]/255,blue = vCol[3,j]/255,alpha = alph[j]))
  }
  arrows(TimeX,PriceData$Low,TimeX,PriceData$High,length = 0)
  rect(TimeX-CSpace,ifelse(PriceData$Open>PriceData$Close,PriceData$Close,PriceData$Open),
       TimeX+CSpace,ifelse(PriceData$Open<PriceData$Close,PriceData$Close,PriceData$Open),
       col=ifelse(PriceData$Open<PriceData$Close,"green","red"))
  if ("TrendState" %in% names(PriceData)) {
    TS.label<-paste(PriceData$TrendStateP3[nrow(PriceData)],PriceData$TrendStateP2[nrow(PriceData)],
                    PriceData$TrendStateP1[nrow(PriceData)],PriceData$TrendState[nrow(PriceData)],sep="-")
    text(TimeX[ceiling(0.85*nrow(PriceData))],YLIM[1]+0.98*abs(diff(YLIM)),TS.label)
  }
  lines(TimeX,PriceData$ema8,col="red",lwd=3)
  lines(TimeX,PriceData$ema21,col="green",lwd=3)
  lines(TimeX,PriceData$ema50,col="blue",lwd=3)
  if (Vols>1) {
    VOL.Height <- floor(nrow(PriceData)/(Vols+1))  #difftime(VOLS[[2]]$TS,VOLS[[1]]$TS,units = "secs")
    MAXV<-max(sapply(VOLS,function(x) x$TICKS))
    for (v in 1:length(VOLS)) { #v=1
      Height.mult<-VOL.Height/1.5*(VOLS[[v]]$TICKS/MAXV)
      X<-VOLS[[v]]$TS+1+Height.mult*VOLS[[v]]$DENSITY$y/max(VOLS[[v]]$DENSITY$y)#*as.numeric(VOL.Height))
      Y<-VOLS[[v]]$DENSITY$x
      polygon(x = c(X,X[1]),y = c(Y,Y[1]),col = rgb(red = 0,green = 0,blue = 1,alpha = 0.5),border = NA)
    }
  } else {
    VOL.Height <- 50 #difftime(PriceData$TimeStamp[50],PriceData$TimeStamp[1])
    MAXV<-max(VOLS[[1]]$TICKS)
    Height.mult<-VOL.Height/(VOLS[[1]]$TICKS/MAXV)
    X<-XLIM[1]+1+Height.mult*VOLS[[1]]$DENSITY$y/max(VOLS[[1]]$DENSITY$y)#*as.numeric(VOL.Height))
    Y<-VOLS[[1]]$DENSITY$x
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
    if (difftime(PriceData$TimeStamp[2],PriceData$TimeStamp[1],units = "min")<=60 & 
        difftime(PriceData$TimeStamp[nrow(PriceData)],PriceData$TimeStamp[1],units = "min")<30000) {
      UFO<-UFOs(PairPrice=PriceData,PlotPair=Pair,minutes=30,hours=NA) 
      rect(xleft =  nrow(PriceData)+4 , #PriceData$TimeStamp[nrow(PriceData)]+difftime(PriceData$TimeStamp[4],PriceData$TimeStamp[1]),
           ybottom = UFO$low[UFO$counts==1],
           xright =   nrow(PriceData)+12, #PriceData$TimeStamp[nrow(PriceData)]+difftime(PriceData$TimeStamp[12],PriceData$TimeStamp[1]),
           ytop = UFO$high[UFO$counts==1],
           col = ifelse(UFO$low[UFO$counts==1]>PriceData$Open[nrow(PriceData)],"blue1","red1"),border = NA) }
    if (difftime(PriceData$TimeStamp[2],PriceData$TimeStamp[1],units = "hour")<=8 & 
        difftime(PriceData$TimeStamp[nrow(PriceData)],PriceData$TimeStamp[1],units = "hour")<4000) {
      UFO<-UFOs(PairPrice=PriceData,PlotPair=Pair,minutes=NA,hours=4) 
      rect(xleft =  nrow(PriceData)+12 , #PriceData$TimeStamp[nrow(PriceData)]+difftime(PriceData$TimeStamp[12],PriceData$TimeStamp[1]),
           ybottom = UFO$low[UFO$counts==1],
           xright = nrow(PriceData)+20  , #PriceData$TimeStamp[nrow(PriceData)]+difftime(PriceData$TimeStamp[20],PriceData$TimeStamp[1]),
           ytop = UFO$high[UFO$counts==1],
           col = ifelse(UFO$low[UFO$counts==1]>PriceData$Open[nrow(PriceData)],"blue2","red2"),border = NA) }
    if (difftime(PriceData$TimeStamp[2],PriceData$TimeStamp[1],units = "day")<=3 & 
        difftime(PriceData$TimeStamp[nrow(PriceData)],PriceData$TimeStamp[1],units = "day")<1000) {
      UFO<-UFOs(PairPrice=PriceData,PlotPair=Pair,minutes=NA,hours=NA,daily=TRUE) 
      rect(xleft =  nrow(PriceData)+20 , #PriceData$TimeStamp[nrow(PriceData)]+difftime(PriceData$TimeStamp[20],PriceData$TimeStamp[1]),
           ybottom = UFO$low[UFO$counts==1],
           xright =  nrow(PriceData)+28  , #PriceData$TimeStamp[nrow(PriceData)]+difftime(PriceData$TimeStamp[28],PriceData$TimeStamp[1]),
           ytop = UFO$high[UFO$counts==1],
           col = ifelse(UFO$low[UFO$counts==1]>PriceData$Open[nrow(PriceData)],"blue3","red3"),border = NA) }
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

PlotIndices<-function(inds,hinds=NA,linds=NA,maxpair=FALSE,Legend=TRUE,newYLim=FALSE,MinBuffer=20,...) {
  #inds=IND$Ind.df
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
    plot(inds[,1],inds[,2],type="n",xlab="time",ylim=YLIM,xlim=range(inds[,1])+c(0,MinBuffer*60),...)
  } else {
    plot(inds[,1],inds[,2],type="n",xlab="time",xlim=range(inds[,1])+c(0,30*difftime(inds[2,1],inds[1,1])),...)
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

FixTimePlot<-function(PriceData) {
  nf<-layout(matrix(c(1,2),nrow=2,ncol=1),1,c(1,3))
  par(mar=c(0,3,2,1))

  PriceData$TimeX<-1:nrow(PriceData)
  plot(PriceData$TimeX,PriceData$rsi14,type="n",ylim=c(0,100),xaxt="n",main=Pair)
  lines(PriceData$TimeX,PriceData$rsi14,col=2,lwd=1)
  lines(PriceData$TimeX,PriceData$adx14,col=6,lwd=1.5)
  lines(PriceData$TimeX,PriceData$adx5,col=4,lwd=1.5)
  abline(h=c(40,60),col=4,lty=2)
  lines(PriceData$TimeX,50+30*PriceData$cci13/max(abs(PriceData$cci13),na.rm = T),col=3)
  legend("topleft",legend = c("RSI(14)","CCI(13)","ADX(5)","ADX(14)"),col=c(2,3,4,6),lty=1)
  CSpace<-0.4*min(diff(PriceData$TimeX))
  alph<-0.25*((0.9*PriceData$TickVolume)/max(PriceData$TickVolume))^3

  XLIM<-c(1,round(1.08*nrow(PriceData)))  #***

  YLIM<-c(0.9998,1.0002)*c(min(PriceData$Low),max(PriceData$High))
  par(mar=c(3,3,0,1))

  plot(XLIM,YLIM,type="n",ylim=YLIM,xlim=XLIM,xaxt="n")  #***

  if (mult==0.001) {
    abline(h=c(floor(range(PriceData$Open)[1]):ceiling(range(PriceData$Open)[2])),col=4)
    abline(h=c(floor(range(PriceData$Open*10)[1]):ceiling(range(PriceData$Open*10)[2]))/10,col=5)
  } else {
    abline(h=c(floor(range(PriceData$Open*10)[1]):ceiling(range(PriceData$Open*10)[2]))/10,col=4)
    abline(h=c(floor(range(PriceData$Open*100)[1]):ceiling(range(PriceData$Open*100)[2]))/100,col=5)
  }

  for (j in 1:nrow(PriceData)) {
    rect(xleft = PriceData$TimeX[j],ybottom = PriceData$Low[j],xright =PriceData$TimeX[nrow(PriceData)],ytop = PriceData$High[j],border = NA,
         col = rgb(red = 0,green = 0,blue = 1,alpha = alph[j]))
  }
  arrows(PriceData$TimeX,PriceData$Low,PriceData$TimeX,PriceData$High,length = 0)
  rect(PriceData$TimeX-CSpace,ifelse(PriceData$Open>PriceData$Close,PriceData$Close,PriceData$Open),
       PriceData$TimeX+CSpace,ifelse(PriceData$Open<PriceData$Close,PriceData$Close,PriceData$Open),
       col=ifelse(PriceData$Open<PriceData$Close,"green","red"))
  if ("TrendState" %in% names(PriceData)) {
    TS.label<-paste(PriceData$TrendStateP3[nrow(PriceData)],PriceData$TrendStateP2[nrow(PriceData)],
                    PriceData$TrendStateP1[nrow(PriceData)],PriceData$TrendState[nrow(PriceData)],sep="-")
    text(PriceData$TimeX[ceiling(0.85*nrow(PriceData))],YLIM[1]+0.98*abs(diff(YLIM)),TS.label)
  }
  lines(PriceData$TimeX,PriceData$ema8,col="red",lwd=3)
  lines(PriceData$TimeX,PriceData$ema21,col="green",lwd=3)
  lines(PriceData$TimeX,PriceData$ema50,col="blue",lwd=3)
  axis
}

PriceAxis<-function(PriceData,ticks=6) {
  # if (!"TimeX" %in% names(PriceData)) {
  #   print("You forgot to number the rows in the Price")
  #   PriceData$TimeX<-1:nrow(PriceData)
  #   #return(NULL)
  # }
  slices<-(nrow(PriceData)/(ticks-1))
  ROWS<-floor(c(1,1:(ticks-1)*(slices-1)))
  ChopTimes<-sapply(format(as.POSIXct(PriceData$TimeStamp[ROWS]), format="%Y-%m-%d %H:%M:%S"),function(y)  mapply(substr,start=c(1,6,9,12,15),stop=c(4,7,10,13,16),x=y))
  small.Unit<-min(which(apply(ChopTimes,1,function(x) length(unique(x)))>=(ticks-1)))
  if (!is.infinite(max(which(apply(ChopTimes[1:(small.Unit-1),],1,function(x) length(unique(x)))>1)))) {
    big.Unit<-max(which(apply(ChopTimes[1:(small.Unit-1),],1,function(x) length(unique(x)))>1))
  } else {
    big.Unit<-small.Unit
  }
  if (big.Unit==2) {
    row.names(ChopTimes)<-c("%Y","%b","%d","%H","%M")
  } else if (big.Unit==3) {
    row.names(ChopTimes)<-c("%Y","%m","%a","%H","%M")
  } else if (big.Unit==4) {
    small.Unit<-4
    row.names(ChopTimes)<-c("%Y","%m","%d","%H:%M","%M")
  } else if (big.Unit==5) {
    small.Unit<-5
    row.names(ChopTimes)<-c("%Y","%m","%d","%H:%M","%H:%M:%S")
  } else {
    row.names(ChopTimes)<-c("%Y","%m","%d","%H","%M")
  }
  Sep=c("-","-"," ",":")
  Labels<-strftime(as.character(PriceData$TimeStamp[ROWS]),format =  paste0(row.names(ChopTimes)[unique(c(big.Unit,small.Unit))],collapse=Sep[big.Unit:(small.Unit-1)]))
  axis(1,at = ROWS,labels = Labels)
}
