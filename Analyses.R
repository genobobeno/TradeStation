
GetIndexes<-function(Price,pca.plot=TRUE,LowerUpper=FALSE,Choices=10,...) {
  # if (!is.na(t.start) & !is.na(t.stop)) {
  #   t.Start<-t.start
  #   t.Stop<-t.stop  #-12*3600
  # } else {
  #   t.Start<-Sys.time()-HoursBack*3600
  #   t.Stop<-Sys.time()  #-12*3600
  # }
  # CheckTimeTicks(t.start = t.Start,t.stop = t.Stop)
  # if (is.na(minutes)) {
  #   Price<-GetAllPrices(t.start = t.Start,t.stop = t.Stop, Seconds=seconds)
  # } else {
  #   Price<-GetAllPrices(t.start = t.Start,t.stop = t.Stop, Seconds=NA,Minutes = minutes)
  # }
  df.Price<-CleanOpen(Price)
  df.Price<-CleanPriceList(df.Price)

  USD<-IndexCurrency("USD",price.list=df.Price,ticker="Close")
  EUR<-IndexCurrency("EUR",price.list=df.Price,ticker="Close")
  GBP<-IndexCurrency("GBP",price.list=df.Price,ticker="Close")
  JPY<-IndexCurrency("JPY",price.list=df.Price,ticker="Close")
  CHF<-IndexCurrency("CHF",price.list=df.Price,ticker="Close")
  CAD<-IndexCurrency("CAD",price.list=df.Price,ticker="Close")
  AUD<-IndexCurrency("AUD",price.list=df.Price,ticker="Close")
  NZD<-IndexCurrency("NZD",price.list=df.Price,ticker="Close")
  
  if (!LowerUpper) {
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
    
    INDSh<-data.frame(TimeStamp=df.Price$EUR_CHF$TimeStamp,USD=10000*USDh$Scores[,1],
                     EUR=10000*EURh$Scores[,1],GBP=10000*GBPh$Scores[,1],JPY=10000*JPYh$Scores[,1],
                     CHF=10000*CHFh$Scores[,1],CAD=10000*CADh$Scores[,1],AUD=10000*AUDh$Scores[,1],
                     NZD=10000*NZDh$Scores[,1])
    
    INDSl<-data.frame(TimeStamp=df.Price$EUR_CHF$TimeStamp,USD=10000*USDl$Scores[,1],
                     EUR=10000*EURl$Scores[,1],GBP=10000*GBPl$Scores[,1],JPY=10000*JPYl$Scores[,1],
                     CHF=10000*CHFl$Scores[,1],CAD=10000*CADl$Scores[,1],AUD=10000*AUDl$Scores[,1],
                     NZD=10000*NZDl$Scores[,1])
    INDSh.c<-data.frame(lapply(INDSh[,-1],function(x) (x-x[1])))
    INDSh[,2:length(INDSh)]<-INDSh.c
    INDSl.c<-data.frame(lapply(INDSl[,-1],function(x) (x-x[1])))
    INDSl[,2:length(INDSl)]<-INDSl.c
    
  }
  
  INDS<-data.frame(TimeStamp=df.Price$EUR_CHF$TimeStamp,USD=10000*USD$Scores[,1],
                   EUR=10000*EUR$Scores[,1],GBP=10000*GBP$Scores[,1],JPY=10000*JPY$Scores[,1],
                   CHF=10000*CHF$Scores[,1],CAD=10000*CAD$Scores[,1],AUD=10000*AUD$Scores[,1],
                   NZD=10000*NZD$Scores[,1])
  INDS.c<-data.frame(lapply(INDS[,-1],function(x) (x-x[1])))
  INDS[,2:length(INDS)]<-INDS.c
  
  if (pca.plot) {
    #par(mfrow=c(1,1),mar=c(3,3,2,1))
    PlotIndices(INDS,...)
  }
  ALL<-princomp(cbind(USD$Scores[,1],EUR$Scores[,1],GBP$Scores[,1],JPY$Scores[,1],
                      CHF$Scores[,1],CAD$Scores[,1],AUD$Scores[,1],NZD$Scores[,1]))
  
  r.c<-cor(cbind(ALL$scores[,1],USD$Scores[,1],EUR$Scores[,1],GBP$Scores[,1],JPY$Scores[,1],
                 CHF$Scores[,1],CAD$Scores[,1],AUD$Scores[,1],NZD$Scores[,1]))
  row.names(r.c)<-c("ALL","USD","EUR","GBP","JPY","CHF","CAD","AUD","NZD")
  colnames(r.c)<-c("ALL","USD","EUR","GBP","JPY","CHF","CAD","AUD","NZD")
  v.pc<-r.c[,1]
  print("Overall Correlation")
  print(round(r.c,digits=3))
  print(t(as.matrix(sort(round(v.pc,digits=3)))))
  R<-r.c[-1,-1]
  BP<-order(R)[2*1:Choices]
  print(BP)
  BestPairs<-matrix(0,nrow=Choices,ncol=2)
  for (i in 1:length(BP)) BestPairs[i,]<-which(R==R[BP[i]],arr.ind = TRUE)[1,]
  cat("\nBest Pairs RIGHT NOW:\n")
  for (i in 1:nrow(BestPairs)) cat(row.names(R)[BestPairs[i,1]],"-",colnames(R)[BestPairs[i,2]],"\t",R[BestPairs[i,1],BestPairs[i,2]],"\n")
  return(list(Ind.df=INDS,Ind.pc=list(USD=USD,EUR=EUR,GBP=GBP,JPY=JPY,CHF=CHF,
                                      CAD=CAD,AUD=AUD,NZD=NZD,Price=df.Price),
              TimeF=max(df.Price$EUR_CHF$TimeStamp)))
}

GetCorrelations<-function(price.list,TopPairs=10) {
  CPairs<-c("EUR_USD","GBP_USD","AUD_USD","NZD_USD","USD_CAD","USD_JPY","USD_CHF",
            "EUR_GBP","EUR_CHF","EUR_CAD","EUR_NZD","EUR_AUD","EUR_JPY",
            "GBP_AUD","GBP_NZD","GBP_JPY","GBP_CHF","GBP_CAD",
            "CAD_JPY","AUD_CAD","NZD_CAD","CAD_CHF",
            "AUD_NZD","AUD_JPY","AUD_CHF",
            "NZD_JPY","NZD_CHF",
            "CHF_JPY")
  Changes<-list()
  for (p in CPairs) {
    OA_In <- p
    PRICES<-price.list[[p]]
    Changes[[p]]<-data.frame(TimeStamp=PRICES$TimeStamp[-1],
                             change=diff(as.numeric(PRICES$Close))/as.numeric(PRICES$Close[-length(PRICES$Close)]))
    colnames(Changes[[p]])[2]<-p
    #Changes[[p]]<-diff(as.numeric(PRICES$Close))/as.numeric(PRICES$Close[-length(PRICES$Close)])
    Prices[[p]]<-PRICES
  }
  PAIRS<-TopPairs 
  PriceData<-Changes[[1]]
  for (i in 2:length(Changes)) {
    PriceData<-merge(PriceData,Changes[[i]],by="TimeStamp",all=TRUE)
    PriceData[is.na(PriceData[,length(PriceData)-1]),length(PriceData)-1]<-0
    PriceData[is.na(PriceData[,length(PriceData)]),length(PriceData)]<-0
  }
  Changes<-PriceData[,-1]
  RPairs<-cor(Changes)
  RSort<-sort(RPairs[RPairs<1])
  RSort<-RSort[!duplicated(RSort)]
  NPairs<-data.frame(pair1=character(),pair2=character(),link=numeric())
  PPairs<-data.frame(pair1=character(),pair2=character(),link=numeric())
  for (i in 1:PAIRS) {
    NPairs[i,]<-c(row.names(which(RPairs==RSort[i],arr.ind=TRUE)),round(RSort[i],digits=4))
    PPairs[i,]<-c(row.names(which(RPairs==RSort[length(RSort)-(i-1)],arr.ind=TRUE)),
                  round(RSort[length(RSort)-(i-1)],digits=4))
  }
  
  cat("\n\nTop",PAIRS,"Moving Together\n\n")
  cat("\t Pair1 \t\t\t Pair2 \t\t\t Correlation\n")
  for (i in 1:PAIRS) cat("\t",PPairs[i,1],"\t\t",PPairs[i,2],"\t\t",PPairs[i,3],"\n")
  cat("\n\nTop",PAIRS,"Moving Against Each Other\n\n")
  cat("\t Pair1 \t\t\t Pair2 \t\t\t Correlation\n")
  for (i in 1:PAIRS) cat("\t",NPairs[i,1],"\t\t",NPairs[i,2],"\t\t",NPairs[i,3],"\n")
}

IndexCurrency<-function(Currency,price.list,ticker="Close") {
  #Currency = "USD"; price.list=df.Price; ticker="Close"
  if (Currency=="JPY") {
    mult<-0.01
  } else {
    mult<-1
  }
  i.l<-grep(Currency,names(price.list))
  t.stamp<-c()
  for (i in i.l) { #unlist(lapply())
    t.stamp<-c(t.stamp,price.list[[i]]$TimeStamp)
  }  
  t.stamp<-unique(t.stamp)
  for (i in i.l) {
    t.stamp<-t.stamp[t.stamp %in% price.list[[i]]$TimeStamp]
  }
  for (i in i.l) {
    price.list[[i]]<-price.list[[i]][price.list[[i]]$TimeStamp %in% t.stamp,]
  }
  label<-c()
  for (i in i.l) {
    if (i==i.l[1]) {
      if (grepl(Currency,strsplit(names(price.list)[i],"_")[[1]][1])) {
        if (grepl("JPY",names(price.list)[i])) mult2<-0.01 else mult2<-1
        p<-mult2*as.numeric(price.list[[i]][,ticker])
        #print(paste("bound",names(price.list)[i]))
        label<-c(label,names(price.list)[i])
      } else {
        p<-1/(mult*as.numeric(price.list[[i]][,ticker]))
        #print(paste("bound inverted",names(price.list)[i]))
        label<-c(label,paste0("i",names(price.list)[i]))
      }
    } else {
      if (grepl(Currency,strsplit(names(price.list)[i],"_")[[1]][1])) {
        if (grepl("JPY",names(price.list)[i])) mult2<-0.01 else mult2<-1
        p<-cbind(p,mult2*as.numeric(price.list[[i]][,ticker]))
        #print(paste("bound",names(price.list)[i]))
        label<-c(label,names(price.list)[i])
      } else {
        p<-cbind(p,1/(mult*as.numeric(price.list[[i]][,ticker])))
        #print(paste("bound inverted",names(price.list)[i]))
        label<-c(label,paste0("i",names(price.list)[i]))
      }
    }
  }
  colnames(p)<-label
  idx<-apply(p,1,prod)

  PRCOMP_Model<-prcomp(p,center = TRUE,scale. = FALSE)

  if (sum(apply(p,2,function(x) cor(x,PRCOMP_Model$x[,1])>0))<4) {
    PRCOMP_Model$x[,1]<- (-1)*PRCOMP_Model$x[,1]
    PRCOMP_Model$flipC = (-1)
  } else {
    PRCOMP_Model$flipC = 1
  }
  
  PC<-princomp(p)
  
  if (sum(apply(p,2,function(x) cor(x,PC$scores[,1])>0))<4) {
    PC$scores[,1]<- (-1)*PC$scores[,1]
    PC$flipC = (-1)
  } else {
    PC$flipC = 1
  }
  
  # PC$center.scores<-PC$scores[,1]-PC$scores[1,1]    
  
  #PC$scores
  list(Scores=PC$scores,PC=PC,PRCOMP=PRCOMP_Model)
  # better to return a list with multiple descriptors
  # list(index=PC$scores[,1],comps=PC$scores,currency=Currency,ticker=ticker)
}


UFOs<-function(PairPrice,PlotPair,minutes=NA,hours=NA,daily=FALSE) {
  if (!is.na(minutes)) {
    UFOPrice<-GetPairPrices(Pair=PlotPair,
                            t.start = min(PairPrice$TimeStamp)-difftime(max(PairPrice$TimeStamp),min(PairPrice$TimeStamp)),
                            t.stop = max(PairPrice$TimeStamp), Minutes = minutes, Hours=NA)
  } else if (!is.na(hours)) {
    UFOPrice<-GetPairPrices(Pair=PlotPair,
                            t.start = min(PairPrice$TimeStamp)-difftime(max(PairPrice$TimeStamp),min(PairPrice$TimeStamp)),
                            t.stop = max(PairPrice$TimeStamp),Hours = hours, Minutes = NA)
  } else if (daily) {
    UFOPrice<-GetPairPrices(Pair=PlotPair,t.start = min(PairPrice$TimeStamp)-difftime(max(PairPrice$TimeStamp),min(PairPrice$TimeStamp)),
                            t.stop = max(PairPrice$TimeStamp),Daily=TRUE,Hours = NA, Minutes = NA)
  }
  
  UFOPrice<-CleanOpen(UFOPrice)
  UFOPrice<-LabelUpDown(price=UFOPrice,multiple.pairs = FALSE)
  mult<-ifelse(floor(UFOPrice$Low[1]/10)>1,0.001,0.00001)  
  arr<-seq(from=min(UFOPrice$Low),to=max(UFOPrice$High),by=mult)
  UFO<-data.frame(price=arr,counts=0)
  h.lookback<-max(c(UFOPrice$Open[nrow(UFOPrice)],UFOPrice$Close[nrow(UFOPrice)]))
  l.lookback<-min(c(UFOPrice$Open[nrow(UFOPrice)],UFOPrice$Close[nrow(UFOPrice)]))
  for (i in (nrow(UFOPrice)-1):1) {
   # print(h.lookback)
    h.lookback<-max(h.lookback,ifelse(UFOPrice$UpDown[i]=="Up",UFOPrice$Close[i],UFOPrice$Open[i])) 
   # print(l.lookback)
    l.lookback<-min(l.lookback,ifelse(UFOPrice$UpDown[i]=="Down",UFOPrice$Close[i],UFOPrice$Open[i]))
    high.ufo<-UFO$price>h.lookback & UFO$price<=UFOPrice$High[i]
    low.ufo<-UFO$price<l.lookback & UFO$price>=UFOPrice$Low[i]
    if (sum(c(high.ufo,low.ufo))>0) UFO$counts[(high.ufo|low.ufo)]<-UFO$counts[(high.ufo|low.ufo)]+1
  }
  UFO$counts<-UFO$counts%%2
  UFO$low<-UFO$price-mult/2
  UFO$high<-UFO$price+mult/2
  UFO[,c("counts","low","high")]
}

PCA_Continue<-function(PCA_INDEX,LookBackHours=1,update.graph=TRUE) {
  NewPrice<-GetAllPrices(Minutes = 1,LookBackHours = LookBackHours)
  NewPrice<-CleanOpen(NewPrice)
  NewPrice<-CleanPriceList(NewPrice)
  s.d<-c("USD","EUR","GBP","JPY","CHF","CAD","AUD","NZD")
  cols<-c("black","darkgreen","blue","red","orange","orangered","lightblue","purple")
  for (d in s.d) { #d=s.d[1]
    d.pairs<-names(PCA_INDEX$Ind.pc[[d]]$PRCOMP$rotation[,1])
    d.mult<-ifelse(grepl("JPY",d.pairs),0.01,1)
    d.exp<-ifelse(grepl("i",d.pairs),-1,1)
    d.data<-lapply(NewPrice[str_remove_all(d.pairs,"i")],function(x) x[x$TimeStamp>=(PCA_INDEX$TimeF-125),])
    d.Index<-10000*PCA_INDEX$Ind.pc[[d]]$PRCOMP$flipC*((as.matrix(sapply(seq_along(d.data),
                                                                         function(x) (d.mult[x]*as.numeric(d.data[[x]]$Close))^d.exp[x])) - 
                                                          matrix(rep(PCA_INDEX$Ind.pc[[d]]$PRCOMP$center,nrow(d.data[[1]])),
                                                                 ncol=length(PCA_INDEX$Ind.pc[[d]]$PRCOMP$center),
                                                                 nrow=nrow(d.data[[1]]),byrow = T))%*%PCA_INDEX$Ind.pc[[d]]$PRCOMP$rotation[,1]) - 10000*PCA_INDEX$Ind.pc[[d]]$PC$scores[1,1]
    if (update.graph) {
      lines(d.data[[1]]$TimeStamp,d.Index,col=cols[which(s.d==d)],lwd=2)
    }
    if (which(s.d==d)==1) {
      INDEX<-data.frame(TimeStamp=d.data[[1]]$TimeStamp)
    }
    INDEX[[d]]<-d.Index
  }  
  return(INDEX)
}


CurrStrength<-function(PCA_INDEX,Windows=c(2:5),plot.momentum=TRUE) {
  #PCA_INDEX<-my.ind;Windows=c(2:5);plot.momentum=TRUE
  
  CPairs<-c("EUR_USD","GBP_USD","AUD_USD","NZD_USD","USD_CAD","USD_JPY","USD_CHF",
            "EUR_GBP","EUR_CHF","EUR_CAD","EUR_NZD","EUR_AUD","EUR_JPY",
            "GBP_AUD","GBP_NZD","GBP_JPY","GBP_CHF","GBP_CAD",
            "CAD_JPY","AUD_CAD","NZD_CAD","CAD_CHF",
            "AUD_NZD","AUD_JPY","AUD_CHF",
            "NZD_JPY","NZD_CHF",
            "CHF_JPY")
  
  # Build first and second order delta
  Ind.d1<-data.frame(TimeStamp=PCA_INDEX$Ind.df$TimeStamp[2:nrow(PCA_INDEX$Ind.df)],apply(PCA_INDEX$Ind.df[,2:9],2,diff))
  Ind.dd1<-data.frame(TimeStamp=Ind.d1$TimeStamp[2:nrow(Ind.d1)],apply(Ind.d1[,2:9],2,diff))
  
  inds<-2:9
  # Now let's dot products of first and second order deltas between each currency
  r<-list()
  # We want sums of these dot products across 2,3,4,5 ticks
  S<-Windows
  N<-nrow(Ind.dd1)
  cur.dot<-list()
  for (s in S) {
    ps<-mat.or.vec(nr=(N-s+1),nc=(length(inds[-1])*length(inds))/2)
    colnames(ps)<-paste("V",1:((length(inds[-1])*length(inds))/2))
    d<-1
    for (j in inds) {
      for (k in inds[inds!=j]) {
        if (k>j) {
          dotp<-Ind.d1[-1,j]*Ind.d1[-1,k]+Ind.dd1[,j]*Ind.dd1[,k]
          ps[,d]<-rowSums(outer(1:(N-s+1),1:s,function(ii,jj) {dotp[(jj-1)+ii]}))
          colnames(ps)[d]<-CPairs[grepl(names(Ind.d1)[j],CPairs) & grepl(names(Ind.d1)[k], CPairs)]
          d<-d+1
        }
      }
    }
    cur.dot[[paste0("Window",s)]]<-ps
  }
  
  
  if (plot.momentum) {  
    # Now, we can sum the L2-Norm of each row... giving some semblance of "Movement" across each window size
    # Visualize that movement under the PCA
    par(mar=c(5,4,3,3),mfrow=c(1,1))
    nf<-layout(matrix(c(1,2,3,3,4,4,5,5,6,6),nrow=2,ncol=5),c(4,1,1,1,1),c(1,1))
    #layout.show(nf)
    # Plot PCA
    PlotIndices(PCA_INDEX$Ind.df,maxpair = F)
    # Plot Total Dot Product
    s=S[1]
    dot.df<-as.data.frame(cur.dot[[paste0("Window",s)]])
    dot.df$ALL<-apply(dot.df,1,function(x) sqrt(sum(x^2)))
    ALL<-dot.df$ALL/max(dot.df$ALL,na.rm = T)
    plot(Ind.dd1$TimeStamp[-c(1:(s-1))],ALL,main="Dynamics",xlim=range(PCA_INDEX$Ind.df$TimeStamp)+c(0,20*60),xlab="time")
    
    for (s in S) {
      dot.df<-as.data.frame(cur.dot[[paste0("Window",s)]])
      dot.df$ALL<-apply(dot.df,1,function(x) sqrt(sum(x^2)))
      ALL<-dot.df$ALL/max(dot.df$ALL,na.rm = T)
      lines(Ind.dd1$TimeStamp[-c(1:(s-1))],ALL,col=s)
    }
    # Plot relative strength of pair movement
    for (s in S) {
      if (s==S[length(S)]) {
        par(mar=c(5,0.5,3,1))
      } else {
        par(mar=c(5,0.5,3,0.5))
      }
      dot.df<-as.data.frame(cur.dot[[paste0("Window",s)]])
      plot(c(0,1),-1.1*range(as.matrix(dot.df[nrow(dot.df),])),type="n",xlab=NA,ylab=NA,xaxt="n",yaxt="n",main=s)
      #axis(4,at = pretty(-1.1*range(as.matrix(dot.df[nrow(dot.df),]))),labels = pretty(-1.1*range(as.matrix(dot.df[nrow(dot.df),]))))
      rect(xleft = -0.5,
           ybottom = seq(from=-1.25*range(as.matrix(dot.df[nrow(dot.df),]))[2],to = -1.25*range(as.matrix(dot.df[nrow(dot.df),]))[1],length.out = 51)[1:50],
           xright = 1.5,
           ytop = seq(from=-1.25*range(as.matrix(dot.df[nrow(dot.df),]))[2],to = -1.25*range(as.matrix(dot.df[nrow(dot.df),]))[1],length.out = 51)[2:51],
           col = rainbow(50,start = 0,end = 0.55,alpha = 0.5),border = NA)
      text(x=0.5,y=-1*dot.df[nrow(dot.df),],labels = names(dot.df))
    }
    
    par(mar=c(5,4,3,3),mfrow=c(1,1))
  }
  par(mar=c(5,4,3,3),mfrow=c(1,1))
}


Impulsive<-function() {
  pM30<-GetAllPrices(Minutes = 30,LookBackHours = 72)
  pM30<-LabelThings(pM30)
  pH4<-GetAllPrices(Hours = 4,LookBackHours = 500)
  pH4<-LabelThings(pH4)
  pD<-GetAllPrices(Days = 100)
  pD<-LabelThings(pD)
  pW<-GetAllPrices(Weeks = 100)
  pW<-LabelThings(pW)
  
  p.dir<-sapply(names(pM30),function(x) {
    c(pM30[[x]]$Direction[nrow(pM30[[x]])],
      pH4[[x]]$Direction[nrow(pH4[[x]])],
      pD[[x]]$Direction[nrow(pD[[x]])],
      pW[[x]]$Direction[nrow(pW[[x]])])
  }) 
  
  p.trends<-sapply(names(pM30),function(x) {
    c(pM30[[x]]$TrendState[nrow(pM30[[x]])],
      pH4[[x]]$TrendState[nrow(pH4[[x]])],
      pD[[x]]$TrendState[nrow(pD[[x]])],
      pW[[x]]$TrendState[nrow(pW[[x]])])
  }) 
  
  p.col<-(p.dir=="Up")+2#matrix(2,nrow=nrow(p.col),ncol=ncol(p.col))
  #p.4Iup<-apply(p.dir,2,function(x) all(x[2:4]=="Up"))
  #p.4Idn<-apply(p.dir,2,function(x) all(x[2:4]=="Down"))
  getTableXY<-function(time.frame.array,price.direction.matrix,price.trend.matrix) {
    #time.frame.array=2:3;price.direction.matrix=p.dir;price.trend.matrix=p.trends
    if (max(time.frame.array)>4) {
      print("Fix Time Frame Problem")
      return(NA)
    }
    p.Iup<-apply(price.direction.matrix,2,function(x) all(x[time.frame.array]=="Up"))
    p.Idn<-apply(price.direction.matrix,2,function(x) all(x[time.frame.array]=="Down"))
    if (max(time.frame.array)<4) {
      time.frame.array<-c(time.frame.array,max(time.frame.array)+1)
    }
    X.Iup<-c(0:(sum(p.Iup)+1)*1/(sum(p.Iup)+1))
    X.Idn<-c(0:(sum(p.Idn)+1)*1/(sum(p.Idn)+1))
    Y.I<-(1-(0:(length(time.frame.array)+1))*1/(length(time.frame.array)+1))
    p.Iup.t<-price.trend.matrix[,p.Iup]
    p.Idn.t<-price.trend.matrix[,p.Idn]
    p.Iup.d<-price.direction.matrix[,p.Iup]
    p.Idn.d<-price.direction.matrix[,p.Idn]
    txt.Iup<-c("",names(p.Iup)[p.Iup])
    txt.Idn<-c("",names(p.Idn)[p.Idn])
    b.colors.up<-rep(0,length(txt.Iup))
    b.colors.dn<-rep(0,length(txt.Idn))
    for (i in time.frame.array) {
      txt.Iup<-c(txt.Iup,c("30M","4H","D","W")[i],p.Iup.t[i,])
      txt.Idn<-c(txt.Idn,c("30M","4H","D","W")[i],p.Idn.t[i,])
      b.colors.up<-c(b.colors.up,c(0,ifelse(p.Iup.d[i,]=="Up","lightgreen","red1")))
      b.colors.dn<-c(b.colors.dn,c(0,ifelse(p.Idn.d[i,]=="Down","red1","lightgreen")))
    }
    list(UP=list(y.t=Y.I[1:(length(Y.I)-1)],y.b=Y.I[2:length(Y.I)],
                 x.l=X.Iup[1:(length(X.Iup)-1)],x.r=X.Iup[2:length(X.Iup)],
                 txt=txt.Iup,cols=b.colors.up),
         DOWN=list(y.t=Y.I[1:(length(Y.I)-1)],y.b=Y.I[2:length(Y.I)],
                   x.l=X.Idn[1:(length(X.Idn)-1)],x.r=X.Idn[2:length(X.Idn)],
                   txt=txt.Idn,cols=b.colors.dn))
  }
  par(mfrow=c(4,1),mar=c(1,1,3,1))
  plot(c(0,1),c(0,1),type="n",main="Impulse-30M UPTRENDING",xaxt="n",yaxt="n")
  XY<-getTableXY(1:2,p.dir,p.trends)
  rect(xleft = rep(XY$UP$x.l,length(XY$UP$y.b)),ybottom = rep(XY$UP$y.b,rep(length(XY$UP$x.l),length(XY$UP$y.b))),
       xright = rep(XY$UP$x.r,length(XY$UP$y.b)),ytop = rep(XY$UP$y.t,rep(length(XY$UP$x.l),length(XY$UP$y.b))),col = XY$UP$cols,border = 1)
  text(x = rep((XY$UP$x.l+XY$UP$x.r)/2,length(XY$UP$y.b)),y = rep((XY$UP$y.b+XY$UP$y.t)/2,rep(length(XY$UP$x.l),length(XY$UP$y.b))),
       labels = c(XY$UP$txt),cex=ifelse(grepl("_",XY$UP$txt) | XY$UP$txt=="UTS" | XY$UP$txt %in% c("30M","4H","D","W"),1.2,1),
       font=ifelse(XY$UP$txt=="UTS",4,1))
  plot(c(0,1),c(0,1),type="n",main="Impulse-30M DOWNTRENDING",xaxt="n",yaxt="n")
  rect(xleft = rep(XY$DOWN$x.l,length(XY$DOWN$y.b)),ybottom = rep(XY$DOWN$y.b,rep(length(XY$DOWN$x.l),length(XY$DOWN$y.b))),
       xright = rep(XY$DOWN$x.r,length(XY$DOWN$y.b)),ytop = rep(XY$DOWN$y.t,rep(length(XY$DOWN$x.l),length(XY$DOWN$y.b))),col = XY$DOWN$cols,border = 1)
  text(x = rep((XY$DOWN$x.l+XY$DOWN$x.r)/2,length(XY$DOWN$y.b)),y = rep((XY$DOWN$y.b+XY$DOWN$y.t)/2,rep(length(XY$DOWN$x.l),length(XY$DOWN$y.b))),
       labels = c(XY$DOWN$txt),cex=ifelse(grepl("_",XY$DOWN$txt) | XY$DOWN$txt=="DTS" | XY$DOWN$txt %in% c("30M","4H","D","W"),1.2,1),
       font=ifelse(XY$DOWN$txt=="DTS",4,1))
  plot(c(0,1),c(0,1),type="n",main="Impulse-4H UPTRENDING",xaxt="n",yaxt="n")
  XY<-getTableXY(2:3,p.dir,p.trends)
  rect(xleft = rep(XY$UP$x.l,length(XY$UP$y.b)),ybottom = rep(XY$UP$y.b,rep(length(XY$UP$x.l),length(XY$UP$y.b))),
       xright = rep(XY$UP$x.r,length(XY$UP$y.b)),ytop = rep(XY$UP$y.t,rep(length(XY$UP$x.l),length(XY$UP$y.b))),col = XY$UP$cols,border = 1)
  text(x = rep((XY$UP$x.l+XY$UP$x.r)/2,length(XY$UP$y.b)),y = rep((XY$UP$y.b+XY$UP$y.t)/2,rep(length(XY$UP$x.l),length(XY$UP$y.b))),
       labels = c(XY$UP$txt),cex=ifelse(grepl("_",XY$UP$txt) | XY$UP$txt=="UTS" | XY$UP$txt %in% c("30M","4H","D","W"),1.2,1),
       font=ifelse(XY$UP$txt=="UTS",4,1))
  plot(c(0,1),c(0,1),type="n",main="Impulse-4H DOWNTRENDING",xaxt="n",yaxt="n")
  rect(xleft = rep(XY$DOWN$x.l,length(XY$DOWN$y.b)),ybottom = rep(XY$DOWN$y.b,rep(length(XY$DOWN$x.l),length(XY$DOWN$y.b))),
       xright = rep(XY$DOWN$x.r,length(XY$DOWN$y.b)),ytop = rep(XY$DOWN$y.t,rep(length(XY$DOWN$x.l),length(XY$DOWN$y.b))),col = XY$DOWN$cols,border = 1)
  text(x = rep((XY$DOWN$x.l+XY$DOWN$x.r)/2,length(XY$DOWN$y.b)),y = rep((XY$DOWN$y.b+XY$DOWN$y.t)/2,rep(length(XY$DOWN$x.l),length(XY$DOWN$y.b))),
       labels = c(XY$DOWN$txt),cex=ifelse(grepl("_",XY$DOWN$txt) | XY$DOWN$txt=="DTS" | XY$DOWN$txt %in% c("30M","4H","D","W"),1.2,1),
       font=ifelse(XY$DOWN$txt=="DTS",4,1))
  
  Impulse30MUp=p.trends[,apply(p.dir,2,function(x) all(x[1:2]=="Up"))]
  Impulse30MDown=p.trends[,apply(p.dir,2,function(x) all(x[1:2]=="Down"))]
  Impulse4HUp=p.trends[,apply(p.dir,2,function(x) all(x[2:3]=="Up"))]
  Impulse4HDown=p.trends[,apply(p.dir,2,function(x) all(x[2:3]=="Down"))]
  par(mar=c(5,4,3,3),mfrow=c(1,1))
  list(Impulse30MUp=Impulse30MUp,
       Impulse30MDown=Impulse30MDown,
       Impulse4HUp=Impulse4HUp,
       Impulse4HDown=Impulse4HDown)
}



