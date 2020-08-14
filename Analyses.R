
GetIndexes<-function(Price,pca.plot=TRUE,...) {
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
  
  INDS<-data.frame(TimeStamp=df.Price$EUR_CHF$TimeStamp,USD=100*USD[,1],
                   EUR=100*EUR[,1],GBP=100*GBP[,1],JPY=100000*JPY[,1],
                   CHF=100*CHF[,1],CAD=100*CAD[,1],AUD=100*AUD[,1],NZD=100*NZD[,1])
  INDS.c<-data.frame(lapply(INDS[,-1],function(x) (x-x[1])))
  INDS[,2:length(INDS)]<-INDS.c
  
  if (pca.plot) {
    #par(mfrow=c(1,1),mar=c(3,3,2,1))
    PlotIndices(INDS,...)
  }
  ALL<-princomp(cbind(USD[,1],EUR[,1],GBP[,1],JPY[,1],CHF[,1],CAD[,1],AUD[,1],NZD[,1]))
  
  r.c<-cor(cbind(ALL$scores[,1],USD[,1],EUR[,1],GBP[,1],JPY[,1],CHF[,1],CAD[,1],AUD[,1],NZD[,1]))
  row.names(r.c)<-c("ALL","USD","EUR","GBP","JPY","CHF","CAD","AUD","NZD")
  colnames(r.c)<-c("ALL","USD","EUR","GBP","JPY","CHF","CAD","AUD","NZD")
  v.pc<-r.c[,1]
  print("Overall Correlation")
  print(round(r.c,digits=3))
  print(as.matrix(sort(round(v.pc,digits=3))))
  return(list(Ind.df=INDS,Ind.pc=ALL))
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
  
  for (i in i.l) {
    if (i==i.l[1]) {
      if (grepl(Currency,strsplit(names(price.list)[i],"_")[[1]][1])) {
        p<-as.numeric(price.list[[i]][,ticker])
        #print(paste("bound",names(price.list)[i]))
      } else {
        p<-1/as.numeric(price.list[[i]][,ticker])
        #print(paste("bound inverted",names(price.list)[i]))
      }
    } else {
      if (grepl(Currency,strsplit(names(price.list)[i],"_")[[1]][1])) {
        p<-cbind(p,as.numeric(price.list[[i]][,ticker]))
        #print(paste("bound",names(price.list)[i]))
      } else {
        p<-cbind(p,1/as.numeric(price.list[[i]][,ticker]))
        #print(paste("bound inverted",names(price.list)[i]))
      }
    }
  }
  idx<-apply(p,1,prod)
  PC<-princomp(p)
  #if (cor(idx,PC$scores[,1])<0) PC$scores[,1]<- (-1)*PC$scores[,1]
  #for (i in 1:ncol(p)) {
  if (sum(apply(p,2,function(x) cor(x,PC$scores[,1])>0))<4) PC$scores[,1]<- (-1)*PC$scores[,1]
  PC$scores
  # better to return a list with multiple descriptors
  # list(index=PC$scores[,1],comps=PC$scores,currency=Currency,ticker=ticker)
}


UFOs<-function(PairPrice,PlotPair,minutes=30,hours=NA) {
  if (!is.na(minutes)) {
    UFOPrice<-GetPairPrices(Pair=PlotPair,t.start = min(PairPrice$TimeStamp),
                            t.stop = max(PairPrice$TimeStamp), Minutes = minutes)
  } else {
    UFOPrice<-GetPairPrices(Pair=PlotPair,t.start = min(PairPrice$TimeStamp),
                            t.stop = max(PairPrice$TimeStamp),Hours = hours, Minutes = NA)
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
