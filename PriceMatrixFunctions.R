
MakePriceArray<-function(price) {
  
  # Get min and max and add about 20% and make a data.frame for each price level.
  # 
  price.df = GetAllPrices(Hours = 4,LookBackHours = 2000)
  price<-price.df[["EUR_USD"]]
  price<-CleanOpen(price)
  price<-LabelUpDown(price=price,multiple.pairs = FALSE)
  price<-LegBaseCount(price=price)
  price<-BuildHistory(price=price)
  price<-AppendIndicators(price=price)
  price<-AppendFeatures(price=price)
  if (class(price)=="list") {
    for (pair in names(price)) {
      mult<-ifelse(floor(price[[pair]]$Low[1]/10)>1,0.001,0.00001)  
      arr<-seq(from=min(price[[pair]]$Low),to=max(price[[pair]]$High),by=mult)
      
    }
  } else if (class(price)=="data.frame") {
    mult<-ifelse(floor(price$Low[1]/10)>1,0.001,0.00001)  
    arr<-seq(from=min(price$Low),to=max(price$High),by=mult)
    if (length(diff(price$LegBase=="Leg")==0)>0) {
      i<-which(diff(price$LegBase=="Leg")==0)
    }
      
  } else {
    print("Not sure what kinda data you're sending.")
    return(NULL)
  }
  
  price$BaseBuildTop<-unlist(mapply(
    function(val, len1, len2) if (!is.na(val) & val == "Leg") rep(0, len1) else cumsum(price$TopWickATR[(len2-(len1-1)):(len2)]),
    rle(price$LegBase)$values, rle(price$LegBase)$lengths, cumsum(rle(price$LegBase)$lengths)))
  
  
}