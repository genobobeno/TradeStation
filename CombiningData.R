ProcessPriceData<-function(df.price) {
  df.price<-CleanOpen(price = df.price)
  df.price<-LabelUpDown(price = df.price)
  df.price<-LegBaseCount(price=df.price)
  df.price<-BuildHistory(price=df.price)
  df.price<-AppendIndicators(price=df.price)
  AppendFeatures(price=df.price)
}

CombinePriceData<-function(list1,list2) {
  # list1<-ProcessPriceData(list1)
  # list2<-ProcessPriceData(list2)
  for (pair in names(list1)) {
    list1[[pair]]<-rbind(list1[[pair]],list2[[pair]])
  }
  list1
}

CombineModelData<-function(price) {
  if (class(price)=="list") {
    for (pair in names(price)) {
      if (pair==names(price)[1]) { 
        x.price<-price[[pair]]
        mult<-ifelse(x.price$Close[1]/10>1,100,10000)
        x.price$Move4<-mult*c(x.price$Close[5:nrow(x.price)]-x.price$Open[5:nrow(x.price)-3],NA,NA,NA,NA)
        x.price$Move3<-mult*c(x.price$Close[4:nrow(x.price)]-x.price$Open[4:nrow(x.price)-2],NA,NA,NA)
        x.price$Move2<-mult*c(x.price$Close[3:nrow(x.price)]-x.price$Open[3:nrow(x.price)-1],NA,NA)
        x.price$Move1<-mult*c(x.price$Close[2:nrow(x.price)]-x.price$Open[2:nrow(x.price)],NA)
      } else {
        mult<-ifelse(price[[pair]]$Close[1]/10>1,100,10000)
        price[[pair]]$Move4<-mult*c(price[[pair]]$Close[5:nrow(price[[pair]])]-price[[pair]]$Open[5:nrow(price[[pair]])-3],NA,NA,NA,NA)
        price[[pair]]$Move3<-mult*c(price[[pair]]$Close[4:nrow(price[[pair]])]-price[[pair]]$Open[4:nrow(price[[pair]])-2],NA,NA,NA)
        price[[pair]]$Move2<-mult*c(price[[pair]]$Close[3:nrow(price[[pair]])]-price[[pair]]$Open[3:nrow(price[[pair]])-1],NA,NA)
        price[[pair]]$Move1<-mult*c(price[[pair]]$Close[2:nrow(price[[pair]])]-price[[pair]]$Open[2:nrow(price[[pair]])],NA)
        x.price<-rbind(x.price,price[[pair]])
      }
    }
  } else {
    mult<-ifelse(price$Close[1]/10>1,100,10000)
    price$Move4<-mult*c(price$Close[5:nrow(price)]-price$Open[5:nrow(price)-3],NA,NA,NA,NA)
    price$Move3<-mult*c(price$Close[4:nrow(price)]-price$Open[4:nrow(price)-2],NA,NA,NA)
    price$Move2<-mult*c(price$Close[3:nrow(price)]-price$Open[3:nrow(price)-1],NA,NA)
    price$Move1<-mult*c(price$Close[2:nrow(price)]-price$Open[2:nrow(price)],NA)
    x.price<-price
  }
  x.price
}
