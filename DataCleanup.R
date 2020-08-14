
CleanOpen<-function(price) {
  #cat("If you give this a {df,list}, it {won't,will} loop over pairs.")
  if (class(price)=="list") {
    for (pair in names(price)) {
      #print(pair)
      price[[pair]]$Open<-as.numeric(price[[pair]]$Open)
      price[[pair]]$High<-as.numeric(price[[pair]]$High)
      price[[pair]]$Low<-as.numeric(price[[pair]]$Low)
      price[[pair]]$Close<-as.numeric(price[[pair]]$Close)
      for (j in 2:nrow(price[[pair]])) {
        if (price[[pair]]$Open[j]!=price[[pair]]$Close[j-1]) {
          price[[pair]]$Open[j]<-price[[pair]]$Close[j-1]
          if (price[[pair]]$Open[j]>price[[pair]]$High[j]) price[[pair]]$High[j]<-price[[pair]]$Open[j] 
          if (price[[pair]]$Open[j]<price[[pair]]$Low[j]) price[[pair]]$Low[j]<-price[[pair]]$Open[j]  
        }
      }
    }
  } else if (class(price)=="data.frame") {
    price$Open<-as.numeric(price$Open)
    price$High<-as.numeric(price$High)
    price$Low<-as.numeric(price$Low)
    price$Close<-as.numeric(price$Close)
    for (j in 2:nrow(price)) {
      if (price$Open[j]!=price$Close[j-1]) {
        price$Open[j]<-price$Close[j-1]
        if (price$Open[j]>price$High[j]) price$High[j]<-price$Open[j] 
        if (price$Open[j]<price$Low[j]) price$Low[j]<-price$Open[j]  
      }
    }
  } else {
    cat("I don't know what kind of data object you gave for price.")
    return(NULL)
  }
  price  
}

CleanPriceList<-function(price){
  #price<-Price
  #cat("If you give this a {df,list}, it {won't,will} loop over pairs.")
  if (class(price)=="list") {
    t.stamp<-c()
    for (pair in names(price)) { #unlist(lapply())
      t.stamp<-c(t.stamp,as.character(price[[pair]]$TimeStamp))
    }  
    t.table<-table(t.stamp)
    startcandle<-min(which(t.table==length(price)))
    if (startcandle>1) {
      for (pair in names(price)) {
        #print(pair)
        price[[pair]]<-price[[pair]][names(t.table)[startcandle-1]<as.character(price[[pair]]$TimeStamp),]
      }
      t.stamp<-unique(t.stamp[t.stamp>names(t.table)[startcandle-1]])
    } else {
      t.stamp<-unique(t.stamp)
    }
    for (pair in names(price)) {
      t.stamp<-t.stamp[t.stamp %in% as.character(price[[pair]]$TimeStamp)]
    }
    for (pair in names(price)) {
     # print(pair)
      if (sum(!as.character(price[[pair]]$TimeStamp) %in% t.stamp)>0) {
        rows_to_drop<-which(!as.character(price[[pair]]$TimeStamp) %in% t.stamp)
        j0<-(-1)
        for (j in rows_to_drop) {
          if (j-j0==1) {
            lookback<-lookback+1
          } else {
            lookback<-1
          }
          price[[pair]]$Close[j-lookback]<-price[[pair]]$Close[j]
          if (price[[pair]]$High[j]>price[[pair]]$High[j-lookback]) price[[pair]]$High[j-lookback]<-price[[pair]]$High[j]
          if (price[[pair]]$Low[j]<price[[pair]]$Low[j-lookback]) price[[pair]]$Low[j-lookback]<-price[[pair]]$Low[j]
          j0<-j
        }
        price[[pair]]<-price[[pair]][-rows_to_drop,]
      }
    }
  }
  price
}
