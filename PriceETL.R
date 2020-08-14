RBindPriceData<-function(curr.price,prev.price) {
  #cat("If you give this a {df,list}, it {won't,will} loop over pairs.")
  if (class(curr.price)=="list") {
    for (pair in names(curr.price)) {
      #print(pair)
      curr.price[[pair]]<-rbind(prev.price[[pair]],curr.price[[pair]])
      curr.price[[pair]][!duplicated(curr.price[[pair]]$TimeStamp),]
    }
  } else if (class(price)=="data.frame") {
    curr.price<-rbind(prev.price,curr.price)
    curr.price[!duplicated(curr.price$TimeStamp),]
  }
  curr.price
}


