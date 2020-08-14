
KChan <- function(ohlc, n_EMA = 10, n_ATR = 14, multiplier = 2) {
  library(quantmod)
  #ohlc<-OHLC(p.obj);n_EMA = 10;n_ATR = 14;multiplier = 2
  mid <- EMA(Cl(ohlc), n_EMA)    
  hi <- mid + multiplier * ATR(HLC = HLC(ohlc), n = n_ATR)[,2]
  lo <- mid - multiplier * ATR(HLC = HLC(ohlc), n = n_ATR)[,2]
  keltner <- cbind(lo, mid, hi)
  colnames(keltner) <- c("Kelt_lo", "Kelt_mid", "Kelt_hi")
  keltner
}




AppendFeatures<-function(price) {
  #cat("If you give this a {df,list}, it {won't,will} loop over pairs.")
  #price<-JW
  if (class(price)=="list") {
    for (pair in names(price)) {
      n<-nrow(price[[pair]])
      price[[pair]]$Delta<-(price[[pair]]$High-price[[pair]]$Low)/price[[pair]]$Close 
      price[[pair]]$HighWick<-price[[pair]]$LowWick<-NA
      price[[pair]]$HighWick[price[[pair]]$UpDown=="Up"]<-(price[[pair]]$High[price[[pair]]$UpDown=="Up"]-price[[pair]]$Close[price[[pair]]$UpDown=="Up"])/price[[pair]]$Close[price[[pair]]$UpDown=="Up"] 
      price[[pair]]$LowWick[price[[pair]]$UpDown=="Up"]<-(price[[pair]]$Open[price[[pair]]$UpDown=="Up"]-price[[pair]]$Low[price[[pair]]$UpDown=="Up"])/price[[pair]]$Close[price[[pair]]$UpDown=="Up"]
      price[[pair]]$HighWick[price[[pair]]$UpDown=="Down"]<-(price[[pair]]$High[price[[pair]]$UpDown=="Down"]-price[[pair]]$Open[price[[pair]]$UpDown=="Down"])/price[[pair]]$Close[price[[pair]]$UpDown=="Down"]
      price[[pair]]$LowWick[price[[pair]]$UpDown=="Down"]<-(price[[pair]]$Close[price[[pair]]$UpDown=="Down"]-price[[pair]]$Low[price[[pair]]$UpDown=="Down"])/price[[pair]]$Close[price[[pair]]$UpDown=="Down"]
      price[[pair]]$d8Vol<-c(rep(NA,8),price[[pair]]$TickVolume[9:n]/
                               colSums(sapply(1:(nrow(price[[pair]])-8),function(x) (price[[pair]]$TickVolume[1:8+x]))))
      price[[pair]]$emacross<-c(NA,abs(diff(price[[pair]]$ema8>price[[pair]]$ema13))==1)
      price[[pair]]$m8dema<-c(rep(NA,20),sapply(21:n,function(x) mean(price[[pair]]$dema[x-7+0:7])))
      price[[pair]]$m13dema<-c(rep(NA,25),sapply(26:n,function(x) mean(price[[pair]]$dema[x-12+0:12])))
    }
  } else if (class(price)=="data.frame") {
    n<-nrow(price)
    price$Delta<-(price$High-price$Low)/price$Close 
    price$HighWick<-price$LowWick<-NA
    price$HighWick[price$UpDown=="Up"]<-(price$High[price$UpDown=="Up"]-price$Close[price$UpDown=="Up"])/price$Close[price$UpDown=="Up"] 
    price$LowWick[price$UpDown=="Up"]<-(price$Open[price$UpDown=="Up"]-price$Low[price$UpDown=="Up"])/price$Close[price$UpDown=="Up"]
    price$HighWick[price$UpDown=="Down"]<-(price$High[price$UpDown=="Down"]-price$Open[price$UpDown=="Down"])/price$Close[price$UpDown=="Down"]
    price$LowWick[price$UpDown=="Down"]<-(price$Close[price$UpDown=="Down"]-price$Low[price$UpDown=="Down"])/price$Close[price$UpDown=="Down"]
    price$d8Vol<-c(rep(NA,8),price$TickVolume[9:n]/
                             colSums(sapply(1:(nrow(price)-8),function(x) (price$TickVolume[1:8+x]))))
    price$emacross<-c(NA,abs(diff(price$ema8>price$ema13))==1)
    price$m8dema<-c(rep(NA,20),sapply(21:n,function(x) mean(price$dema[x-7+0:7])))
    price$m13dema<-c(rep(NA,25),sapply(26:n,function(x) mean(price$dema[x-12+0:12])))
  }
  price
}


AppendIndicators<-function(price) {
  #cat("If you give this a {df,list}, it {won't,will} loop over pairs.")
  #price<-JW
  if (class(price)=="list") {
    for (pair in names(price)) {
      #print(pair); pair="EUR_USD"
      price[[pair]]$adx5<-ADX(cbind(price[[pair]]$High,price[[pair]]$Low,price[[pair]]$Close),n = 5)[,4]
      price[[pair]]$adx8<-ADX(cbind(price[[pair]]$High,price[[pair]]$Low,price[[pair]]$Close),n = 8)[,4]
      price[[pair]]$adx14<-ADX(cbind(price[[pair]]$High,price[[pair]]$Low,price[[pair]]$Close),n = 14)[,4]
      price[[pair]]$atr14<-ATR(cbind(price[[pair]]$High,price[[pair]]$Low,price[[pair]]$Close),n = 14)[,2]
      price[[pair]]$cci13<-CCI(cbind(price[[pair]]$High,price[[pair]]$Low,price[[pair]]$Close),n = 13)
      price[[pair]]$lr310<-SMA(price[[pair]]$Close,n = 3)-SMA(price[[pair]]$Close,n = 10)
      price[[pair]]$lr310ma<-SMA(price[[pair]]$lr310,n = 16)
      price[[pair]]$ema8<-EMA(price[[pair]]$Close,n = 8)
      price[[pair]]$ema13<-EMA(price[[pair]]$Close,n = 13)
      price[[pair]]$ema21<-EMA(price[[pair]]$Close,n = 21)
      #emaSD<-sd(ema8[!is.na(ema21)]-ema21[!is.na(ema21)])
      price[[pair]]$ema50<-EMA(price[[pair]]$Close,n = 50)
      price[[pair]]$rsi14<-RSI(price[[pair]]$Close,n = 14)
      price[[pair]]$ccrs<-(price[[pair]]$rsi14-50)*price[[pair]]$cci13
      price[[pair]]$speed<-((price[[pair]]$rsi14-50)+30*price[[pair]]$cci13/max(abs(price[[pair]]$cci13)))*price[[pair]]$adx5
      ##  derivatives
      price[[pair]]$dadx<-c(NA,diff(price[[pair]]$adx14))
      price[[pair]]$d2adx<-c(NA,NA,diff(price[[pair]]$adx14,lag=2))
      price[[pair]]$dema<-(price[[pair]]$ema8-price[[pair]]$ema13)/price[[pair]]$Close
      price[[pair]]$d2ema<-c(NA,NA,(diff(price[[pair]]$ema8)[-1]+diff(price[[pair]]$ema8,lag = 2))+
                               (diff(price[[pair]]$ema13)[-1]+diff(price[[pair]]$ema13,lag = 2)))/price[[pair]]$Close
      price[[pair]]$dema8<-c(NA,diff(price[[pair]]$ema8)/price[[pair]]$Close[-1])
      price[[pair]]$dema13<-c(NA,diff(price[[pair]]$ema13)/price[[pair]]$Close[-1])
      price[[pair]]$d3cci13<-c(NA,NA,NA,diff(price[[pair]]$cci13,lag=3))
      price[[pair]]$d2cci13<-c(NA,NA,diff(price[[pair]]$cci13,lag=2))
      price[[pair]]$d3rsi14<-c(NA,NA,NA,diff(price[[pair]]$rsi14,lag=3))
      price[[pair]]$d2rsi14<-c(NA,NA,diff(price[[pair]]$rsi14,lag=2))
      price[[pair]]$d2ccrs<-price[[pair]]$d2cci13*price[[pair]]$d2rsi14
      price[[pair]]$d3ccrs<-price[[pair]]$d3cci13*price[[pair]]$d3rsi14
      bbands<-BBands(cbind(price[[pair]]$High,price[[pair]]$Low,price[[pair]]$Close))
      price[[pair]]<-cbind(price[[pair]],bbands)
      colnames(price[[pair]])[ncol(price[[pair]])-3:0]<-paste0("BB",colnames(price[[pair]])[ncol(price[[pair]])-3:0])
      p.obj<-cbind(price[[pair]]$Open,price[[pair]]$High,price[[pair]]$Low,price[[pair]]$Close)
      colnames(p.obj)<-c("Open","High","Low","Close")
      kc<-KChan(OHLC(p.obj))
      price[[pair]]<-cbind(price[[pair]],kc)
      price[[pair]]$HighSqueeze<-ifelse(price[[pair]]$BBup<price[[pair]]$Kelt_hi,1,0)
      price[[pair]]$LowSqueeze<-ifelse(price[[pair]]$BBdn>price[[pair]]$Kelt_lo,1,0)
      price[[pair]]$TrendUp<-ifelse(price[[pair]]$adx14>30 & price[[pair]]$dema>0,1,0)
      price[[pair]]$TrendDown<-ifelse(price[[pair]]$adx14>30 & price[[pair]]$dema<0,1,0)
    }
  } else if (class(price)=="data.frame") {
    price$adx5<-ADX(cbind(price$High,price$Low,price$Close),n = 5)[,4]
    price$adx8<-ADX(cbind(price$High,price$Low,price$Close),n = 8)[,4]
    price$adx14<-ADX(cbind(price$High,price$Low,price$Close),n = 14)[,4]
    price$atr14<-ATR(cbind(price$High,price$Low,price$Close),n = 14)[,2]
    price$cci13<-CCI(cbind(price$High,price$Low,price$Close),n = 13)
    price$lr310<-SMA(price$Close,n = 3)-SMA(price$Close,n = 10)
    price$lr310ma<-SMA(price$lr310,n = 16)
    price$ema8<-EMA(price$Close,n = 8)
    price$ema13<-EMA(price$Close,n = 13)
    price$ema21<-EMA(price$Close,n = 21)
    #emaSD<-sd(ema8[!is.na(ema21)]-ema21[!is.na(ema21)])
    price$ema50<-EMA(price$Close,n = 50)
    price$rsi14<-RSI(price$Close,n = 14)
    price$ccrs<-(price$rsi14-50)*price$cci13
    price$speed<-((price[[pair]]$rsi14-50)+30*price$cci13/max(abs(price$cci13)))*price$adx5
    ##  derivatives
    price$dadx<-c(NA,diff(price$adx14))
    price$d2adx<-c(NA,NA,diff(price$adx14,lag=2))
    price$dema<-(price$ema8-price$ema13)/price$Close
    price$d2ema<-c(NA,NA,(diff(price$ema8)[-1]+diff(price$ema8,lag = 2))+
                             (diff(price$ema13)[-1]+diff(price$ema13,lag = 2)))/price$Close
    price$dema8<-c(NA,diff(price$ema8)/price$Close[-1])
    price$dema13<-c(NA,diff(price$ema13)/price$Close[-1])
    price$d3cci13<-c(NA,NA,NA,diff(price$cci13,lag=3))
    price$d2cci13<-c(NA,NA,diff(price$cci13,lag=2))
    price$d3rsi14<-c(NA,NA,NA,diff(price$rsi14,lag=3))
    price$d2rsi14<-c(NA,NA,diff(price$rsi14,lag=2))
    price$d2ccrs<-price$d2cci13*price$d2rsi14
    price$d3ccrs<-price$d3cci13*price$d3rsi14
    bbands<-BBands(cbind(price$High,price$Low,price$Close))
    price<-cbind(price,bbands)
    colnames(price)[ncol(price)-3:0]<-paste0("BB",colnames(price)[ncol(price)-3:0])
    p.obj<-cbind(price$Open,price$High,price$Low,price$Close)
    colnames(p.obj)<-c("Open","High","Low","Close")
    kc<-KChan(OHLC(p.obj))
    price<-cbind(price,kc)
    price$HighSqueeze<-ifelse(price$BBup<price$Kelt_hi,1,0)
    price$LowSqueeze<-ifelse(price$BBdn>price$Kelt_lo,1,0)
    price$TrendUp<-ifelse(price$adx14>30 & price$dema>0,1,0)
    price$TrendDown<-ifelse(price$adx14>30 & price$dema<0,1,0)
  }
  price
}

# par(mfrow=c(1,1))
# plot(JW$dema)  no movement ~ 0.00006 for more than 4 candles.
# plot(JW$d2ema)
# lines(JW$dema)
# StrengthCalc<-function(price) {
#   cat("If you give this a {df,list}, it {won't,will} loop over pairs.")
#   if (class(price)=="list") {
#     for (pair in names(price)) {
#       price[[pair]]$RecentHigh<-max(price[[pair]]$LegHigh,price[[pair]]$AnchorHigh)
#       price[[pair]]$RecentLow<-min(price[[pair]]$LegLow,price[[pair]]$AnchorLow)
#       #cumsum with a reset
#       #ave(df$a, cumsum(c(F, diff(df$a) < 0)), FUN=seq_along) - 1
#       price[[pair]]
#     }
#   } else if (class(price)=="data.frame") {
#   }
# }