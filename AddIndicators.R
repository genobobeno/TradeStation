
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

AppendIndicators<-function(price) {
  #cat("If you give this a {df,list}, it {won't,will} loop over pairs.")
  #price<-JW
  if (class(price)=="list") {
    for (pair in names(price)) {
      #print(pair); pair="EUR_USD"
      n<-nrow(price[[pair]])
      mult<-ifelse(price[[pair]]$Close[1]/10>1,100,10000)
      price[[pair]]$adx5<-ADX(cbind(price[[pair]]$High,price[[pair]]$Low,price[[pair]]$Close),n = 5)[,4]
      price[[pair]]$adx8<-ADX(cbind(price[[pair]]$High,price[[pair]]$Low,price[[pair]]$Close),n = 8)[,4]
      price[[pair]]$adx14<-ADX(cbind(price[[pair]]$High,price[[pair]]$Low,price[[pair]]$Close),n = 14)[,4]
      price[[pair]]$atr14<-ATR(cbind(price[[pair]]$High,price[[pair]]$Low,price[[pair]]$Close),n = 14)[,2]
      price[[pair]]$cci13<-CCI(cbind(price[[pair]]$High,price[[pair]]$Low,price[[pair]]$Close),n = 13)
      price[[pair]]$lr310<-mult*(SMA(price[[pair]]$Close,n = 3)-SMA(price[[pair]]$Close,n = 10))
      price[[pair]]$lr310ma<-SMA(price[[pair]]$lr310,n = 16)
      price[[pair]]$ema3<-EMA(price[[pair]]$Close,n = 3)
      price[[pair]]$ema5<-EMA(price[[pair]]$Close,n = 5)
      price[[pair]]$ema8<-EMA(price[[pair]]$Close,n = 8)
      price[[pair]]$ema13<-EMA(price[[pair]]$Close,n = 13)
      price[[pair]]$ema21<-EMA(price[[pair]]$Close,n = 21)
      #emaSD<-sd(ema8[!is.na(ema21)]-ema21[!is.na(ema21)])
      price[[pair]]$ema50<-EMA(price[[pair]]$Close,n = 50)
      price[[pair]]$rsi14<-RSI(price[[pair]]$Close,n = 14)
      price[[pair]]$SMAHigh<-SMA(price[[pair]]$High,n = 10)
      price[[pair]]$SMALow<-SMA(price[[pair]]$Low,n = 10)
      
      price[[pair]]$ccrs<-(price[[pair]]$rsi14-50)*price[[pair]]$cci13
      price[[pair]]$speed<-((price[[pair]]$rsi14-50)+30*price[[pair]]$cci13/max(abs(price[[pair]]$cci13),na.rm = T))*price[[pair]]$adx5
      ##  derivatives
      price[[pair]]$dadx<-c(NA,diff(price[[pair]]$adx14))
      price[[pair]]$d2adx<-c(NA,NA,diff(price[[pair]]$adx14,lag=2))
      price[[pair]]$dema<-mult*(price[[pair]]$ema8-price[[pair]]$ema13)
      price[[pair]]$d2ema<-mult*c(NA,NA,(diff(price[[pair]]$ema8)[-1]+diff(price[[pair]]$ema8,lag = 2))+
                               (diff(price[[pair]]$ema13)[-1]+diff(price[[pair]]$ema13,lag = 2)))
      price[[pair]]$dema8<-mult*c(NA,diff(price[[pair]]$ema8))
      price[[pair]]$dema13<-mult*c(NA,diff(price[[pair]]$ema13))
      price[[pair]]$dema21<-mult*c(NA,diff(price[[pair]]$ema21))
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
      price[[pair]]$BBKC<-(price[[pair]]$BBup-price[[pair]]$BBdn)/(price[[pair]]$Kelt_hi-price[[pair]]$Kelt_lo)
      price[[pair]]$HighSqueeze<-ifelse(price[[pair]]$BBup<price[[pair]]$Kelt_hi,3,0)
      price[[pair]]$LowSqueeze<-ifelse(price[[pair]]$BBdn>price[[pair]]$Kelt_lo,3,0)
      price[[pair]]$HighSqueeze<-c(NA,ifelse(price[[pair]]$HighSqueeze[1:(n-1)]==3 & price[[pair]]$HighSqueeze[2:n]==0,2,0))
      price[[pair]]$LowSqueeze<-c(NA,ifelse(price[[pair]]$LowSqueeze[1:(n-1)]==3 & price[[pair]]$LowSqueeze[2:n]==0,2,0))
      price[[pair]]$HighSqueeze<-c(NA,NA,ifelse(price[[pair]]$HighSqueeze[1:(n-2)]==2 & price[[pair]]$HighSqueeze[2:(n-1)]==0,1,0))
      price[[pair]]$LowSqueeze<-c(NA,NA,ifelse(price[[pair]]$LowSqueeze[1:(n-2)]==2 & price[[pair]]$LowSqueeze[2:(n-1)]==0,1,0))
      price[[pair]]$TrendUp<-factor(ifelse(price[[pair]]$adx14>30 & price[[pair]]$dema>0,1,0))
      price[[pair]]$TrendDown<-factor(ifelse(price[[pair]]$adx14>30 & price[[pair]]$dema<0,1,0))
    }
  } else if (class(price)=="data.frame") {
    n<-nrow(price)
    mult<-ifelse(price$Close[1]/10>1,100,10000)
    price$adx5<-ADX(cbind(price$High,price$Low,price$Close),n = 5)[,4]
    price$adx8<-ADX(cbind(price$High,price$Low,price$Close),n = 8)[,4]
    price$adx14<-ADX(cbind(price$High,price$Low,price$Close),n = 14)[,4]
    price$atr14<-ATR(cbind(price$High,price$Low,price$Close),n = 14)[,2]
    price$cci13<-CCI(cbind(price$High,price$Low,price$Close),n = 13)
    price$lr310<-mult*(SMA(price$Close,n = 3)-SMA(price$Close,n = 10))
    price$lr310ma<-SMA(price$lr310,n = 16)
    price$ema3<-EMA(price$Close,n = 3)
    price$ema5<-EMA(price$Close,n = 5)
    price$ema8<-EMA(price$Close,n = 8)
    price$ema13<-EMA(price$Close,n = 13)
    price$ema21<-EMA(price$Close,n = 21)
    #emaSD<-sd(ema8[!is.na(ema21)]-ema21[!is.na(ema21)])
    price$ema50<-EMA(price$Close,n = 50)
    price$rsi14<-RSI(price$Close,n = 14)
    price$SMAHigh<-SMA(price$High,n = 10)
    price$SMALow<-SMA(price$Low,n = 10)
    
    price$ccrs<-(price$rsi14-50)*price$cci13
    price$speed<-((price$rsi14-50)+30*price$cci13/max(abs(price$cci13),na.rm = T))*price$adx5
    ##  derivatives
    price$dadx<-c(NA,diff(price$adx14))
    price$d2adx<-c(NA,NA,diff(price$adx14,lag=2))
    price$dema<-mult*(price$ema8-price$ema13)
    price$d2ema<-mult*c(NA,NA,(diff(price$ema8)[-1]+diff(price$ema8,lag = 2))+
                             (diff(price$ema13)[-1]+diff(price$ema13,lag = 2)))
    price$dema8<-mult*c(NA,diff(price$ema8))
    price$dema13<-mult*c(NA,diff(price$ema13))
    price$dema21<-mult*c(NA,diff(price$ema21))
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
    price$BBKC<-(price$BBup-price$BBdn)/(price$Kelt_hi-price$Kelt_lo)
    price$HighSqueeze<-factor(ifelse(price$BBup<price$Kelt_hi,3,0))
    price$LowSqueeze<-factor(ifelse(price$BBdn>price$Kelt_lo,3,0))
    price$HighSqueeze<-c(NA,ifelse(price$HighSqueeze[1:(n-1)]==3 & price$HighSqueeze[2:n]==0,2,0))
    price$LowSqueeze<-c(NA,ifelse(price$LowSqueeze[1:(n-1)]==3 & price$LowSqueeze[2:n]==0,2,0))
    price$HighSqueeze<-c(NA,ifelse(price$HighSqueeze[1:(n-1)]==2 & price$HighSqueeze[2:n]==0,1,0))
    price$LowSqueeze<-c(NA,ifelse(price$LowSqueeze[1:(n-1)]==2 & price$LowSqueeze[2:n]==0,1,0))
    price$TrendUp<-factor(ifelse(price$adx14>30 & price$dema>0,1,0))
    price$TrendDown<-factor(ifelse(price$adx14>30 & price$dema<0,1,0))
  }
  price
}

AppendFeatures<-function(price) {
  #cat("If you give this a {df,list}, it {won't,will} loop over pairs.")
  #price<-JW
  if (class(price)=="list") {
    for (pair in names(price)) {
      mult<-ifelse(price[[pair]]$Close[1]/10>1,100,10000)
      n<-nrow(price[[pair]])

      price[[pair]]$emaMetricFast <- (mult*(price[[pair]]$ema5-price[[pair]]$ema8))^2 +
        (mult*(price[[pair]]$ema5-price[[pair]]$ema13))^2 +
        (mult*(price[[pair]]$ema13-price[[pair]]$ema8))^2
      price[[pair]]$demaMetricFast <- c(NA,diff(price[[pair]]$emaMetricFast))
      price[[pair]]$d2emaMetricFast <- c(NA,NA,diff(price[[pair]]$emaMetricFast,lag = 2))
      
      price[[pair]]$emaMetric <- (mult*(price[[pair]]$ema8-price[[pair]]$ema13))^2 +
        (mult*(price[[pair]]$ema8-price[[pair]]$ema21))^2 +
        (mult*(price[[pair]]$ema21-price[[pair]]$ema13))^2
      price[[pair]]$demaMetric <- c(NA,diff(price[[pair]]$emaMetric))
      price[[pair]]$d2emaMetric <- c(NA,NA,diff(price[[pair]]$emaMetric,lag = 2))
      
      price[[pair]]$emaRank <- (price[[pair]]$ema3>price[[pair]]$ema5) + 2*(price[[pair]]$ema3>price[[pair]]$ema8) +
        3*(price[[pair]]$ema3>price[[pair]]$ema13) + 4*(price[[pair]]$ema3>price[[pair]]$ema21) +
        (price[[pair]]$ema5>price[[pair]]$ema8) + 2*(price[[pair]]$ema5>price[[pair]]$ema13) + 
        3*(price[[pair]]$ema5>price[[pair]]$ema21) + (price[[pair]]$ema8>price[[pair]]$ema13) + 
        2*(price[[pair]]$ema8>price[[pair]]$ema21) + (price[[pair]]$ema13>price[[pair]]$ema21)

      price[[pair]]$STO<-stoch(cbind(price[[pair]]$High,price[[pair]]$Low,price[[pair]]$Close),nFastK = 5,nFastD = 3,nSlowD = 3)[,1]-0.5
      price[[pair]]$WIL<-WPR(cbind(price[[pair]]$High,price[[pair]]$Low,price[[pair]]$Close),n = 5)-0.5
      
      DChan<-DonchianChannel(cbind(price[[pair]]$High,price[[pair]]$Low),n = 5)
      price[[pair]]<-cbind(price[[pair]],DChan)
      colnames(price[[pair]])[ncol(price[[pair]])-2:0]<-c("DChanHigh5","DChanMid5","DChanLow5")
      price[[pair]]$DChanRange5 <- mult*(price[[pair]]$DChanHigh5 - price[[pair]]$DChanLow5) 
      price[[pair]]$DChanShift5 <- mult*(price[[pair]]$Close - price[[pair]]$DChanMid5) 
      DChan<-DonchianChannel(cbind(price[[pair]]$High,price[[pair]]$Low),n = 13)
      price[[pair]]<-cbind(price[[pair]],DChan)
      colnames(price[[pair]])[ncol(price[[pair]])-2:0]<-c("DChanHigh13","DChanMid13","DChanLow13")
      price[[pair]]$DChanRange13 <- mult*(price[[pair]]$DChanHigh13 - price[[pair]]$DChanLow13) 
      price[[pair]]$DChanShift13 <- mult*(price[[pair]]$Close - price[[pair]]$DChanMid13) 
      
      price[[pair]]$dDChanHigh<-mult*(price[[pair]]$DChanHigh5 - price[[pair]]$DChanHigh13)
      price[[pair]]$dDChanMid<-mult*(price[[pair]]$DChanMid5 - price[[pair]]$DChanMid13)
      price[[pair]]$dDChanLow<-mult*(price[[pair]]$DChanLow5 - price[[pair]]$DChanLow13)
      
      price[[pair]]$DChanMove5 <- c(NA,ifelse((diff(price[[pair]]$DChanHigh5)>0 & abs(diff(price[[pair]]$DChanHigh5)) > abs(diff(price[[pair]]$DChanLow5))),
                                           "UpMoving",
                                           ifelse((diff(price[[pair]]$DChanLow5)<0 & abs(diff(price[[pair]]$DChanHigh5)) < abs(diff(price[[pair]]$DChanLow5))),
                                                  "DownMoving",
                                                  ifelse((diff(price[[pair]]$DChanHigh5)<0 & abs(diff(price[[pair]]$DChanHigh5)) > abs(diff(price[[pair]]$DChanLow5))),
                                                         "UpConsolidation",
                                                         ifelse((diff(price[[pair]]$DChanLow5)>0 & abs(diff(price[[pair]]$DChanHigh5)) < abs(diff(price[[pair]]$DChanLow5))),
                                                                "DownConsolidation","NoMove")))))

      price[[pair]]$DChanMetric5 <- c(NA,log((1+abs(mult*diff(price[[pair]]$DChanHigh5)))/(1+abs(mult*diff(price[[pair]]$DChanLow5)))))
      
      price[[pair]]$CStoch<-sign(price[[pair]]$DChanMid5-price[[pair]]$Close)*100*price[[pair]]$STO*price[[pair]]$WIL

      price[[pair]]$emaXUp<-factor(c(NA,(diff(price[[pair]]$ema8>price[[pair]]$ema13)==1)+0))
      price[[pair]]$emaXDown<-factor(c(NA,(diff(price[[pair]]$ema8<price[[pair]]$ema13)==1)+0))
      price[[pair]]$emaXUp1<-factor(c(NA,NA,(price[[pair]]$emaXUp[2:(n-1)]==1 & price[[pair]]$ema8[3:n]>price[[pair]]$ema13[3:n])+0))
      price[[pair]]$emaXDown1<-factor(c(NA,NA,(price[[pair]]$emaXDown[2:(n-1)]==1 & price[[pair]]$ema8[3:n]<price[[pair]]$ema13[3:n])+0))
      price[[pair]]$HighWick<-price[[pair]]$LowWick<-price[[pair]]$TopWickATR<-price[[pair]]$BottomWickATR<-NA
      price[[pair]]$HighWick[price[[pair]]$UpDown=="Up"]<-(price[[pair]]$High[price[[pair]]$UpDown=="Up"]-price[[pair]]$Close[price[[pair]]$UpDown=="Up"])/price[[pair]]$Close[price[[pair]]$UpDown=="Up"] 
      price[[pair]]$LowWick[price[[pair]]$UpDown=="Up"]<-(price[[pair]]$Open[price[[pair]]$UpDown=="Up"]-price[[pair]]$Low[price[[pair]]$UpDown=="Up"])/price[[pair]]$Close[price[[pair]]$UpDown=="Up"]
      price[[pair]]$HighWick[price[[pair]]$UpDown=="Down"]<-(price[[pair]]$High[price[[pair]]$UpDown=="Down"]-price[[pair]]$Open[price[[pair]]$UpDown=="Down"])/price[[pair]]$Close[price[[pair]]$UpDown=="Down"]
      price[[pair]]$LowWick[price[[pair]]$UpDown=="Down"]<-(price[[pair]]$Close[price[[pair]]$UpDown=="Down"]-price[[pair]]$Low[price[[pair]]$UpDown=="Down"])/price[[pair]]$Close[price[[pair]]$UpDown=="Down"]
      price[[pair]]$TopWickATR[price[[pair]]$UpDown=="Up"]<-(price[[pair]]$High[price[[pair]]$UpDown=="Up"]-price[[pair]]$Close[price[[pair]]$UpDown=="Up"])/price[[pair]]$atr14[price[[pair]]$UpDown=="Up"]
      price[[pair]]$BottomWickATR[price[[pair]]$UpDown=="Up"]<-(price[[pair]]$Open[price[[pair]]$UpDown=="Up"]-price[[pair]]$Low[price[[pair]]$UpDown=="Up"])/price[[pair]]$atr14[price[[pair]]$UpDown=="Up"]
      price[[pair]]$TopWickATR[price[[pair]]$UpDown=="Down"]<-(price[[pair]]$High[price[[pair]]$UpDown=="Down"]-price[[pair]]$Open[price[[pair]]$UpDown=="Down"])/price[[pair]]$atr14[price[[pair]]$UpDown=="Down"]
      price[[pair]]$BottomWickATR[price[[pair]]$UpDown=="Down"]<-(price[[pair]]$Close[price[[pair]]$UpDown=="Down"]-price[[pair]]$Low[price[[pair]]$UpDown=="Down"])/price[[pair]]$atr14[price[[pair]]$UpDown=="Down"]
      
      price[[pair]]$BaseBuildTop<-unlist(mapply(
        function(val, len1, len2) if (!is.na(val) & val == "Leg") rep(0, len1) else cumsum(price[[pair]]$TopWickATR[(len2-(len1-1)):(len2)]),
        rle(price[[pair]]$LegBase)$values, rle(price[[pair]]$LegBase)$lengths, cumsum(rle(price[[pair]]$LegBase)$lengths)))
      
      price[[pair]]$BaseBuildBottom<-unlist(mapply(
        function(val, len1, len2) if (!is.na(val) & val == "Leg") rep(0, len1) else cumsum(price[[pair]]$BottomWickATR[(len2-(len1-1)):(len2)]),
        rle(price[[pair]]$LegBase)$values, rle(price[[pair]]$LegBase)$lengths, cumsum(rle(price[[pair]]$LegBase)$lengths)))
      
      price[[pair]]$SMAHigh<-SMA(price[[pair]]$High,n = 10)
      price[[pair]]$SMALow<-SMA(price[[pair]]$Low,n = 10)
      price[[pair]]$SSLXUp<-factor(c(NA,diff(price[[pair]]$Close>price[[pair]]$SMAHigh)==1))
      price[[pair]]$SSLXDown<-factor(c(NA,diff(price[[pair]]$Close<price[[pair]]$SMALow)==1))
      
      price[[pair]]$d8Vol<-c(rep(NA,8),price[[pair]]$TickVolume[9:n]/
                               colSums(sapply(1:(nrow(price[[pair]])-8),function(x) (price[[pair]]$TickVolume[1:8+x]))))
      price[[pair]]$d4Vol<-c(rep(NA,4),price[[pair]]$TickVolume[5:n]/
                               colSums(sapply(1:(nrow(price[[pair]])-4),function(x) (price[[pair]]$TickVolume[1:4+x]))))
      price[[pair]]$PreMove1<-mult*(price[[pair]]$Close-price[[pair]]$Open)
      price[[pair]]$PreMove2<-mult*(c(rep(NA,1),price[[pair]]$Close[2:n]-price[[pair]]$Open[1:(n-1)])) 
      price[[pair]]$PreMove3<-mult*(c(rep(NA,2),price[[pair]]$Close[3:n]-price[[pair]]$Open[1:(n-2)])) 
      price[[pair]]$PreMove4<-mult*(c(rep(NA,3),price[[pair]]$Close[4:n]-price[[pair]]$Open[1:(n-3)])) 
      price[[pair]]$PreMove5<-mult*(c(rep(NA,4),price[[pair]]$Close[5:n]-price[[pair]]$Open[1:(n-4)])) 
      price[[pair]]$ad8Vol<-c(rep(NA,8),price[[pair]]$TickVolume[9:n]/
                                colSums(sapply(1:(nrow(price[[pair]])-8),function(x) (price[[pair]]$TickVolume[0:7+x]))))
      price[[pair]]$ad4Vol<-c(rep(NA,4),price[[pair]]$TickVolume[5:n]/
                                colSums(sapply(1:(nrow(price[[pair]])-4),function(x) (price[[pair]]$TickVolume[0:3+x]))))
      price[[pair]]$ratioD4Vol<-c(rep(NA,4),diff(price[[pair]]$TickVolume[4:n]) +
                                    colMeans(sapply(1:(nrow(price[[pair]])-4),function(x) (diff(price[[pair]]$TickVolume[0:3+x]))))) # UpDown interaction
      price[[pair]]$m8dema<-c(rep(NA,20),sapply(21:n,function(x) mean(price[[pair]]$dema[x-7+0:7])))
      price[[pair]]$m13dema<-c(rep(NA,25),sapply(26:n,function(x) mean(price[[pair]]$dema[x-12+0:12])))
      price[[pair]]$Range<-mult*(price[[pair]]$High-price[[pair]]$Low)
      price[[pair]]$BuySellCandle<-ifelse(price[[pair]]$Close>price[[pair]]$Low+0.8*(price[[pair]]$High-price[[pair]]$Low),"Buy",
                                          ifelse(price[[pair]]$Close<price[[pair]]$Low+0.2*(price[[pair]]$High-price[[pair]]$Low),"Sell","Neutral"))
      price[[pair]]$sdRange<-c(rep(NA,24),sapply(1:(n-24),function(x) sd(price[[pair]]$Range[x+0:24])))
      price[[pair]]$Engulfing<-c(NA,ifelse(price[[pair]]$Low[2:n]<price[[pair]]$Low[1:(n-1)] & price[[pair]]$High[2:n]>price[[pair]]$High[1:(n-1)],1,0))
    }
  } else if (class(price)=="data.frame") {
    mult<-ifelse(price$Close[1]/10>1,100,10000)
    n<-nrow(price)
    
    price$emaMetricFast <- (mult*(price$ema5-price$ema8))^2 +
      (mult*(price$ema5-price$ema13))^2 +
      (mult*(price$ema13-price$ema8))^2
    price$demaMetricFast <- c(NA,diff(price$emaMetricFast))
    price$d2emaMetricFast <- c(NA,NA,diff(price$emaMetricFast,lag = 2))
    
    price$emaMetric <- (mult*(price$ema8-price$ema13))^2 +
      (mult*(price$ema8-price$ema21))^2 +
      (mult*(price$ema21-price$ema13))^2
    price$demaMetric <- c(NA,diff(price$emaMetric))
    price$d2emaMetric <- c(NA,NA,diff(price$emaMetric,lag = 2))
    
    price$emaRank <- (price$ema3>price$ema5) + 2*(price$ema3>price$ema8) +
      3*(price$ema3>price$ema13) + 4*(price$ema3>price$ema21) +
      (price$ema5>price$ema8) + 2*(price$ema5>price$ema13) + 
      3*(price$ema5>price$ema21) + (price$ema8>price$ema13) + 
      2*(price$ema8>price$ema21) + (price$ema13>price$ema21)
    
    price$STO<-stoch(cbind(price$High,price$Low,price$Close),nFastK = 5,nFastD = 3,nSlowD = 3)[,1]-0.5
    price$WIL<-WPR(cbind(price$High,price$Low,price$Close),n = 5)-0.5
    
    DChan<-DonchianChannel(cbind(price$High,price$Low),n = 5)
    price<-cbind(price,DChan)
    colnames(price)[ncol(price)-2:0]<-c("DChanHigh5","DChanMid5","DChanLow5")
    price$DChanRange5 <- mult*(price$DChanHigh5 - price$DChanLow5) 
    price$DChanShift5 <- mult*(price$Close - price$DChanMid5) 
    DChan<-DonchianChannel(cbind(price$High,price$Low),n = 13)
    price<-cbind(price,DChan)
    colnames(price)[ncol(price)-2:0]<-c("DChanHigh13","DChanMid13","DChanLow13")
    price$DChanRange13 <- mult*(price$DChanHigh13 - price$DChanLow13) 
    price$DChanShift13 <- mult*(price$Close - price$DChanMid13) 
    
    price$dDChanHigh<-mult*(price$DChanHigh5 - price$DChanHigh13)
    price$dDChanMid<-mult*(price$DChanMid5 - price$DChanMid13)
    price$dDChanLow<-mult*(price$DChanLow5 - price$DChanLow13)
    
    price$DChanMove5 <- c(NA,ifelse((diff(price$DChanHigh5)>0 & abs(diff(price$DChanHigh5)) > abs(diff(price$DChanLow5))),
                                            "UpMoving",
                                            ifelse((diff(price$DChanLow5)<0 & abs(diff(price$DChanHigh5)) < abs(diff(price$DChanLow5))),
                                                   "DownMoving",
                                                   ifelse((diff(price$DChanHigh5)<0 & abs(diff(price$DChanHigh5)) > abs(diff(price$DChanLow5))),
                                                          "UpConsolidation",
                                                          ifelse((diff(price$DChanLow5)>0 & abs(diff(price$DChanHigh5)) < abs(diff(price$DChanLow5))),
                                                                 "DownConsolidation","NoMove")))))
    
    price$DChanMetric5 <- c(NA,log((1+abs(mult*diff(price$DChanHigh5)))/(1+abs(mult*diff(price$DChanLow5)))))
    
    price$CStoch<-sign(price$DChanMid5-price$Close)*100*price$STO*price$WIL
    
    price$emaXUp<-factor(c(NA,(diff(price$ema8>price$ema13)==1)+0))
    price$emaXDown<-factor(c(NA,(diff(price$ema8<price$ema13)==1)+0))
    price$emaXUp1<-factor(c(NA,NA,(price$emaXUp[2:(n-1)]==1 & price$ema8[3:n]>price$ema13[3:n])+0))
    price$emaXDown1<-factor(c(NA,NA,(price$emaXDown[2:(n-1)]==1 & price$ema8[3:n]<price$ema13[3:n])+0))
    price$HighWick<-price$LowWick<-price$TopWickATR<-price$BottomWickATR<-NA
    price$HighWick[price$UpDown=="Up"]<-(price$High[price$UpDown=="Up"]-price$Close[price$UpDown=="Up"])/price$Close[price$UpDown=="Up"] 
    price$LowWick[price$UpDown=="Up"]<-(price$Open[price$UpDown=="Up"]-price$Low[price$UpDown=="Up"])/price$Close[price$UpDown=="Up"]
    price$HighWick[price$UpDown=="Down"]<-(price$High[price$UpDown=="Down"]-price$Open[price$UpDown=="Down"])/price$Close[price$UpDown=="Down"]
    price$LowWick[price$UpDown=="Down"]<-(price$Close[price$UpDown=="Down"]-price$Low[price$UpDown=="Down"])/price$Close[price$UpDown=="Down"]
    price$TopWickATR[price$UpDown=="Up"]<-(price$High[price$UpDown=="Up"]-price$Close[price$UpDown=="Up"])/price$atr14[price$UpDown=="Up"]
    price$BottomWickATR[price$UpDown=="Up"]<-(price$Open[price$UpDown=="Up"]-price$Low[price$UpDown=="Up"])/price$atr14[price$UpDown=="Up"]
    price$TopWickATR[price$UpDown=="Down"]<-(price$High[price$UpDown=="Down"]-price$Open[price$UpDown=="Down"])/price$atr14[price$UpDown=="Down"]
    price$BottomWickATR[price$UpDown=="Down"]<-(price$Close[price$UpDown=="Down"]-price$Low[price$UpDown=="Down"])/price$atr14[price$UpDown=="Down"]
    
    price$BaseBuildTop<-unlist(mapply(
      function(val, len1, len2) if (!is.na(val) & val == "Leg") rep(0, len1) else cumsum(price$TopWickATR[(len2-(len1-1)):(len2)]),
      rle(price$LegBase)$values, rle(price$LegBase)$lengths, cumsum(rle(price$LegBase)$lengths)))
    
    price$BaseBuildBottom<-unlist(mapply(
      function(val, len1, len2) if (!is.na(val) & val == "Leg") rep(0, len1) else cumsum(price$BottomWickATR[(len2-(len1-1)):(len2)]),
      rle(price$LegBase)$values, rle(price$LegBase)$lengths, cumsum(rle(price$LegBase)$lengths)))
    
    price$SSLXUp<-factor(c(NA,diff(price$Close>price$SMAHigh)==1))
    price$SSLXDown<-factor(c(NA,diff(price$Close<price$SMALow)==1))
    
    price$d8Vol<-c(rep(NA,8),price$TickVolume[9:n]/
                     colSums(sapply(1:(nrow(price)-8),function(x) (price$TickVolume[1:8+x]))))
    price$d4Vol<-c(rep(NA,4),price$TickVolume[5:n]/
                     colSums(sapply(1:(nrow(price)-4),function(x) (price$TickVolume[1:4+x]))))
    price$PreMove1<-mult*(price$Close-price$Open) 
    price$PreMove2<-mult*(c(rep(NA,1),price$Close[2:n]-price$Open[1:(n-1)]))
    price$PreMove3<-mult*(c(rep(NA,2),price$Close[3:n]-price$Open[1:(n-2)])) 
    price$PreMove4<-mult*(c(rep(NA,3),price$Close[4:n]-price$Open[1:(n-3)])) 
    price$PreMove5<-mult*(c(rep(NA,4),price$Close[5:n]-price$Open[1:(n-4)])) 
    price$ad8Vol<-c(rep(NA,8),price$TickVolume[9:n]/
                      colSums(sapply(1:(nrow(price)-8),function(x) (price$TickVolume[0:7+x]))))
    price$ad4Vol<-c(rep(NA,4),price$TickVolume[5:n]/
                      colSums(sapply(1:(nrow(price)-4),function(x) (price$TickVolume[0:3+x]))))
    price$ratioD4Vol<-c(rep(NA,4),diff(price$TickVolume[4:n]) +
                          colMeans(sapply(1:(nrow(price)-4),function(x) (diff(price$TickVolume[0:3+x]))))) # UpDown interaction
    price$m8dema<-c(rep(NA,20),sapply(21:n,function(x) mean(price$dema[x-7+0:7])))
    price$m13dema<-c(rep(NA,25),sapply(26:n,function(x) mean(price$dema[x-12+0:12])))
    price$Range<-mult*(price$High-price$Low)
    price$BuySellCandle<-ifelse(price$Close>price$Low+0.8*(price$High-price$Low),"Buy",
                                        ifelse(price$Close<price$Low+0.2*(price$High-price$Low),"Sell","Neutral"))
    price$sdRange<-c(rep(NA,24),sapply(1:(n-24),function(x) sd(price$Range[x+0:24])))
    price$Engulfing<-c(NA,ifelse(price$Low[2:n]<price$Low[1:(n-1)] & price$High[2:n]>price$High[1:(n-1)],1,0))
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