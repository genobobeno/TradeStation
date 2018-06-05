SetTrade<-function(ticker,proximal,distal,target,account) {
  library(TTR)
  library(quantmod)
  ticker="AAPL"
  getSymbols(ticker, src = "yahoo",
             from = Sys.Date()-100, to = Sys.Date())
  eval(parse(text = paste0("Sym <-",ticker)))
  atr<-ATR(Sym[,c(2:4)])
  atr<-atr[nrow(atr),"atr"]
  distal<-ceiling(0.02*atr)+distal
  risk = abs(proximal-distal)
  reward = abs(proximal-target)/risk
  size<-floor(0.5*account/Sym[nrow(Sym),1])
  if (risk*size>0.02*account) size<-floor(0.02*account/risk)
  print("")
}

###  First Trade: HPQ: 22.15 short, stop 22.22, target 21.87, 4:1, 12/13