GetPair = function(s1,s2=NA,days=252) {
  # Call Yahoo, get data, parse data.frame, 
  # fill parent environment with col.[date,sec.A,sec.B]
  if (length(s1)==2) { s2 = s1[2]; s1 = s1[1] }
  if (length(s1)<3) {
    df1 = YahooRead(StockURL(s1));    df2 = YahooRead(StockURL(s2))    
    col.date <<- as.Date(df1[1:days,"Date"], "%Y-%m-%d")
    col.sec.A.adj.price <<- df1[1:days,"Adj.Close"]
    col.sec.B.adj.price <<- df2[1:days,"Adj.Close"]
    TS = eval(parse(text = paste0("data.frame(",s1,"=col.sec.A.adj.price,",s2,"=col.sec.B.adj.price)")))
  } else {
    for (i in 1:length(s1)) {
      df = yahoo.read(StockURL(s1[i]))
      if (i>1) {
        TS[,s1[i]] = df[1:days,'Adj.Close']
        if (i==2) col.sec.B.adj.price <<- df[1:days,"Adj.Close"]
      } else {
        eval(parse(text = paste0("TS = data.frame(",s1[i],"=df[1:days,'Adj.Close'])")))
        col.sec.A.adj.price <<- df[1:days,"Adj.Close"]
      } 
    }
  }
  col.date <<- TS$DATE <- as.Date(df1[1:days,"Date"], "%Y-%m-%d")
  TS = TS[nrow(TS):1,]
  return (TS)
}