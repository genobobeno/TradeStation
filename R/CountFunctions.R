UpDown<-function(v,lim) {
  if (class(v)=="character") return(2*(v=="Up")+(-1))
  if (class(v)=="numeric") return(2*(v>lim)+(-1))
}

LabelUpDown<-function(price,multiple.pairs=TRUE) {
  #cat("If you give this a {df,list}, it {won't,will} loop over pairs.")
  if (class(price)=="list") {
    # lapply(price,function(x) (x$UpDown<-ifelse(x$Close>x$Open,"Up",
    #                                 ifelse(x$Close<x$Open,"Down","Neutral"))))
    for (pair in names(price)) {
      price[[pair]]$UpDown<-ifelse(price[[pair]]$Close>price[[pair]]$Open,"Up",
                                   ifelse(price[[pair]]$Close<price[[pair]]$Open,"Down","Neutral"))
      # Now kill the Neutrals
      if (price[[pair]]$UpDown[1]=="Neutral") price[[pair]]$UpDown[1]<-"Up"
      for (i in 2:nrow(price[[pair]])) {
        if (price[[pair]]$UpDown[i]=="Neutral" & price[[pair]]$UpDown[i-1]=="Up") price[[pair]]$UpDown[i]<-"Up"
        if (price[[pair]]$UpDown[i]=="Neutral" & price[[pair]]$UpDown[i-1]=="Down") price[[pair]]$UpDown[i]<-"Down"
      }
      
      # Squashing Colors & TrendStates
      price[[pair]]$ActionHigh<-price[[pair]]$AnchorHigh<-price[[pair]]$LegHigh<-NA
      price[[pair]]$ActionLow<-price[[pair]]$AnchorLow<-price[[pair]]$LegLow<-NA
      price[[pair]]$ActionHigh[1]<-price[[pair]]$AnchorHigh[1]<-price[[pair]]$LegHigh[1]<-max(price[[pair]][1,c("Open","Close")])
      price[[pair]]$ActionLow[1] <-price[[pair]]$AnchorLow[1]<-price[[pair]]$LegLow[1]<-min(price[[pair]][1,c("Open","Close")])
      price[[pair]]$TrendState<-price[[pair]]$TrendSide<-price[[pair]]$Direction<-price[[pair]]$Confirmed<-NA
      price[[pair]]$Setup<-price[[pair]]$AB<-FALSE
      for (i in 2:nrow(price[[pair]])) {
        if (price[[pair]]$UpDown[i-1]=="Up" & price[[pair]]$UpDown[i]=="Up") {
          price[[pair]]$ActionHigh[i]<-price[[pair]]$Close[i]
          price[[pair]]$ActionLow[i] <-price[[pair]]$ActionLow[i-1]
          price[[pair]]$AnchorHigh[i]<-price[[pair]]$AnchorHigh[i-1] # Down
          price[[pair]]$AnchorLow[i] <-price[[pair]]$AnchorLow[i-1]
          price[[pair]]$LegHigh[i]<-price[[pair]]$LegHigh[i-1] # Up
          price[[pair]]$LegLow[i] <-price[[pair]]$LegLow[i-1]
          if ((price[[pair]]$AnchorHigh[i]-price[[pair]]$AnchorLow[i]) <
              (price[[pair]]$LegHigh[i]-price[[pair]]$LegLow[i])) {
            price[[pair]]$TrendSide[i]<-"Sideways"
            price[[pair]]$Direction[i]<-"Up"
            if (price[[pair]]$ActionHigh[i]>price[[pair]]$AnchorHigh[i]) {
              price[[pair]]$Confirmed[i]<-TRUE
            } else {
              price[[pair]]$Confirmed[i]<-FALSE
            } 
            price[[pair]]$TrendState[i]<-paste0("SBU",ifelse(price[[pair]]$Confirmed[i],"C",""))
          } else {
            price[[pair]]$TrendSide[i]<-"Trend"
            ClosestLow<-max(which(price[[pair]]$AnchorLow[i]>price[[pair]]$LegLow[1:i]))
            ClosestHigh<-max(which(price[[pair]]$AnchorHigh[i]<price[[pair]]$LegHigh[1:i]))
            if (is.infinite(ClosestHigh)&is.infinite(ClosestLow)) {
              price[[pair]]$Direction[i]<-NA
              price[[pair]]$Setup[i]<-NA
              price[[pair]]$AB[i]<-ifelse(price[[pair]]$ActionHigh[i]<price[[pair]]$AnchorHigh[i],FALSE,TRUE)
              price[[pair]]$TrendState[i]<-NA
            } else if (ClosestHigh>ClosestLow) {
              price[[pair]]$Direction[i]<-"Down"
              price[[pair]]$Setup[i]<-TRUE
              price[[pair]]$AB[i]<-ifelse(price[[pair]]$ActionHigh[i]<price[[pair]]$AnchorHigh[i],FALSE,TRUE)
              price[[pair]]$TrendState[i]<-paste0("DTS",ifelse(price[[pair]]$AB[i],"AB",""))
              if (price[[pair]]$TrendState[i]=="DTSAB") price[[pair]]$Direction[i]<-"Up"
            } else {
              price[[pair]]$Direction[i]<-"Up"
              price[[pair]]$Setup[i]<-FALSE
              price[[pair]]$AB[i]<-FALSE
              price[[pair]]$TrendState[i]<-"UTNS"
            }
          }
        } else if (price[[pair]]$UpDown[i-1]=="Down" & price[[pair]]$UpDown[i]=="Down") {
          price[[pair]]$ActionHigh[i]<-price[[pair]]$ActionHigh[i-1]
          price[[pair]]$ActionLow[i] <-price[[pair]]$Close[i]
          price[[pair]]$AnchorHigh[i]<-price[[pair]]$AnchorHigh[i-1]
          price[[pair]]$AnchorLow[i] <-price[[pair]]$AnchorLow[i-1]
          price[[pair]]$LegHigh[i]<-price[[pair]]$LegHigh[i-1]
          price[[pair]]$LegLow[i] <-price[[pair]]$LegLow[i-1]
          if ((price[[pair]]$AnchorHigh[i]-price[[pair]]$AnchorLow[i]) <
              (price[[pair]]$LegHigh[i]-price[[pair]]$LegLow[i])) {
            price[[pair]]$TrendSide[i]<-"Sideways"
            price[[pair]]$Direction[i]<-"Down"
            if (price[[pair]]$ActionLow[i]<price[[pair]]$AnchorLow[i]) {
              price[[pair]]$Confirmed[i]<-TRUE
            } else {
              price[[pair]]$Confirmed[i]<-FALSE
            } 
            price[[pair]]$TrendState[i]<-paste0("SBD",ifelse(price[[pair]]$Confirmed[i],"C",""))
          } else {
            price[[pair]]$TrendSide[i]<-"Trend"
            ClosestLow<-max(which(price[[pair]]$AnchorLow[i]>price[[pair]]$LegLow[1:i]))
            ClosestHigh<-max(which(price[[pair]]$AnchorHigh[i]<price[[pair]]$LegHigh[1:i]))
            if (is.infinite(ClosestHigh)&is.infinite(ClosestLow)) {
              price[[pair]]$Direction[i]<-NA
              price[[pair]]$Setup[i]<-NA
              price[[pair]]$AB[i]<-ifelse(price[[pair]]$ActionHigh[i]<price[[pair]]$AnchorHigh[i],FALSE,TRUE)
              price[[pair]]$TrendState[i]<-NA
            } else if (ClosestHigh<ClosestLow) {
              price[[pair]]$Direction[i]<-"Up"
              price[[pair]]$Setup[i]<-TRUE
              price[[pair]]$AB[i]<-ifelse(price[[pair]]$ActionLow[i]>price[[pair]]$AnchorLow[i],FALSE,TRUE)
              price[[pair]]$TrendState[i]<-paste0("UTS",ifelse(price[[pair]]$AB[i],"AB",""))
              if (price[[pair]]$TrendState[i]=="UTSAB") price[[pair]]$Direction[i]<-"Down"
            } else {
              price[[pair]]$Direction[i]<-"Down"
              price[[pair]]$Setup[i]<-FALSE
              price[[pair]]$AB[i]<-FALSE
              price[[pair]]$TrendState[i]<-"DTNS"
            }
          }
        } else if (price[[pair]]$UpDown[i-1]=="Up" & price[[pair]]$UpDown[i]=="Down") {
          price[[pair]]$ActionHigh[i]<-price[[pair]]$Open[i]
          price[[pair]]$ActionLow[i] <-price[[pair]]$Close[i]
          price[[pair]]$AnchorHigh[i]<-price[[pair]]$ActionHigh[i-1]
          price[[pair]]$AnchorLow[i] <-price[[pair]]$ActionLow[i-1]
          price[[pair]]$LegHigh[i]<-price[[pair]]$AnchorHigh[i-1]
          price[[pair]]$LegLow[i] <-price[[pair]]$AnchorLow[i-1]
          if ((price[[pair]]$AnchorHigh[i]-price[[pair]]$AnchorLow[i]) <
              (price[[pair]]$LegHigh[i]-price[[pair]]$LegLow[i])) {
            price[[pair]]$TrendSide[i]<-"Sideways"
            price[[pair]]$Direction[i]<-"Down"
            if (price[[pair]]$ActionLow[i]<price[[pair]]$AnchorLow[i]) {
              price[[pair]]$Confirmed[i]<-TRUE
            } else {
              price[[pair]]$Confirmed[i]<-FALSE
            } 
            price[[pair]]$TrendState[i]<-paste0("SBD",ifelse(price[[pair]]$Confirmed[i],"C",""))
          } else {
            price[[pair]]$TrendSide[i]<-"Trend"
            ClosestLow<-max(which(price[[pair]]$AnchorLow[i]>price[[pair]]$LegLow[1:i]))
            ClosestHigh<-max(which(price[[pair]]$AnchorHigh[i]<price[[pair]]$LegHigh[1:i]))
            if (is.infinite(ClosestHigh)&is.infinite(ClosestLow)) {
              price[[pair]]$Direction[i]<-NA
              price[[pair]]$Setup[i]<-NA
              price[[pair]]$AB[i]<-ifelse(price[[pair]]$ActionHigh[i]<price[[pair]]$AnchorHigh[i],FALSE,TRUE)
              price[[pair]]$TrendState[i]<-NA
            } else if (ClosestHigh<ClosestLow) {
              price[[pair]]$Direction[i]<-"Up"
              price[[pair]]$Setup[i]<-TRUE
              price[[pair]]$AB[i]<-ifelse(price[[pair]]$ActionLow[i]>price[[pair]]$AnchorLow[i],FALSE,TRUE)
              price[[pair]]$TrendState[i]<-paste0("UTS",ifelse(price[[pair]]$AB[i],"AB",""))
              if (price[[pair]]$TrendState[i]=="UTSAB") price[[pair]]$Direction[i]<-"Down"
            } else {
              price[[pair]]$Direction[i]<-"Down"
              price[[pair]]$Setup[i]<-FALSE
              price[[pair]]$AB[i]<-FALSE
              price[[pair]]$TrendState[i]<-"DTNS"
            }
          }
        } else if (price[[pair]]$UpDown[i-1]=="Down" & price[[pair]]$UpDown[i]=="Up") {
          price[[pair]]$ActionHigh[i]<-price[[pair]]$Close[i]
          price[[pair]]$ActionLow[i] <-price[[pair]]$Open[i]
          price[[pair]]$AnchorHigh[i]<-price[[pair]]$ActionHigh[i-1]
          price[[pair]]$AnchorLow[i] <-price[[pair]]$ActionLow[i-1]
          price[[pair]]$LegHigh[i]<-price[[pair]]$AnchorHigh[i-1]
          price[[pair]]$LegLow[i] <-price[[pair]]$AnchorLow[i-1]
          if ((price[[pair]]$AnchorHigh[i]-price[[pair]]$AnchorLow[i]) <
              (price[[pair]]$LegHigh[i]-price[[pair]]$LegLow[i])) {
            price[[pair]]$TrendSide[i]<-"Sideways"
            price[[pair]]$Direction[i]<-"Up"
            if (price[[pair]]$ActionHigh[i]>price[[pair]]$AnchorHigh[i]) {
              price[[pair]]$Confirmed[i]<-TRUE
            } else {
              price[[pair]]$Confirmed[i]<-FALSE
            } 
            price[[pair]]$TrendState[i]<-paste0("SBU",ifelse(price[[pair]]$Confirmed[i],"C",""))
          } else {
            price[[pair]]$TrendSide[i]<-"Trend"
            ClosestLow<-max(which(price[[pair]]$AnchorLow[i]>price[[pair]]$LegLow[1:i]))
            ClosestHigh<-max(which(price[[pair]]$AnchorHigh[i]<price[[pair]]$LegHigh[1:i]))
            if (is.infinite(ClosestHigh) & is.infinite(ClosestLow)) {
              price[[pair]]$Direction[i]<-NA
              price[[pair]]$Setup[i]<-NA
              price[[pair]]$AB[i]<-ifelse(price[[pair]]$ActionHigh[i]<price[[pair]]$AnchorHigh[i],FALSE,TRUE)
              price[[pair]]$TrendState[i]<-NA
            } else if (ClosestHigh>ClosestLow) {
              price[[pair]]$Direction[i]<-"Down"
              price[[pair]]$Setup[i]<-TRUE
              price[[pair]]$AB[i]<-ifelse(price[[pair]]$ActionHigh[i]<price[[pair]]$AnchorHigh[i],FALSE,TRUE)
              price[[pair]]$TrendState[i]<-paste0("DTS",ifelse(price[[pair]]$AB[i],"AB",""))
              if (price[[pair]]$TrendState[i]=="DTSAB") price[[pair]]$Direction[i]<-"Up"
            } else {
              price[[pair]]$Direction[i]<-"Up"
              price[[pair]]$Setup[i]<-FALSE
              price[[pair]]$AB[i]<-FALSE
              price[[pair]]$TrendState[i]<-"UTNS"
            }
          }
        } 
      }
    }
  } else if (class(price)=="data.frame") {
    price$UpDown<-ifelse(price$Close>price$Open,"Up",
                                 ifelse(price$Close<price$Open,"Down","Neutral"))
    # Now kill the Neutrals
    if (price$UpDown[1]=="Neutral") price$UpDown[1]<-"Up"
    for (i in 2:nrow(price)) {
      if (price$UpDown[i]=="Neutral" & price$UpDown[i-1]=="Up") price$UpDown[i]<-"Up"
      if (price$UpDown[i]=="Neutral" & price$UpDown[i-1]=="Down") price$UpDown[i]<-"Down"
    }
    
    # Squashing Colors & TrendStates
    price$ActionHigh<-price$AnchorHigh<-price$LegHigh<-NA
    price$ActionLow<-price$AnchorLow<-price$LegLow<-NA
    price$ActionHigh[1]<-price$AnchorHigh[1]<-price$LegHigh[1]<-max(price[1,c("Open","Close")])
    price$ActionLow[1] <-price$AnchorLow[1]<-price$LegLow[1]<-min(price[1,c("Open","Close")])
    price$TrendState<-price$TrendSide<-price$Direction<-price$Confirmed<-NA
    price$Setup<-price$AB<-FALSE
    for (i in 2:nrow(price)) {
      if (price$UpDown[i-1]=="Up" & price$UpDown[i]=="Up") {
        price$ActionHigh[i]<-price$Close[i]
        price$ActionLow[i] <-price$ActionLow[i-1]
        price$AnchorHigh[i]<-price$AnchorHigh[i-1] # Down
        price$AnchorLow[i] <-price$AnchorLow[i-1]
        price$LegHigh[i]<-price$LegHigh[i-1] # Up
        price$LegLow[i] <-price$LegLow[i-1]
        if ((price$AnchorHigh[i]-price$AnchorLow[i]) <
            (price$LegHigh[i]-price$LegLow[i])) {
          price$TrendSide[i]<-"Sideways"
          price$Direction[i]<-"Up"
          if (price$ActionHigh[i]>price$AnchorHigh[i]) {
            price$Confirmed[i]<-TRUE
          } else {
            price$Confirmed[i]<-FALSE
          } 
          price$TrendState[i]<-paste0("SBU",ifelse(price$Confirmed[i],"C",""))
        } else {
          price$TrendSide[i]<-"Trend"
          ClosestLow<-max(which(price$AnchorLow[i]>price$LegLow[1:i]))
          ClosestHigh<-max(which(price$AnchorHigh[i]<price$LegHigh[1:i]))
          if (is.infinite(ClosestHigh)&is.infinite(ClosestLow)) {
            price$Direction[i]<-NA
            price$Setup[i]<-NA
            price$AB[i]<-ifelse(price$ActionHigh[i]<price$AnchorHigh[i],FALSE,TRUE)
            price$TrendState[i]<-NA
          } else if (ClosestHigh>ClosestLow) {
            price$Direction[i]<-"Down"
            price$Setup[i]<-TRUE
            price$AB[i]<-ifelse(price$ActionHigh[i]<price$AnchorHigh[i],FALSE,TRUE)
            price$TrendState[i]<-paste0("DTS",ifelse(price$AB[i],"AB",""))
            if (price$TrendState[i]=="DTSAB") price$Direction[i]<-"Up"
          } else {
            price$Direction[i]<-"Up"
            price$Setup[i]<-FALSE
            price$AB[i]<-FALSE
            price$TrendState[i]<-"UTNS"
          }
        }
      } else if (price$UpDown[i-1]=="Down" & price$UpDown[i]=="Down") {
        price$ActionHigh[i]<-price$ActionHigh[i-1]
        price$ActionLow[i] <-price$Close[i]
        price$AnchorHigh[i]<-price$AnchorHigh[i-1]
        price$AnchorLow[i] <-price$AnchorLow[i-1]
        price$LegHigh[i]<-price$LegHigh[i-1]
        price$LegLow[i] <-price$LegLow[i-1]
        if ((price$AnchorHigh[i]-price$AnchorLow[i]) <
            (price$LegHigh[i]-price$LegLow[i])) {
          price$TrendSide[i]<-"Sideways"
          price$Direction[i]<-"Down"
          if (price$ActionLow[i]<price$AnchorLow[i]) {
            price$Confirmed[i]<-TRUE
          } else {
            price$Confirmed[i]<-FALSE
          } 
          price$TrendState[i]<-paste0("SBD",ifelse(price$Confirmed[i],"C",""))
        } else {
          price$TrendSide[i]<-"Trend"
          ClosestLow<-max(which(price$AnchorLow[i]>price$LegLow[1:i]))
          ClosestHigh<-max(which(price$AnchorHigh[i]<price$LegHigh[1:i]))
          if (is.infinite(ClosestHigh)&is.infinite(ClosestLow)) {
            price$Direction[i]<-NA
            price$Setup[i]<-NA
            price$AB[i]<-ifelse(price$ActionHigh[i]<price$AnchorHigh[i],FALSE,TRUE)
            price$TrendState[i]<-NA
          } else if (ClosestHigh<ClosestLow) {
            price$Direction[i]<-"Up"
            price$Setup[i]<-TRUE
            price$AB[i]<-ifelse(price$ActionLow[i]>price$AnchorLow[i],FALSE,TRUE)
            price$TrendState[i]<-paste0("UTS",ifelse(price$AB[i],"AB",""))
            if (price$TrendState[i]=="UTSAB") price$Direction[i]<-"Down"
          } else {
            price$Direction[i]<-"Down"
            price$Setup[i]<-FALSE
            price$AB[i]<-FALSE
            price$TrendState[i]<-"DTNS"
          }
        }
      } else if (price$UpDown[i-1]=="Up" & price$UpDown[i]=="Down") {
        price$ActionHigh[i]<-price$Open[i]
        price$ActionLow[i] <-price$Close[i]
        price$AnchorHigh[i]<-price$ActionHigh[i-1]
        price$AnchorLow[i] <-price$ActionLow[i-1]
        price$LegHigh[i]<-price$AnchorHigh[i-1]
        price$LegLow[i] <-price$AnchorLow[i-1]
        if ((price$AnchorHigh[i]-price$AnchorLow[i]) <
            (price$LegHigh[i]-price$LegLow[i])) {
          price$TrendSide[i]<-"Sideways"
          price$Direction[i]<-"Down"
          if (price$ActionLow[i]<price$AnchorLow[i]) {
            price$Confirmed[i]<-TRUE
          } else {
            price$Confirmed[i]<-FALSE
          } 
          price$TrendState[i]<-paste0("SBD",ifelse(price$Confirmed[i],"C",""))
        } else {
          price$TrendSide[i]<-"Trend"
          ClosestLow<-max(which(price$AnchorLow[i]>price$LegLow[1:i]))
          ClosestHigh<-max(which(price$AnchorHigh[i]<price$LegHigh[1:i]))
          if (is.infinite(ClosestHigh)&is.infinite(ClosestLow)) {
            price$Direction[i]<-NA
            price$Setup[i]<-NA
            price$AB[i]<-ifelse(price$ActionHigh[i]<price$AnchorHigh[i],FALSE,TRUE)
            price$TrendState[i]<-NA
          } else if (ClosestHigh<ClosestLow) {
            price$Direction[i]<-"Up"
            price$Setup[i]<-TRUE
            price$AB[i]<-ifelse(price$ActionLow[i]>price$AnchorLow[i],FALSE,TRUE)
            price$TrendState[i]<-paste0("UTS",ifelse(price$AB[i],"AB",""))
            if (price$TrendState[i]=="UTSAB") price$Direction[i]<-"Down"
          } else {
            price$Direction[i]<-"Down"
            price$Setup[i]<-FALSE
            price$AB[i]<-FALSE
            price$TrendState[i]<-"DTNS"
          }
        }
      } else if (price$UpDown[i-1]=="Down" & price$UpDown[i]=="Up") {
        price$ActionHigh[i]<-price$Close[i]
        price$ActionLow[i] <-price$Open[i]
        price$AnchorHigh[i]<-price$ActionHigh[i-1]
        price$AnchorLow[i] <-price$ActionLow[i-1]
        price$LegHigh[i]<-price$AnchorHigh[i-1]
        price$LegLow[i] <-price$AnchorLow[i-1]
        if ((price$AnchorHigh[i]-price$AnchorLow[i]) <
            (price$LegHigh[i]-price$LegLow[i])) {
          price$TrendSide[i]<-"Sideways"
          price$Direction[i]<-"Up"
          if (price$ActionHigh[i]>price$AnchorHigh[i]) {
            price$Confirmed[i]<-TRUE
          } else {
            price$Confirmed[i]<-FALSE
          } 
          price$TrendState[i]<-paste0("SBU",ifelse(price$Confirmed[i],"C",""))
        } else {
          price$TrendSide[i]<-"Trend"
          ClosestLow<-max(which(price$AnchorLow[i]>price$LegLow[1:i]))
          ClosestHigh<-max(which(price$AnchorHigh[i]<price$LegHigh[1:i]))
          if (is.infinite(ClosestHigh)&is.infinite(ClosestLow)) {
            price$Direction[i]<-NA
            price$Setup[i]<-NA
            price$AB[i]<-ifelse(price$ActionHigh[i]<price$AnchorHigh[i],FALSE,TRUE)
            price$TrendState[i]<-NA
          } else if (ClosestHigh>ClosestLow) {
            price$Direction[i]<-"Down"
            price$Setup[i]<-TRUE
            price$AB[i]<-ifelse(price$ActionHigh[i]<price$AnchorHigh[i],FALSE,TRUE)
            price$TrendState[i]<-paste0("DTS",ifelse(price$AB[i],"AB",""))
            if (price$TrendState[i]=="DTSAB") price$Direction[i]<-"Up"
          } else {
            price$Direction[i]<-"Up"
            price$Setup[i]<-FALSE
            price$AB[i]<-FALSE
            price$TrendState[i]<-"UTNS"
          }
        }
      } 
    }
  } else {
    cat("I don't know what kind of data object you gave for price.")
  }
  price
}

LegBaseCount<-function(price) {
  if ((class(price)=="list" && !("UpDown" %in% names(price[[1]]))) |
      (class(price)=="data.frame" && !("UpDown" %in% names(price)))) {
    cat("You need to LabelUpDown(price) first.")
  }
  if (class(price)=="list") {
    for (pair in names(price)) {
      mult<-ifelse(price[[pair]]$Close[1]/10>1,100,10000)
      price[[pair]]$BaseHigh<-price[[pair]]$BaseLow<-NA
      price[[pair]]$PrevBaseHigh<-price[[pair]]$PrevBaseLow<-NA
      price[[pair]]$LegBase<-NA
      price[[pair]]$RallyDropBase<-NA
      price[[pair]]$LegBaseRange<-NA
      price[[pair]]$LegBaseRatio<-NA
      price[[pair]]$Formation<-NA
      price[[pair]]$NLegBase<-NA
      price[[pair]]$BaseHigh[1]<-0;price[[pair]]$BaseLow[1]<-1000
      #price[[pair]]$PrevBaseHigh[1]<-0;price[[pair]]$PrevBaseLow[1]<-1000
      #cumsum with a reset
      #ave(df$a, cumsum(c(F, diff(df$a) < 0)), FUN=seq_along) - 1
      for (i in 2:nrow(price[[pair]])) {
        if (price[[pair]]$Close[i]>=price[[pair]]$Low[i-1] & 
            price[[pair]]$Close[i]<=price[[pair]]$High[i-1]) {
          price[[pair]]$LegBase[i]<-"Base"
          price[[pair]]$BaseHigh[i]<-max(price[[pair]]$BaseHigh[i-1],price[[pair]]$High[i],na.rm=T)
          price[[pair]]$BaseLow[i]<-min(price[[pair]]$BaseLow[i-1],price[[pair]]$Low[i],na.rm=T)
          price[[pair]]$RallyDropBase[i]<-"Base"
          price[[pair]]$LegBaseRange[i]<-mult*(price[[pair]]$BaseHigh[i] - price[[pair]]$BaseLow[i])
          if (sum(price[[pair]]$LegBase[!is.na(price[[pair]]$LegBase)]=="Leg")>0) {
            ind<-max(which(price[[pair]]$LegBase=="Leg"))
            price[[pair]]$PrevBaseHigh[i]<-price[[pair]]$BaseHigh[ind]
            price[[pair]]$PrevBaseLow[i]<-price[[pair]]$BaseLow[ind]
            price[[pair]]$Formation[i]<-ifelse(price[[pair]]$UpDown[ind]=="Up","RallyBase","DropBase")
            price[[pair]]$LegBaseRatio[i]<-mult*(price[[pair]]$High[i]-price[[pair]]$Low[i])/price[[pair]]$LegBaseRange[i]
          } 
          if (is.na(price[[pair]]$LegBase[i-1])) {
            price[[pair]]$NLegBase[i]<-1
          } else {
            price[[pair]]$NLegBase[i]<-1+ifelse(price[[pair]]$LegBase[i-1]=="Base",price[[pair]]$NLegBase[i-1],0)
          }
        } else if (!is.na(price[[pair]]$LegBase[i]) & price[[pair]]$LegBase[i-1]=="Base") {
          if (price[[pair]]$Close[i]>=Low & price[[pair]]$Close[i]<=High) {
            price[[pair]]$LegBase[i]<-"Base"
            price[[pair]]$BaseHigh[i]<-max(price[[pair]]$BaseHigh[i-1],price[[pair]]$High[i],na.rm=T)
            price[[pair]]$BaseLow[i]<-min(price[[pair]]$BaseLow[i-1],price[[pair]]$Low[i],na.rm=T)
            price[[pair]]$RallyDropBase[i]<-"Base"
            price[[pair]]$LegBaseRange[i]<-mult*(price[[pair]]$BaseHigh[i] - price[[pair]]$BaseLow[i])
            if (sum(price[[pair]]$LegBase=="Leg",na.rm = T)>0) {
              ind<-max(which(price[[pair]]$LegBase=="Leg"))
              price[[pair]]$Formation[i]<-ifelse(price[[pair]]$UpDown[ind]=="Up","RallyBase","DropBase")
              price[[pair]]$LegBaseRatio[i]<-mult*(price[[pair]]$High[i]-price[[pair]]$Low[i])/price[[pair]]$LegBaseRange[i]
              price[[pair]]$PrevBaseHigh[i]<-price[[pair]]$BaseHigh[ind]
              price[[pair]]$PrevBaseLow[i]<-price[[pair]]$BaseLow[ind]
            } 
            price[[pair]]$NLegBase[i]<-1+price[[pair]]$NLegBase[i-1]
          } else {
            price[[pair]]$LegBase[i]<-"Leg"
            price[[pair]]$PrevBaseHigh[i]<-price[[pair]]$PrevBaseHigh[i-1]
            price[[pair]]$PrevBaseLow[i]<-price[[pair]]$PrevBaseLow[i-1]
            price[[pair]]$RallyDropBase[i]<-ifelse(price[[pair]]$UpDown[i]=="Up","Rally","Drop")
            if (!is.na(price[[pair]]$LegBase[i-1]) & price[[pair]]$LegBase[i-1]=="Leg" & 
                price[[pair]]$UpDown[i-1]!=price[[pair]]$UpDown[i]) {
              price[[pair]]$Formation[i]<-ifelse(price[[pair]]$UpDown[i]=="Up","ReversalUp","ReversalDown")
              price[[pair]]$LegBaseRange[i]<-ifelse(price[[pair]]$UpDown[i]=="Up",mult*(min(price[[pair]]$Low[c(i,i-1)])-price[[pair]]$Open[i-1]),
                                            mult*(max(price[[pair]]$High[c(i,i-1)])-price[[pair]]$Open[i-1]))
              price[[pair]]$LegBaseRatio[i]<-mult*(price[[pair]]$High[i]-price[[pair]]$Low[i])/price[[pair]]$LegBaseRange[i]
            } else if (sum(price[[pair]]$LegBase=="Base",na.rm = T)>0) {
              ind<-max(which(price[[pair]]$LegBase=="Base"))
              price[[pair]]$Formation[i]<-ifelse(!is.na(price[[pair]]$Formation[ind]),paste0(price[[pair]]$Formation[ind],price[[pair]]$RallyDropBase[i]),NA)
              price[[pair]]$LegBaseRange[i]<-ifelse(price[[pair]]$UpDown[i]=="Up",mult*(price[[pair]]$High[i] - price[[pair]]$Low[ind+1]),
                                            mult*(price[[pair]]$High[ind+1] - price[[pair]]$Low[i]))
              price[[pair]]$LegBaseRatio[i]<-mult*(price[[pair]]$High[i]-price[[pair]]$Low[i])/price[[pair]]$LegBaseRange[i]
            } 
            if (!is.na(price[[pair]]$LegBase[i-1])) {
              price[[pair]]$NLegBase[i]<-1+ifelse(price[[pair]]$LegBase[i-1]=="Leg",price[[pair]]$NLegBase[i-1],0)
            } 
          }
        } else {
          price[[pair]]$LegBase[i]<-"Leg"
          price[[pair]]$PrevBaseHigh[i]<-price[[pair]]$PrevBaseHigh[i-1]
          price[[pair]]$PrevBaseLow[i]<-price[[pair]]$PrevBaseLow[i-1]
          price[[pair]]$RallyDropBase[i]<-ifelse(price[[pair]]$UpDown[i]=="Up","Rally","Drop")
          if (!is.na(price[[pair]]$LegBase[i-1]) & price[[pair]]$LegBase[i-1]=="Leg" & 
              price[[pair]]$UpDown[i-1]!=price[[pair]]$UpDown[i]) {
            price[[pair]]$Formation[i]<-ifelse(price[[pair]]$UpDown[i]=="Up","ReversalUp","ReversalDown")
            price[[pair]]$LegBaseRange[i]<-ifelse(price[[pair]]$UpDown[i]=="Up",mult*(min(price[[pair]]$Low[c(i,i-1)])-price[[pair]]$Open[i-1]),
                                          mult*(max(price[[pair]]$High[c(i,i-1)])-price[[pair]]$Open[i-1]))
            price[[pair]]$LegBaseRatio[i]<-mult*(price[[pair]]$High[i]-price[[pair]]$Low[i])/price[[pair]]$LegBaseRange[i]
          } else if (sum(price[[pair]]$LegBase=="Base",na.rm = T)>0) {
            ind<-max(which(price[[pair]]$LegBase=="Base"))
            price[[pair]]$Formation[i]<-ifelse(!is.na(price[[pair]]$Formation[ind]),paste0(price[[pair]]$Formation[ind],price[[pair]]$RallyDropBase[i]),NA)
            price[[pair]]$LegBaseRange[i]<-ifelse(price[[pair]]$UpDown[i]=="Up",mult*(price[[pair]]$High[i] - price[[pair]]$Low[ind+1]),
                                          mult*(price[[pair]]$High[ind+1] - price[[pair]]$Low[i]))
            price[[pair]]$LegBaseRatio[i]<-mult*(price[[pair]]$High[i]-price[[pair]]$Low[i])/price[[pair]]$LegBaseRange[i]
          } 
          if (is.na(price[[pair]]$LegBase[i-1])) {
            price[[pair]]$NLegBase[i]<-1
          } else {
            price[[pair]]$NLegBase[i]<-1+ifelse(price[[pair]]$LegBase[i-1]=="Leg",price[[pair]]$NLegBase[i-1],0)
          } 
        }
      }
    }
  } else if (class(price)=="data.frame") {
    price$BaseHigh<-price$BaseLow<-NA
    price$PrevBaseHigh<-price$PrevBaseLow<-NA
    price$LegBase<-NA
    price$NLegBase<-NA
    price$RallyDropBase<-NA
    price$LegBaseRange<-NA
    price$LegBaseRatio<-NA
    price$Formation<-NA
    price$BaseHigh[1]<-0;price$BaseLow[1]<-1000
    mult<-ifelse(price$Close[1]/10>1,100,10000)
    #price$PrevBaseHigh[1]<-NA;price$PrevBaseLow[1]<-NA
    #cumsum with a reset
    #ave(df$a, cumsum(c(F, diff(df$a) < 0)), FUN=seq_along) - 1
    for (i in 2:nrow(price)) {
      if (price$Close[i]>=price$Low[i-1] & 
          price$Close[i]<=price$High[i-1]) {
        price$LegBase[i]<-"Base"
        price$BaseHigh[i]<-max(price$BaseHigh[i-1],price$High[i],na.rm=T)
        price$BaseLow[i]<-min(price$BaseLow[i-1],price$Low[i],na.rm=T)
        price$RallyDropBase[i]<-"Base"
        price$LegBaseRange[i]<-mult*(price$BaseHigh[i] - price$BaseLow[i])
        if (sum(price$LegBase[!is.na(price$LegBase)]=="Leg")>0) {
          ind<-max(which(price$LegBase=="Leg"))
          price$PrevBaseHigh[i]<-price$BaseHigh[ind]
          price$PrevBaseLow[i]<-price$BaseLow[ind]
          price$Formation[i]<-ifelse(price$UpDown[ind]=="Up","RallyBase","DropBase")
          price$LegBaseRatio[i]<-mult*(price$High[i]-price$Low[i])/price$LegBaseRange[i]
        } 
        if (is.na(price$LegBase[i-1])) {
          price$NLegBase[i]<-1
        } else {
          price$NLegBase[i]<-1+ifelse(price$LegBase[i-1]=="Base",price$NLegBase[i-1],0)
        }
      } else if (!is.na(price$LegBase[i]) & price$LegBase[i-1]=="Base") {
        if (price$Close[i]>=Low & price$Close[i]<=High) {
          price$LegBase[i]<-"Base"
          price$BaseHigh[i]<-max(price$BaseHigh[i-1],price$High[i],na.rm=T)
          price$BaseLow[i]<-min(price$BaseLow[i-1],price$Low[i],na.rm=T)
          price$RallyDropBase[i]<-"Base"
          price$LegBaseRange[i]<-mult*(price$BaseHigh[i] - price$BaseLow[i])
          if (sum(price$LegBase=="Leg",na.rm = T)>0) {
            ind<-max(which(price$LegBase=="Leg"))
            price$Formation[i]<-ifelse(price$UpDown[ind]=="Up","RallyBase","DropBase")
            price$LegBaseRatio[i]<-mult*(price$High[i]-price$Low[i])/price$LegBaseRange[i]
            price$PrevBaseHigh[i]<-price$BaseHigh[ind]
            price$PrevBaseLow[i]<-price$BaseLow[ind]
          } 
          price$NLegBase[i]<-1+price$NLegBase[i-1]
        } else {
          price$LegBase[i]<-"Leg"
          price$PrevBaseHigh[i]<-price$PrevBaseHigh[i-1]
          price$PrevBaseLow[i]<-price$PrevBaseLow[i-1]
          price$RallyDropBase[i]<-ifelse(price$UpDown[i]=="Up","Rally","Drop")
          if (!is.na(price$LegBase[i-1]) & price$LegBase[i-1]=="Leg" & 
              price$UpDown[i-1]!=price$UpDown[i]) {
            price$Formation[i]<-ifelse(price$UpDown[i]=="Up","ReversalUp","ReversalDown")
            price$LegBaseRange[i]<-ifelse(price$UpDown[i]=="Up",mult*(min(price$Low[c(i,i-1)])-price$Open[i-1]),
                                                  mult*(max(price$High[c(i,i-1)])-price$Open[i-1]))
            price$LegBaseRatio[i]<-mult*(price$High[i]-price$Low[i])/price$LegBaseRange[i]
          } else if (sum(price$LegBase=="Base",na.rm = T)>0) {
            ind<-max(which(price$LegBase=="Base"))
            price$Formation[i]<-ifelse(!is.na(price$Formation[ind]),paste0(price$Formation[ind],price$RallyDropBase[i]),NA)
            price$LegBaseRange[i]<-ifelse(price$UpDown[i]=="Up",mult*(price$High[i] - price$Low[ind+1]),
                                          mult*(price$High[ind+1] - price$Low[i]))
            price$LegBaseRatio[i]<-mult*(price$High[i]-price$Low[i])/price$LegBaseRange[i]
          } 
          if (!is.na(price$LegBase[i-1])) {
            price$NLegBase[i]<-1+ifelse(price$LegBase[i-1]=="Leg",price$NLegBase[i-1],0)
          } 
        }
      } else {
        price$LegBase[i]<-"Leg"
        price$PrevBaseHigh[i]<-price$PrevBaseHigh[i-1]
        price$PrevBaseLow[i]<-price$PrevBaseLow[i-1]
        price$RallyDropBase[i]<-ifelse(price$UpDown[i]=="Up","Rally","Drop")
        if (!is.na(price$LegBase[i-1]) & price$LegBase[i-1]=="Leg" & 
            price$UpDown[i-1]!=price$UpDown[i]) {
          price$Formation[i]<-ifelse(price$UpDown[i]=="Up","ReversalUp","ReversalDown")
          price$LegBaseRange[i]<-ifelse(price$UpDown[i]=="Up",mult*(min(price$Low[c(i,i-1)])-price$Open[i-1]),
                                        mult*(max(price$High[c(i,i-1)])-price$Open[i-1]))
          price$LegBaseRatio[i]<-mult*(price$High[i]-price$Low[i])/price$LegBaseRange[i]
        } else if (sum(price$LegBase=="Base",na.rm = T)>0) {
          ind<-max(which(price$LegBase=="Base"))
          price$Formation[i]<-ifelse(!is.na(price$Formation[ind]),paste0(price$Formation[ind],price$RallyDropBase[i]),NA)
          price$LegBaseRange[i]<-ifelse(price$UpDown[i]=="Up",mult*(price$High[i] - price$Low[ind+1]),
                                        mult*(price$High[ind+1] - price$Low[i]))
          price$LegBaseRatio[i]<-mult*(price$High[i]-price$Low[i])/price$LegBaseRange[i]
        } 
        if (is.na(price$LegBase[i-1])) {
          price$NLegBase[i]<-1
        } else {
          price$NLegBase[i]<-1+ifelse(price$LegBase[i-1]=="Leg",price$NLegBase[i-1],0)
        } 
      }
    }
  } else {cat("I don't know what kind of data object this is.")}
  price
}

ABCount<-function(price) {
  if ((class(price)=="list" && !("UpDown" %in% names(price[[1]]))) |
      (class(price)=="data.frame" && !("UpDown" %in% names(price)))) {
    cat("You need to LabelUpDown(price) first.")
  }
  for (pair in names(price)) {
    price[[pair]]$ABUpDown<-NA             #Leg or Base T/F
    price[[pair]]$NAB<-NA
    #High<-0;Low<-1000
    for (i in 2:nrow(price[[pair]])) {
      if (price[[pair]]$UpDown[i]=="Up" & price[[pair]]$UpDown[i-1]=="Down" &
          price[[pair]]$Close[i]>=price[[pair]]$Low[i-1] & 
            price[[pair]]$Close[i]<=price[[pair]]$High[i-1]   ) {
        price[[pair]]$LegBase[i]<-"Base"
        High<-max(High,price[[pair]]$High[i])
        Low<-min(Low,price[[pair]]$Low[i])
        if (is.na(price[[pair]]$LegBase[i-1])) {
          price[[pair]]$NLegBase[i]<-1
        } else {
          price[[pair]]$NLegBase[i]<-1+ifelse(price[[pair]]$LegBase[i-1]=="Base",price[[pair]]$NLegBase[i-1],0)
        }
      } else if (!is.na(price[[pair]]$LegBase[i]) & price[[pair]]$LegBase[i-1]=="Base") {
        if (price[[pair]]$Close[i]>=Low & price[[pair]]$Close[i]<=High) {
          price[[pair]]$LegBase[i]<-"Base"
          High<-max(High,price[[pair]]$High[i])
          Low<-min(Low,price[[pair]]$Low[i])
          if (!is.na(price[[pair]]$LegBase[i-1])) {
            price[[pair]]$NLegBase[i]<-1+ifelse(price[[pair]]$LegBase[i-1]=="Base",price[[pair]]$NLegBase[i-1],0)
          } 
        } else {
          price[[pair]]$LegBase[i]<-"Leg"
          High<-0
          Low<-1000
          if (!is.na(price[[pair]]$LegBase[i-1])) {
            price[[pair]]$NLegBase[i]<-1+ifelse(price[[pair]]$LegBase[i-1]=="Leg",price[[pair]]$NLegBase[i-1],0)
          } 
        }
      } else {
        price[[pair]]$LegBase[i]<-"Leg"
        High<-0
        Low<-1000
        if (is.na(price[[pair]]$LegBase[i-1])) {
          price[[pair]]$NLegBase[i]<-1
        } else {
          price[[pair]]$NLegBase[i]<-1+ifelse(price[[pair]]$LegBase[i-1]=="Leg",price[[pair]]$NLegBase[i-1],0)
        } 
      }
    }
  }
}

BuildHistory<-function(price){
  #price<-JW
  if (class(price)=="list") {
    for (pair in names(price)) {
      price[[pair]]$TrendStateP1<-price[[pair]]$TrendStateP2<-price[[pair]]$TrendStateP3<-price[[pair]]$TrendState[max(which(is.na(price[[pair]]$TrendState)))]
      price[[pair]]$TrendStateN<-price[[pair]]$TrendStateN1<-price[[pair]]$TrendStateN2<-price[[pair]]$TrendStateN3<-1
      price[[pair]]$TrendStatePrev1<-price[[pair]]$TrendStatePrev2<-price[[pair]]$TrendStatePrev3<-paste0(price[[pair]]$TrendState[max(which(is.na(price[[pair]]$TrendState)))],1)
      ROWS<-(max(which(is.na(price[[pair]]$TrendState)))+2):nrow(price[[pair]])
      for (j in ROWS) {
        if (price[[pair]]$TrendState[j]==price[[pair]]$TrendState[j-1]) {
          price[[pair]]$TrendStateN[j]<-1+price[[pair]]$TrendStateN[j-1]
          price[[pair]]$TrendStateP1[j]<-price[[pair]]$TrendStateP1[j-1]
          price[[pair]]$TrendStateP2[j]<-price[[pair]]$TrendStateP2[j-1]
          price[[pair]]$TrendStateP3[j]<-price[[pair]]$TrendStateP3[j-1]
          price[[pair]]$TrendStateN1[j]<-price[[pair]]$TrendStateN1[j-1]
          price[[pair]]$TrendStateN2[j]<-price[[pair]]$TrendStateN2[j-1]
          price[[pair]]$TrendStateN3[j]<-price[[pair]]$TrendStateN3[j-1]
          price[[pair]]$TrendStatePrev1[j]<-price[[pair]]$TrendStatePrev1[j-1]
          price[[pair]]$TrendStatePrev2[j]<-price[[pair]]$TrendStatePrev2[j-1]
          price[[pair]]$TrendStatePrev3[j]<-price[[pair]]$TrendStatePrev3[j-1]
        } else {
          price[[pair]]$TrendStateN[j]<-1
          price[[pair]]$TrendStateP1[j]<-price[[pair]]$TrendState[j-1]
          price[[pair]]$TrendStateP2[j]<-price[[pair]]$TrendStateP1[j-1]
          price[[pair]]$TrendStateP3[j]<-price[[pair]]$TrendStateP2[j-1]
          price[[pair]]$TrendStateN1[j]<-price[[pair]]$TrendStateN[j-1]
          price[[pair]]$TrendStateN2[j]<-price[[pair]]$TrendStateN1[j-1]
          price[[pair]]$TrendStateN3[j]<-price[[pair]]$TrendStateN2[j-1]
          price[[pair]]$TrendStatePrev1[j]<-paste0(price[[pair]]$TrendState[j-1],price[[pair]]$TrendStateN[j-1])
          price[[pair]]$TrendStatePrev2[j]<-price[[pair]]$TrendStatePrev1[j-1]
          price[[pair]]$TrendStatePrev3[j]<-price[[pair]]$TrendStatePrev2[j-1]
        }
      }
    }
  } else if (class(price)=="data.frame") {
    price$TrendStateP1<-price$TrendStateP2<-price$TrendStateP3<-price$TrendState[max(which(is.na(price$TrendState)))]
    price$TrendStateN<-price$TrendStateN1<-price$TrendStateN2<-price$TrendStateN3<-1
    price$TrendStatePrev1<-price$TrendStatePrev2<-price$TrendStatePrev3<-paste0(price$TrendState[max(which(is.na(price$TrendState)))],1)
    ROWS<-(max(which(is.na(price$TrendState)))+2):nrow(price)
    for (j in ROWS) { #j=11
      if (price$TrendState[j]==price$TrendState[j-1]) {
        price$TrendStateN[j]<-1+price$TrendStateN[j-1]
        price$TrendStateP1[j]<-price$TrendStateP1[j-1]
        price$TrendStateP2[j]<-price$TrendStateP2[j-1]
        price$TrendStateP3[j]<-price$TrendStateP3[j-1]
        price$TrendStateN1[j]<-price$TrendStateN1[j-1]
        price$TrendStateN2[j]<-price$TrendStateN2[j-1]
        price$TrendStateN3[j]<-price$TrendStateN3[j-1]
        price$TrendStatePrev1[j]<-price$TrendStatePrev1[j-1]
        price$TrendStatePrev2[j]<-price$TrendStatePrev2[j-1]
        price$TrendStatePrev3[j]<-price$TrendStatePrev3[j-1]
      } else {
        price$TrendStateN[j]<-1
        price$TrendStateP1[j]<-price$TrendState[j-1]
        price$TrendStateP2[j]<-price$TrendStateP1[j-1]
        price$TrendStateP3[j]<-price$TrendStateP2[j-1]
        price$TrendStateN1[j]<-price$TrendStateN[j-1]
        price$TrendStateN2[j]<-price$TrendStateN1[j-1]
        price$TrendStateN3[j]<-price$TrendStateN2[j-1]
        price$TrendStatePrev1[j]<-paste0(price$TrendState[j-1],price$TrendStateN[j-1])
        price$TrendStatePrev2[j]<-price$TrendStatePrev1[j-1]
        price$TrendStatePrev3[j]<-price$TrendStatePrev2[j-1]
      }
    }
  } else {
    cat("I don't know what kind of data object you gave for price.")
    return(NULL)
  }
  price  
}
