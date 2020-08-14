AllPairsPriceHistory<-function(TimeInSeconds=600,Seconds=15,Minutes=NA,Hours=NA) {
  # -- character ---- # Your Account Type "practice" or "live"
  OA_At <- "practice"  
  # Your Account ID and Token
  OA_Ai <- readRDS("MyAccount.rds"); OA_Ak <- readRDS("MyToken.rds")
  # Hour of the "End of the Day" and "TimeZone"
  OA_Da <- 17;  OA_Ta <- "EST"
  # -- character ---- # Granularity of the prices
  if (!is.na(Seconds)) {
    if (Seconds %in% c(5,10,15,30)) {
      OA_Gn <- paste0("S",Seconds)
    } else {
      print("Pick a Valid Seconds Time value: 5, 10, 15, or 30")
      return(NULL)
    }
  } else if (!is.na(Minutes)) {
    if (Minutes %in% c(1,2,4,5,10,15,30)) {
      OA_Gn <- paste0("M",Minutes)
    } else {
      print("Pick a Valid Minutes Time value: 1, 2, 4, 5, 10, 15, or 30")
      return(NULL)
    }
  } else if (!is.na(Hours)) {
    if (Hours %in% c(1,2,3,4,6,8,12)) {
      OA_Gn <- paste0("H",Seconds)
    } else {
      print("Pick a Valid Hours Time value: 1, 2, 3, 4, 6, 8, 12")
      return(NULL)
    }
  } else {
    print("Pick a valid time frame")
    return(NULL)
  }
  
  # start and stop
  OA_F1Px<-str_replace(paste0(as.character(Sys.time()-TimeInSeconds),"-05:00")," ","T")
  OA_F2Px<-str_replace(paste0(as.character(Sys.time()),"-05:00")," ","T")
  
  CPairs<-c("EUR_USD","GBP_USD","AUD_USD","NZD_USD","USD_CAD","USD_JPY","USD_CHF",
            "EUR_GBP","EUR_CHF","EUR_CAD","EUR_NZD","EUR_AUD","EUR_JPY",
            "GBP_AUD","GBP_NZD","GBP_JPY","GBP_CHF","GBP_CAD",
            "CAD_JPY","AUD_CAD","NZD_CAD","CAD_CHF",
            "AUD_NZD","AUD_JPY","AUD_CHF",
            "NZD_JPY","NZD_CHF",
            "CHF_JPY")
  Prices<-list()
  for (p in CPairs) {
    OA_In <- p
    Prices[[p]]<-HisPrices(Account=OA_Ai, AccountType=OA_At,Granularity=OA_Gn,DayAlign=OA_Da,Token=OA_Ak,
                      Instrument=OA_In,Start=OA_F1Px,End=OA_F2Px,Count = NULL, TimeAlign=OA_Ta)
  }
  Prices
}


CheckTimeTicks<-function(t.start,t.stop) {
  if (class(t.start)[1]=="POSIXct" & class(t.stop)[1]=="POSIXct") {
    if (t.start<t.stop) {
      # print("Pick a Valid Seconds Time value: 5, 10, 15, or 30")
      # print("Pick a Valid Minutes Time value: 1, 2, 4, 5, 10, 15, or 30")
      # print("Pick a Valid Hours Time value: 1, 2, 3, 4, 6, 8, 12")
      print("Assuming you want at least 100 candles, here are the units you can select:")
      SEC<-difftime(t.stop,t.start,units = "secs")
      MIN<-difftime(t.stop,t.start,units = "mins")
      HRS<-difftime(t.stop,t.start,units = "hours")
      if (SEC>500) {
        cat("\nSECONDS :: ")
        for (s in c(5,10,15,30)) {
          if (SEC>=100*s) {
            cand<-paste0(ifelse(SEC>s*500,500,floor(SEC/s)),": S",s)
            cat(cand, " || ")
          }
        }
      }
      if (MIN>=100) {
        cat("\nMINUTES :: ")
        for (m in c(1,2,4,5,10,15,30)) {
          if (MIN>=100*m) {
            cand<-paste0(ifelse(MIN>m*500,500,floor(MIN/m)),": M",m)
            cat(cand, " || ")
          }
        }
      }
      if (HRS>=100) {
        cat("\nHOURS :: ")
        for (h in c(1,2,3,4,6,8,12)) {
          if (HRS>=100*h) {
            cand<-paste0(ifelse(HRS>h*500,500,floor(HRS/h)),": H",h)
            cat(cand, " || ")
          }
        }
      }
    } else {
      print("Your start and stop times might be flipped.")
      return(FALSE)
    }
  } else {
    print("Times are not the proper class of object.")
    return(FALSE)
  }
  return(TRUE)
}

GetAllPrices<-GetCorrelations<-function(Seconds=NA,Minutes=1,Hours=NA,LookBackHours=NA,t.start=NA,t.stop=NA) {
  stopifnot(is.na(LookBackHours)|(is.na(t.start)&is.na(t.stop)))
  # Hours=1
  # -- character ---- # Your Account Type "practice" or "live"
  OA_At <- "practice"  
  # Your Account ID and Token
  OA_Ai <- readRDS("MyAccount.rds"); OA_Ak <- readRDS("MyToken.rds")
  # Hour of the "End of the Day" and "TimeZone"
  OA_Da <- 17;  OA_Ta <- "EST"
  # -- character ---- # Granularity of the prices
  if (!is.na(Seconds)) {
    if (Seconds %in% c(5,10,15,30)) {
      OA_Gn <- paste0("S",Seconds)
    } else {
      print("Pick a Valid Seconds Time value: 5, 10, 15, or 30")
      return(NULL)
    }
  } else if (!is.na(Minutes)) {
    if (Minutes %in% c(1,2,4,5,10,15,30)) {
      OA_Gn <- paste0("M",Minutes)
    } else {
      print("Pick a Valid Minutes Time value: 1, 2, 4, 5, 10, 15, or 30")
      return(NULL)
    }
  } else if (!is.na(Hours)) {
    if (Hours %in% c(1,2,3,4,6,8,12)) {
      OA_Gn <- paste0("H",Hours)
    } else {
      print("Pick a Valid Hours Time value: 1, 2, 3, 4, 6, 8, 12")
      return(NULL)
    }
  } else {
    print("Pick a valid time frame")
    return(NULL)
  }
  
  # start and stop
  if (!is.na(LookBackHours)) {
    OA_F1Px<-str_replace(paste0(as.character(Sys.time()-3600*LookBackHours),"-05:00")," ","T")
    OA_F2Px<-str_replace(paste0(as.character(Sys.time()),"-05:00")," ","T")
  } else if (!is.na(t.start) & !is.na(t.stop)) {
    if (CheckTimeTicks(t.start=t.start,t.stop=t.stop)) {
      OA_F1Px<-str_replace(paste0(as.character(t.start),"-05:00")," ","T")
      OA_F2Px<-str_replace(paste0(as.character(t.stop),"-05:00")," ","T")
    } else {
      print("Your start and stop timing isn't properly entered.")
      return(NULL)
    }
  } else {
    print("Your start, stop, or lookback timing isn't properly entered.")
    return(NULL)
  }
  
  CPairs<-c("EUR_USD","GBP_USD","AUD_USD","NZD_USD","USD_CAD","USD_JPY","USD_CHF",
            "EUR_GBP","EUR_CHF","EUR_CAD","EUR_NZD","EUR_AUD","EUR_JPY",
            "GBP_AUD","GBP_NZD","GBP_JPY","GBP_CHF","GBP_CAD",
            "CAD_JPY","AUD_CAD","NZD_CAD","CAD_CHF",
            "AUD_NZD","AUD_JPY","AUD_CHF",
            "NZD_JPY","NZD_CHF",
            "CHF_JPY")
  Changes<-list()
  Prices<-list()
  for (p in CPairs) {
    OA_In <- p
    PRICES<-HisPrices(Account=OA_Ai, AccountType=OA_At,Granularity=OA_Gn,DayAlign=OA_Da,Token=OA_Ak,
                      Instrument=OA_In,Start=OA_F1Px,End=OA_F2Px,Count = NULL, TimeAlign=OA_Ta)
    Changes[[p]]<-data.frame(TimeStamp=PRICES$TimeStamp[-1],
                             change=diff(as.numeric(PRICES$Close))/as.numeric(PRICES$Close[-length(PRICES$Close)]))
    colnames(Changes[[p]])[2]<-p
    #Changes[[p]]<-diff(as.numeric(PRICES$Close))/as.numeric(PRICES$Close[-length(PRICES$Close)])
    Prices[[p]]<-PRICES
  }
  Prices
}

