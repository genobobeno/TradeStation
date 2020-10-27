AllPairsPriceHistory<-function(TimeInSeconds=600,Seconds=15,Minutes=NA,Hours=NA,Daily=FALSE,Weekly=FALSE,Monthly=FALSE) {
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
  } else if (Daily) {
    OA_Gn <- "D"
  } else if (Weekly) {
    OA_Gn <- "W"
  } else if (Monthly) {
    OA_Gn <- "M"
  } else {
    print("Pick a valid time frame")
    return(NULL)
  }
  
  # start and stop
  # ifelse for daylight savings
  t.adj<-ifelse(round(as.numeric(`attr<-`(Sys.time(),"tzone","GMT") - 
                            as.POSIXct(format(Sys.time()),tz="GMT")))==4,"-04:00","-05:00") 
  OA_F1Px<-str_replace(paste0(as.character(Sys.time()-TimeInSeconds),t.adj)," ","T")
  OA_F2Px<-str_replace(paste0(as.character(Sys.time()),t.adj)," ","T")
  
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
    if (grepl("4",t.adj)) Prices[[p]]$TimeStamp<-Prices[[p]]$TimeStamp+3600
    
    Prices[[p]][,4:7]<-as.numeric(Prices[[p]][,4:7])
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

GetAllPrices<-function(Seconds=NA,Minutes=NA,Hours=NA,Days=NA,Weeks=NA,Months=NA,
                       LookBackHours=NA,t.start=NA,t.stop=NA,candle.complete=FALSE) {
  stopifnot(((is.na(LookBackHours)|(is.na(t.start)&is.na(t.stop))) &
              (!is.na(Seconds) | !is.na(Minutes) | !is.na(Hours))) |
              (!is.na(Days)|!is.na(Weeks)|!is.na(Months)))
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
  } else if (!is.na(Days)) {
    OA_Gn <- "D"
  } else if (!is.na(Weeks)) {
    OA_Gn <- "W"
  } else if (!is.na(Months)) {
    OA_Gn <- "M"
  } else {
    print("Pick a valid time frame")
    return(NULL)
  }
  
  # start and stop
  t.adj<-ifelse(round(as.numeric(`attr<-`(Sys.time(),"tzone","GMT") -
                                   as.POSIXct(format(Sys.time()),tz="GMT")))==4,"-04:00","-05:00")
  if (!is.na(LookBackHours)) {
    OA_F1Px<-str_replace(paste0(as.character(Sys.time()-3600*LookBackHours),t.adj)," ","T")
    OA_F2Px<-str_replace(paste0(as.character(Sys.time()),t.adj)," ","T")
  } else if (!is.na(t.start) & !is.na(t.stop)) {
    if (CheckTimeTicks(t.start=t.start,t.stop=t.stop)) {
      OA_F1Px<-str_replace(paste0(as.character(t.start),t.adj)," ","T")
      OA_F2Px<-str_replace(paste0(as.character(t.stop),t.adj)," ","T")
    } else {
      print("Your start and stop timing isn't properly entered.")
      return(NULL)
    }
  } else if (!is.na(Days)|!is.na(Weeks)|!is.na(Months)) {
    if (!is.na(Days)) {
      TMult<-3600*24*Days
    } else if (!is.na(Weeks)) {
      TMult<-3600*24*7*Weeks
    } else if (!is.na(Months)) {
      TMult<-3600*24*30*Months
    } else {print("Something weird going on with your Days, Weeks, or Months selection")}
    OA_F1Px<-str_replace(paste0(as.character(Sys.time()-TMult),t.adj)," ","T")
    OA_F2Px<-str_replace(paste0(as.character(Sys.time()),t.adj)," ","T")
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
    #p<-CPairs[1]
    OA_In <- p
    PRICES<-HisPrices(Account=OA_Ai, AccountType=OA_At,Granularity=OA_Gn,DayAlign=OA_Da,Token=OA_Ak,
                      Instrument=OA_In,Start=OA_F1Px,End=OA_F2Px,Count = NULL, TimeAlign=OA_Ta)
    if (grepl("4",t.adj)) PRICES$TimeStamp<-PRICES$TimeStamp+3600
    Changes[[p]]<-data.frame(TimeStamp=PRICES$TimeStamp[-1],
                             change=diff(as.numeric(PRICES$Close))/as.numeric(PRICES$Close[-length(PRICES$Close)]))
    colnames(Changes[[p]])[2]<-p
    #Changes[[p]]<-diff(as.numeric(PRICES$Close))/as.numeric(PRICES$Close[-length(PRICES$Close)])
    if (candle.complete) {
      Prices[[p]]<-PRICES[PRICES$Complete,]
    } else {
      Prices[[p]]<-PRICES
    }
  }
  Prices
}


GetPairPrices<-function(Pair,Seconds=NA,Minutes=NA,Hours=NA,
                        Daily=FALSE,Weekly=FALSE,Monthly=FALSE,
                        LookBackHours=NA,t.start=NA,t.stop=NA) {
  #Pair="EUR_CAD";Seconds=NA;Minutes=NA;Hours=4;Daily=FALSE;Weekly=FALSE;Monthly=FALSE;LookBackHours=NA
  #t.start=t.Start;t.stop=t.Stop
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
  } else if (Daily) {
    OA_Gn <- "D"
  } else if (Weekly) {
    OA_Gn <- "W"
  } else if (Monthly) {
    OA_Gn <- "M"
  }  else {
    print("Pick a valid time frame")
    return(NULL)
  }
  
  # start and stop
  t.adj<-ifelse(round(as.numeric(`attr<-`(Sys.time(),"tzone","GMT") - 
                                   as.POSIXct(format(Sys.time()),tz="GMT")))==4,"-04:00","-05:00") 
  if (!is.na(LookBackHours)) {
    OA_F1Px<-str_replace(paste0(as.character(Sys.time()-3600*LookBackHours),t.adj)," ","T")
    OA_F2Px<-str_replace(paste0(as.character(Sys.time()),t.adj)," ","T")
  } else if (!is.na(t.start) & !is.na(t.stop)) {
    if (CheckTimeTicks(t.start=t.start,t.stop=t.stop)) {
      OA_F1Px<-str_replace(paste0(as.character(t.start),t.adj)," ","T")
      OA_F2Px<-str_replace(paste0(as.character(t.stop),t.adj)," ","T")
    } else {
      print("Your start and stop timing isn't properly entered.")
      return(NULL)
    }
  } else {
    print("Your start, stop, or lookback timing isn't properly entered.")
    return(NULL)
  }
  
  if (Pair %in% c("EUR_USD","GBP_USD","AUD_USD","NZD_USD","USD_CAD","USD_JPY","USD_CHF",
            "EUR_GBP","EUR_CHF","EUR_CAD","EUR_NZD","EUR_AUD","EUR_JPY",
            "GBP_AUD","GBP_NZD","GBP_JPY","GBP_CHF","GBP_CAD",
            "CAD_JPY","AUD_CAD","NZD_CAD","CAD_CHF",
            "AUD_NZD","AUD_JPY","AUD_CHF",
            "NZD_JPY","NZD_CHF",
            "CHF_JPY")) {
    #p<-CPairs[1]
    cat("Pulling",Pair)
    OA_In <- Pair
    PRICES<-HisPrices(Account=OA_Ai, AccountType=OA_At,Granularity=OA_Gn,DayAlign=OA_Da,Token=OA_Ak,
                      Instrument=OA_In,Start=OA_F1Px,End=OA_F2Px,Count = NULL, TimeAlign=OA_Ta)
    if (grepl("4",t.adj)) PRICES$TimeStamp<-PRICES$TimeStamp+3600
    
    # Changes[[p]]<-data.frame(TimeStamp=PRICES$TimeStamp[-1],
    #                          change=diff(as.numeric(PRICES$Close))/as.numeric(PRICES$Close[-length(PRICES$Close)]))
    # colnames(Changes[[p]])[2]<-p
    # #Changes[[p]]<-diff(as.numeric(PRICES$Close))/as.numeric(PRICES$Close[-length(PRICES$Close)])
  } else { cat("Pair must not be formatted correctly") ; return(NULL) }
  PRICES
}

CollectData<-function(t.start=NA,t.stop=NA) {
  # Hours=1
  # -- character ---- # Your Account Type "practice" or "live"
  OA_At <- "practice"  
  # Your Account ID and Token
  OA_Ai <- readRDS("MyAccount.rds"); OA_Ak <- readRDS("MyToken.rds")
  # Hour of the "End of the Day" and "TimeZone"
  OA_Da <- 17;  OA_Ta <- "EST"
  # -- character ---- # Granularity of the prices
  # start and stop
  t.adj<-ifelse(round(as.numeric(`attr<-`(Sys.time(),"tzone","GMT") - 
                                   as.POSIXct(format(Sys.time()),tz="GMT")))==4,"-04:00","-05:00") 
  if (CheckTimeTicks(t.start=t.start,t.stop=t.stop)) {
    OA_F1Px<-str_replace(paste0(as.character(t.start),t.adj)," ","T")
    OA_F2Px<-str_replace(paste0(as.character(t.stop),t.adj)," ","T")
  } else {
    print("Your start and stop timing isn't properly entered.")
    return(NULL)
  }
  CPairs<-c("EUR_USD","GBP_USD","AUD_USD","NZD_USD","USD_CAD","USD_JPY","USD_CHF",
            "EUR_GBP","EUR_CHF","EUR_CAD","EUR_NZD","EUR_AUD","EUR_JPY",
            "GBP_AUD","GBP_NZD","GBP_JPY","GBP_CHF","GBP_CAD",
            "CAD_JPY","AUD_CAD","NZD_CAD","CAD_CHF",
            "AUD_NZD","AUD_JPY","AUD_CHF",
            "NZD_JPY","NZD_CHF",
            "CHF_JPY")
  P<-list()
  for (Minutes in c(1,5,30)) {
    OA_Gn <- paste0("M",Minutes)
    Changes<-list()
    Prices<-list()
    if (Minutes<30) {
      
      t.stop5<-t.stop
      
      for (p in CPairs) {
        #p<-CPairs[1]
        OA_In <- p
        PRICES<-HisPrices(Account=OA_Ai, AccountType=OA_At,Granularity=OA_Gn,DayAlign=OA_Da,Token=OA_Ak,
                          Instrument=OA_In,Start=OA_F1Px,End=OA_F2Px,Count = NULL, TimeAlign=OA_Ta)
        if (grepl("4",t.adj)) PRICES$TimeStamp<-PRICES$TimeStamp+3600
        
        Changes[[p]]<-data.frame(TimeStamp=PRICES$TimeStamp[-1],
                                 change=diff(as.numeric(PRICES$Close))/as.numeric(PRICES$Close[-length(PRICES$Close)]))
        colnames(Changes[[p]])[2]<-p
        #Changes[[p]]<-diff(as.numeric(PRICES$Close))/as.numeric(PRICES$Close[-length(PRICES$Close)])
        Prices[[p]]<-PRICES
      }
    }
    ### Now loop on merging time
    P[[OA_Gn]]<-Prices
  }
  P
}

CheckTimeTicks(t.start=Sys.time()-120*3600,t.stop=Sys.time())

GetSessionPrices<-function(Seconds=NA,Minutes=1,Hours=NA,LookBackHours=NA,Date=NA,Session=NA) {
 # stopifnot(is.na(LookBackHours)|(is.na(t.start)&is.na(t.stop)))
  # Hours=1
  # -- character ---- # Your Account Type "practice" or "live"
  OA_At <- "practice"  
  # Your Account ID and Token
  OA_Ai <- readRDS("MyAccount.rds"); OA_Ak <- readRDS("MyToken.rds")
  # Hour of the "End of the Day" and "TimeZone"
  OA_Da <- 0
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
  
  # if (round(as.numeric(`attr<-`(Sys.time(),"tzone","GMT") -
  #                      as.POSIXct(format(Sys.time()),tz="GMT")))==4) {
    OA_Ta <- "America/New_York"

  if (class(Date)=="character") {
    Date<-as.Date(Date)
  }
  if (Session=="Asia") {
    OA_F1Px<-as.POSIXct(paste0(as.character(Date-1)," 22:00:00"),tz = "GMT")
    OA_F2Px<-as.POSIXct(paste0(as.character(Date)," 07:00:00"),tz = "GMT")
    attr(OA_F1Px, "tzone") <- "America/New_York"
    attr(OA_F2Px, "tzone") <- "America/New_York"
    OA_F1Px<-str_replace(as.character(OA_F1Px)," ","T")
    OA_F2Px<-str_replace(as.character(OA_F2Px)," ","T")
  } else if (Session=="Europe") {
    OA_F1Px<-as.POSIXct(paste0(as.character(Date)," 07:00:00"),tz = "GMT")
    OA_F2Px<-as.POSIXct(paste0(as.character(Date)," 16:00:00"),tz = "GMT")
    attr(OA_F1Px, "tzone") <- "America/New_York"
    attr(OA_F2Px, "tzone") <- "America/New_York"
    OA_F1Px<-str_replace(as.character(OA_F1Px)," ","T")
    OA_F2Px<-str_replace(as.character(OA_F2Px)," ","T")
  } else if (Session=="America") {
    OA_F1Px<-as.POSIXct(paste0(as.character(Date)," 12:00:00"),tz = "GMT")
    OA_F2Px<-as.POSIXct(paste0(as.character(Date)," 20:00:00"),tz = "GMT")
    attr(OA_F1Px, "tzone") <- "America/New_York"
    attr(OA_F2Px, "tzone") <- "America/New_York"
    OA_F1Px<-str_replace(as.character(OA_F1Px)," ","T")
    OA_F2Px<-str_replace(as.character(OA_F2Px)," ","T")
  } else {
    print("Choices for Session are 'Asia', 'Europe', or 'America'")
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
    #p<-CPairs[1]
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
