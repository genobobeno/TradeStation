
source("Initialize.R")
##### Plot Pricing

# saveRDS(df.Price,"NiceMorning.rds")
# 
# t.Start<-Sys.time()-17*3600
# t.Stop<-Sys.time() #-24*7.5*3600
# CheckTimeTicks(t.start = t.Start,t.stop = t.Stop)
# df.Price<-GetAllPrices(t.start = t.Start,t.stop = t.Stop, Minutes = 2)
############################################
# TestWeeks<-list()
# t.Start = as.POSIXct("2020-07-12 18:00:00 EST")
# t.Stop = as.POSIXct("2020-07-17 18:00:00 EST")
# TestWeeks[["W1"]]<-GetAllPrices(t.start = t.Start,t.stop = t.Stop, Minutes = 10)
# t.Start = as.POSIXct("2020-07-19 18:00:00 EST")
# t.Stop = as.POSIXct("2020-07-24 18:00:00 EST")
# TestWeeks[["W2"]]<-GetAllPrices(t.start = t.Start,t.stop = t.Stop, Minutes = 10)
# t.Start = as.POSIXct("2020-07-26 18:00:00 EST")
# t.Stop = as.POSIXct("2020-07-31 18:00:00 EST")
# TestWeeks[["W3"]]<-GetAllPrices(t.start = t.Start,t.stop = t.Stop, Minutes = 10)
# t.Start = as.POSIXct("2020-08-02 18:00:00 EST")
# t.Stop = as.POSIXct("2020-08-07 18:00:00 EST")
# TestWeeks[["W4"]]<-GetAllPrices(t.start = t.Start,t.stop = t.Stop, Minutes = 10)

#saveRDS(TestWeeks,"FourWeeksTestData.rds")
TestWeeks<-readRDS("FourWeeksTestData.rds")
PAIRS<-names(TestWeeks[[1]])

### So far, best parameters: 3 Candles, PCUT=1.2%, Score cut = 1.1-1.15 pips
TestList<-list()
TradeList<-list()
PairPipsList<-list()

for (w in 1:4) {
  for (m in c(2)) {
    EntryModel<-readRDS(paste0("GLM_M",m,"a.rds"))
    EntryModel2<-readRDS(paste0("GLM_M",m-1,"a.rds"))
    ExitModel<-readRDS(paste0("GLM_M",m-1,"a.rds"))
    for (p in c(8,10,12,14)) {
    for (s in c(65,70,75)) {
      if (m==2 & s>80) next
      if (m==3 & (s<70 | p==5 )) next
      if (m==4 & s<80) next
      cat("\n******************************************************************************\n")
      cat("Now testing Week",w,"and",p/10,"% and Model looking",m,"candles ahead. Score cut at",s/100)
      ##cat("\n**********************************************************")
      
      TestName<-paste0("W",w,"_M",m,"_P",p,"_S",s)
      PCUT<-p/1000
      PCUT2<-0.01
      Pips<-data.frame(Pair=character(0),Trades=numeric(0),TotalPips=numeric(0))
      Trades<-data.frame(Pair=character(0),enter.index=numeric(0),exit.index=numeric(0),
                         entries=character(0),exits=character(0),
                         trades=numeric(0),wins=character(0),
                         enter.pred=numeric(0), exit.pred=numeric(0))
      for (pair in PAIRS) {
        #cat(pair,";")
        #pair<-names(df.Price)[1] 
        JW<-CleanOpen(TestWeeks[[paste0("W",w)]][[pair]])
        #JW<-CleanOpen(df.Price[["GBP_JPY"]])
        JW<-LabelUpDown(price=JW,multiple.pairs = FALSE)
        JW<-LegBaseCount(price=JW)
        JW<-BuildHistory(price=JW)
        JW<-AppendIndicators(price=JW)
        JW<-AppendFeatures(price=JW)
        JW$emacross<-factor(JW$emacross)
        JW$TrendUp<-factor(JW$TrendUp)
        JW$TrendDown<-factor(JW$TrendDown)
        JW$HighSqueeze<-factor(JW$HighSqueeze)
        JW$LowSqueeze<-factor(JW$LowSqueeze)
        JW$TrendState<-factor(JW$TrendState)
        JW$TrendStateP1<-factor(JW$TrendStateP1)
        JW$LegBase<-factor(JW$LegBase)
        mult<-ifelse(JW$Close[1]/10>1,100,10000)
        #JW$atr14
        # table(JW$TrendStatePrev2[15:nrow(JW)],JW$TrendStatePrev3[15:nrow(JW)])
        # nrow(JW)
        #JW[250:270,]
        
        # UTSMoves<-predict(UTSModel1,newdata = JW)
        # DTSMoves<-predict(DTSModel1,newdata = JW)
        # 
        # UTSMoves<-predict(UTSModel1,newdata = JW)
        # DTSMoves<-predict(DTSModel1,newdata = JW)
        if (grepl("lm",EntryModel$call)) {
          EntryMoves<-predict(EntryModel,newdata = JW)
        } else if (grepl("gbm",EntryModel$call)) {
          EntryMoves<-predict(EntryModel,newdata = JW,n.trees = 100)
        } 
        if (grepl("lm",EntryModel2$call)) {
          EntryMoves2<-predict(EntryModel2,newdata = JW)
        } else if (grepl("gbm",EntryModel2$call)) {
          EntryMoves2<-predict(EntryModel2,newdata = JW,n.trees = 100)
        } 
        if (grepl("lm",ExitModel$call)) {
          ExitMoves<-predict(ExitModel,newdata = JW)
        } else if (grepl("gbm",ExitModel$call)) {
          ExitMoves<-predict(ExitModel,newdata = JW,n.trees = 100)
        } 
        CUTS<-quantile(EntryMoves,probs = c(0+PCUT,1-PCUT),na.rm = T)
        CUTS2<-quantile(EntryMoves2,probs = c(0+PCUT2,1-PCUT2),na.rm = T)
        #PRED<-3*sd(Moves,na.rm = T)
        par(mfrow=c(1,1))
        # plot(density(UTSMoves,na.rm=TRUE))
        # plot(density(DTSMoves,na.rm=TRUE))
        i=nrow(JW)
        
        # wins<-c()
        # for (k in 10:30) {
        # emaSD<-sd(JW$ema8-JW$ema21,na.rm = T)
        dt.i = floor(i/2)-max(which(difftime(JW$TimeStamp[floor(i/2)],JW$TimeStamp[1:floor(i/2)],units = "min")>=30))
        
        #abs(JW$Open[i]-JW$Close[i-dt.i])/JW$Open[i]<0.01
        #difftime(JW$TimeStamp[5],JW$TimeStamp[2],units = "min")
        b.entry<- #ifelse(!is.na(JW$TrendState[1:i]),JW$TrendState[1:i]=="UTS",FALSE) & 
          ifelse(!is.na(EntryMoves[1:i]),EntryMoves[1:i] > CUTS[2],FALSE) & 
          c(rep(FALSE,dt.i),abs(JW$Open[(1+dt.i):i]-JW$Close[1:(i-dt.i)])/JW$Open[i]<0.01) &
          ifelse(!is.na(EntryMoves2[1:i]),EntryMoves2[1:i] > CUTS2[2],FALSE) #&
        #ifelse(!is.na(JW$ema13[1:i]),JW$ema8[1:i] > JW$ema13[1:i],FALSE)
        s.entry<- #ifelse(!is.na(JW$TrendState[1:i]),JW$TrendState[1:i]=="DTS",FALSE) & 
          ifelse(!is.na(EntryMoves[1:i]),EntryMoves[1:i] < CUTS[1],FALSE) &
          c(rep(FALSE,dt.i),abs(JW$Open[(1+dt.i):i]-JW$Close[1:(i-dt.i)])/JW$Open[i]<0.01) &
          ifelse(!is.na(EntryMoves2[1:i]),EntryMoves2[1:i] < CUTS2[1],FALSE) #&
        #ifelse(!is.na(JW$ema13[1:i]),JW$ema8[1:i] < JW$ema13[1:i],FALSE)
        # b.entry<- ifelse(!is.na(JW$TrendState[1:i]),JW$TrendState[1:i]=="UTS",FALSE) & 
        #   ifelse(!is.na(UTSMoves[1:i]),UTSMoves[1:i] > 2.5,FALSE) & 
        #   c(rep(FALSE,dt.i),abs(JW$Open[(1+dt.i):i]-JW$Close[1:(i-dt.i)])/JW$Open[i]<0.01)
        # s.entry<- ifelse(!is.na(JW$TrendState[1:i]),JW$TrendState[1:i]=="UTS",FALSE) & 
        #   ifelse(!is.na(DTSMoves),DTSMoves[1:i] < (-2.5),FALSE) &
        #   c(rep(FALSE,dt.i),abs(JW$Open[(1+dt.i):i]-JW$Close[1:(i-dt.i)])/JW$Open[i]<0.01)
        if (sum(b.entry)==0 | sum(s.entry)==0) {
          #cat("  ",pair,"had zero of either buys or sells ;;; ")
          next
        }
        
        b.tf<-which(b.entry)
        s.tf<-which(s.entry)
        
        b.exit <- #ifelse(!is.na(JW$emacross[1:i]),JW$emacross[1:i],FALSE) | 
          (ifelse(!is.na(ExitMoves[1:i]),ExitMoves[1:i] < (-1*s/100),FALSE) & 
             (1:i - c(rep(0,b.tf[1]-1),rep(b.tf,diff(c(0,b.tf)))))>m+1) |
          c(rep(FALSE,i-1),TRUE)  #c(FALSE,FALSE,diff(b.entry)==-1)[1:i]
        s.exit <- #ifelse(!is.na(JW$emacross[1:i]),JW$emacross[1:i],FALSE) |
          (ifelse(!is.na(ExitMoves[1:i]),ExitMoves[1:i] > (s/100),FALSE) & 
             (1:i - c(rep(0,s.tf[1]-1),rep(s.tf,diff(c(0,s.tf)))))>m+1) |
          c(rep(FALSE,i-1),TRUE) #c(FALSE,FALSE,diff(s.entry)==-1)[1:i]
        #cat(paste(pair,": Entries :",sum(b.entry)+sum(s.entry)," ;;; "))
        if ((sum(b.entry)>0 & sum(b.exit)<1) | (sum(s.entry)>0 & sum(s.exit)<1)) next
        DrawChart(JW[1:i,],SR=FALSE,PlotUFO=FALSE,main=pair)
        entryPreds<-c()
        #s.entryPreds<-c()
        exitPreds<-c()
        #s.exitPreds<-c()
        trades<-c()
        entries<-c()
        exits<-c()
        b.enter=0
        s.enter=0
        ex=0
        for (j in 1:i) {
          if (b.entry[j] & b.enter==0 & s.enter==0) {
            entries<-c(entries,j+1)
            b.enter<-JW$Open[j+1]
            ex<-1
            entryPreds<-c(entryPreds,EntryMoves[j])
          } else if (b.exit[j] & b.enter!=0) {
            trades<-c(trades,ex*(JW$Close[j]-b.enter))
            exits<-c(exits,j)
            b.enter<-0;ex<-0
            exitPreds<-c(exitPreds,ExitMoves[j])
          } else if (b.enter==0 & s.entry[j] & s.enter==0) {
            entries<-c(entries,j+1)
            s.enter<-JW$Open[j+1]
            ex<-(-1)
            entryPreds<-c(entryPreds,EntryMoves[j])
          } else if (s.exit[j] & s.enter!=0) {
            trades<-c(trades,ex*(JW$Close[j]-s.enter))
            exits<-c(exits,j)
            s.enter<-0;ex<-0
            exitPreds<-c(exitPreds,ExitMoves[j])
          } else {
            next
          }
        }
        
        #JW[entries,]
        
        abline(v=JW$TimeStamp[entries],col=4)
        points(JW$TimeStamp[entries],JW$Open[entries],col=4,pch=19)
        abline(v=JW$TimeStamp[exits],col=2)
        points(JW$TimeStamp[exits],JW$Close[exits],col=2,pch=19)
        tr<-data.frame(Pair=pair,enter.index=entries[1:length(exits)],exit.index=exits,
                       entries=JW$TimeStamp[entries[1:length(exits)]],exits=JW$TimeStamp[exits],
                       trades,wins=ifelse(trades>0,"yes","no"),
                       enter.pred=entryPreds[1:length(exits)], exit.pred=exitPreds[1:length(exits)])
        for (j in 1:nrow(tr)) {
          polygon(JW$TimeStamp[c(tr$enter.index[c(j,j)],tr$exit.index[c(j,j)],tr$enter.index[j])],
                  c(JW$Open[tr$enter.index[j]],JW$Close[tr$exit.index[c(j,j)]],JW$Open[tr$enter.index[c(j,j)]]),
                  col=ifelse(tr$trades[j]>0,"green","red"),density = 30)
        }
        # which(b.entry)
        # which(b.exit)
        # which(s.entry)
        # which(s.exit)
        strptime(df.Price$EUR_USD$TimeStamp,format = "%Y-%m-%d %H:%M:%S")
        Trades<-rbind(Trades,data.frame(Pair=pair,enter.index=entries[1:length(exits)],exit.index=exits,
                                        entries=strptime(JW$TimeStamp[entries[1:length(exits)]],format = "%Y-%m-%d %H:%M:%S"),
                                        exits=strptime(JW$TimeStamp[exits],format = "%Y-%m-%d %H:%M:%S"),
                                        trades,wins=ifelse(trades>0,"yes","no"),
                                        enter.pred=entryPreds[1:length(exits)], exit.pred=exitPreds[1:length(exits)]))
        Pips<-rbind(Pips,data.frame(Pair=pair,Trades=length(trades),TotalPips=mult*sum(trades)))
        
      }
      Pips$PipsPerTrade<-Pips$TotalPips/Pips$Trades
      TradeList[[TestName]]<-Trades
      PairPipsList[[TestName]]<-Pips
      TestList[[TestName]]<-colSums(Pips[,c(2,3)])
      #cat("\n**********************************************************\n")
      cat("\nResults: ",TestList[[TestName]][2]," Pips ;;; ",TestList[[TestName]][2]/TestList[[TestName]][1],"Pips per Trade")
      cat("\n****************************************************************************\n")
    }      
    }
  }
}


for (l in 1:length(TestList)) {
  print(TestList[[l]])
}

Outcomes<-aggregate(cbind(TotalPips,Trades)~Pair,data=do.call(rbind,PairPipsList),sum)
Outcomes$PipsPerTrade<-Outcomes$TotalPips/Outcomes$Trades
PairPips<-do.call(rbind,PairPipsList)
table(PairPips$Pair,PairPips$TotalPips>0)

TradeList_Week1<-TradeList
PairPipsList_Week1<-PairPipsList
TradeList_Week2<-TradeList
PairPipsList_Week2<-PairPipsList
TradeList_Week3<-TradeList
PairPipsList_Week3<-PairPipsList

PairPipsList_Week1$P15_M4_S90
PairPipsList_Week2$P15_M4_S90
PairPipsList_Week3$P15_M4_S90

colSums(PairPipsList_Week1$P15_M4_S90[,c(2,3)])
colSums(PairPipsList_Week2$P15_M4_S90[,c(2,3)])
colSums(PairPipsList_Week3$P15_M4_S90[,c(2,3)])

saveRDS(PairPipsList,"FourWeeksTestM34.rds")
saveRDS(PairPipsList,"FourWeeksTestM2.rds")
write.csv(t(sapply(PairPipsList,function(x) colSums(x[,c(2,3)]))),file = "ResultsM2.csv")


#  I KIND OF LIKE THIS Parameter setting...
# ******************************************************************************
#   Now testing 1.5 % and Model looking 4 candles ahead. Score cut at 0.9
# Results:  1376.4  Pips ;;;  9.492414 Pips per Trade
# ****************************************************************************

# ******************************************************************************
# Testing 0.8 % and Model looking 2 candles ahead. Score cut at 0.7, and 0.75
# Results:  718.9  Pips ;;;  23.19032 Pips per Trade
# Results:  652  Pips ;;;  24.14815 Pips per Trade
# ****************************************************************************

# 
# par(mfrow=c(2,2),mar=c(5,4,3,3))
# plot(density(TradeList$P5_M4$enter.pred[TradeList$P5_M4$wins=="yes"]))
# lines(density(TradeList$P5_M4$enter.pred[TradeList$P5_M4$wins=="no"]),col=2)
# 
# plot(TradeList$P5_M4$enter.pred[TradeList$P5_M4$wins=="yes"],TradeList$P5_M4$trades[TradeList$P5_M4$wins=="yes"],ylim=range(TradeList$P5_M4$trades))
# points(TradeList$P5_M4$enter.pred[TradeList$P5_M4$wins=="no"],TradeList$P5_M4$trades[TradeList$P5_M4$wins=="no"],col=2)
# 
# plot(TradeList$P5_M4$exit.pred[TradeList$P5_M4$wins=="yes"],TradeList$P5_M4$trades[TradeList$P5_M4$wins=="yes"],ylim=range(TradeList$P5_M4$trades))
# points(TradeList$P5_M4$exit.pred[TradeList$P5_M4$wins=="no"],TradeList$P5_M4$trades[TradeList$P5_M4$wins=="no"],col=2)
# 
# plot(TradeList$P5_M4$exit.pred[TradeList$P5_M4$wins=="yes"& TradeList$P5_M4$enter.pred>0],TradeList$P5_M4$trades[TradeList$P5_M4$wins=="yes"& TradeList$P5_M4$enter.pred>0],ylim=range(TradeList$P5_M4$trades))
# points(TradeList$P5_M4$exit.pred[TradeList$P5_M4$wins=="no"& TradeList$P5_M4$enter.pred>0],TradeList$P5_M4$trades[TradeList$P5_M4$wins=="no"& TradeList$P5_M4$enter.pred>0],col=2)
# 
# M2<-TestList
# M2.All<-PairPipsList
# 
# # wins<-c(wins,sum(trades))
# # }
# TargetLoop<-function(price, target) {
#   price$target<-ifelse(row.names(price)%in%target,1,0)
#   par(mfrow=c(2,2),mar=c(4,3,3,1)) 
#   for (j in colnames(price)[-c(1,3:21,23:32,35:36,41,ncol(price))]) {
#     plot(density(price[price$target==1,j],na.rm=TRUE),col=4,main=j)
#     lines(density(price[price$target==0,j],na.rm=TRUE),col=2)
#   }
# }
# 
# good.buy.entries <- c(44,45,160,161)
# good.buy.exits <- c(126,127,193,193)
# good.sell.entries <- c(28,29,136,138,193,228)
# good.sell.exits <- c(44,45,160,161,200,259)  
# 
# JW[20:23,]
# 
# TargetLoop(JW,good.sell.exits)
