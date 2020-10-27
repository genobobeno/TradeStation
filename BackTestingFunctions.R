#  Pre-Modeling Analysis
table(Training$TimeStamp)

DefaultPlot<-function() {
  par(mfrow=c(2,2),mar=c(5,4,3,2))
}

ShowCut<-function(CUT,Price,Move=NA,Column=NA,Desc='') {
  if (!is.na(Move)) {
    Column<-paste0("Move",Move)
  } 
  plot(density(Price[!CUT,Column],na.rm=T),xlim=range(Price[!is.infinite(Price[,Column]),Column],na.rm = T),col=2,
       main=paste(Column,Desc))
  lines(density(Price[CUT,Column],na.rm=T),xlim=range(Price[!is.infinite(Price[,Column]),Column],na.rm = T),col=3)
  lines(density(Price[,Column],na.rm=T),col=1)
  cat(Column,"\nMean of CUT:    ",mean(Price[CUT,Column],na.rm=T),"; Uncertainty: ",2*sd(Price[CUT,Column],na.rm=T)/sqrt(sum(CUT,na.rm = T)),"; Sum of Instances: ",sum(CUT,na.rm = T),"\n")
  cat("Mean of Density:",mean(Price[,Column],na.rm=T),"; Uncertainty: ",2*sd(Price[,Column],na.rm=T)/sqrt(nrow(Price)),"; Sum of Instances: ",nrow(Price),"\n")
}

HistCut<-function(CUT,Price,Desc='') {
  COLS<-colnames(Price)[sapply(Price,function(x) length(unique(x))<=21)]
  for (j in COLS) {
    vT<-table(CUT,Price[,j],useNA = "ifany")
    vT[1,]<-vT[1,]/sum(vT[1,])
    vT[2,]<-vT[2,]/sum(vT[2,])
    barplot(vT,main=paste(Desc,j),beside = TRUE,legend=TRUE)
  }
  # cat("\nMean of CUT:    ",mean(Price[CUT,Column],na.rm=T),"; Uncertainty: ",2*sd(Price[CUT,Column],na.rm=T)/sqrt(sum(CUT,na.rm = T)),"; Sum of Instances: ",sum(CUT,na.rm = T),"\n")
  # cat("Mean of Density:",mean(Price[,Column],na.rm=T),"; Uncertainty: ",2*sd(Price[,Column],na.rm=T)/sqrt(nrow(Training)),"; Sum of Instances: ",nrow(Training),"\n")
}


ContourCuts<-function(CUT,Price,ColumnX,ColumnY,XMultiplier=NA,YMultiplier=NA) {
  ContourPlot(var1 = ifelse(is.na(XMultiplier),1,XMultiplier)*Price[CUT & !is.na(Price[,ColumnX]) & !is.na(Price[,ColumnY]),ColumnX], 
              var2 = ifelse(is.na(YMultiplier),1,YMultiplier)*Price[CUT & !is.na(Price[,ColumnX]) & !is.na(Price[,ColumnY]),ColumnY],
              main=paste(ColumnY,"vs",ColumnX,"with CUT"),xlab=ColumnX,ylab=ColumnY,
              xlim=range(ifelse(is.na(XMultiplier),1,XMultiplier)*Price[,ColumnX],na.rm=T),
              ylim=range(ifelse(is.na(YMultiplier),1,YMultiplier)*Price[,ColumnY],na.rm=T))
  ContourPlot(var1 = ifelse(is.na(XMultiplier),1,XMultiplier)*Price[!is.na(Price[,ColumnX]) & !is.na(Price[,ColumnY]),ColumnX], 
              var2 = ifelse(is.na(YMultiplier),1,YMultiplier)*Price[!is.na(Price[,ColumnX]) & !is.na(Price[,ColumnY]),ColumnY],
              main=paste(ColumnY,"vs",ColumnX,"population"),xlab=ColumnX,ylab=ColumnY)
}

CutCycle<-function(Lead=2,Cuts) {
  c(rep(FALSE,Lead))
}

  Training$LegBase[2:(N-2)]=="Base" & 
  Training$LegBase[3:(N-1)]=="Leg" & 
  Training$UpDown[3:(N-1)]=="Up" & 
  Training$UpDown[4:N]=="Up" &
  Training$NLegBase[2:(N-2)]>=2 & 
  Training$d2ema[4:N]>3/10000 & 
  Training$speed[4:N]>500 &
  Training$TickVolume[4:N]>800 &
  0.08893*Training$emaMetric[4:N] - 0.08689*Training$d2emaMetric[4:N] > 1 

Cuts<-c(FALSE,FALSE,FALSE,Training$TickVolume[4:N]>800 &
          Training$LegBase[2:(N-2)]=="Base" & 
          Training$LegBase[3:(N-1)]=="Leg" & 
          Training$UpDown[3:(N-1)]=="Down" & 
          Training$UpDown[4:N]=="Down" & 
          0.08893*Training$emaMetric[4:N] - 0.08689*Training$d2emaMetric[4:N] < (-0.2)) 



par(mfrow=c(3,3))
MoveCut<-ifelse(!is.na(Training$Move4[Cuts]),Training$Move4[Cuts]>8,FALSE)
for (s in colnames(Training)[sapply(Training,is.numeric)]) ShowCut(CUT = MoveCut,Training[which(Cuts)-3,],Column = s,Desc=": Lag 3")
HistCut(CUT = MoveCut,Training[which(Cuts)-3,],Desc=": Lag 3")

N<-nrow(Training)
Cuts<-with(Training,ifelse(!is.na(STO),STO<0.1,FALSE) & 
             c(FALSE,FALSE,FALSE,ifelse(!is.na(m8dema[1:(N-3)]),m8dema[1:(N-3)]>(-0.0008),FALSE)) & # 796, 5.85
             c(FALSE,FALSE,FALSE,ifelse(!is.na(adx14[1:(N-3)]),adx14[1:(N-3)]<48,FALSE)) & # 761, 6.02
             c(FALSE,FALSE,FALSE,ifelse(!is.na(demaMetric[1:(N-3)]),demaMetric[1:(N-3)]>(-90),FALSE)) & # 785, 5.91
             ifelse(!is.na(DChanRange5),DChanRange5>18,FALSE) &
             ifelse(!is.na(d4Vol),d4Vol>0.19,FALSE)  & 
             ifelse(!is.na(d2cci13),d2cci13>(-200),FALSE) &
             c(FALSE,ifelse(!is.na(d3cci13[1:(N-1)]),d3cci13[1:(N-1)]>(-150),FALSE)) &
             c(FALSE,FALSE,ifelse(!is.na(d2ccrs[1:(N-2)]),d2ccrs[1:(N-2)]<2200,FALSE)) & #*******
             c(FALSE,FALSE,ifelse(!is.na(d3rsi14[1:(N-2)]),d3rsi14[1:(N-2)]>(-15),FALSE)) &
             c(FALSE,FALSE,ifelse(!is.na(DChanRange13[1:(N-2)]),DChanRange13[1:(N-2)]>40,FALSE)) &  # ******* 2516,2.27
             c(FALSE,ifelse(!is.na(lr310ma[1:(N-1)]),lr310ma[1:(N-1)]<(-5),FALSE)) ##********  1091, 3.83
)


sum(with(Training,ifelse(!is.na(STO),STO<0.1,FALSE))) 
sum(with(Training,c(FALSE,FALSE,FALSE,ifelse(!is.na(m8dema[1:(N-3)]),m8dema[1:(N-3)]>(-0.0008),FALSE))))  # 796, 5.85
sum(with(Training,c(FALSE,FALSE,FALSE,ifelse(!is.na(adx14[1:(N-3)]),adx14[1:(N-3)]<48,FALSE)))) # 761, 6.02
sum(with(Training,c(FALSE,FALSE,FALSE,ifelse(!is.na(demaMetric[1:(N-3)]),demaMetric[1:(N-3)]>(-90),FALSE)))) # 785, 5.91
sum(with(Training,ifelse(!is.na(DChanRange5),DChanRange5>18,FALSE)))
sum(with(Training,ifelse(!is.na(d4Vol),d4Vol>0.19,FALSE)))
sum(with(Training,ifelse(!is.na(d2cci13),d2cci13>(-200),FALSE)))
sum(with(Training,c(FALSE,ifelse(!is.na(d3cci13[1:(N-1)]),d3cci13[1:(N-1)]>(-150),FALSE))))
sum(with(Training,c(FALSE,FALSE,ifelse(!is.na(d2ccrs[1:(N-2)]),d2ccrs[1:(N-2)]<2200,FALSE))))  #*******
sum(with(Training,c(FALSE,FALSE,ifelse(!is.na(d3rsi14[1:(N-2)]),d3rsi14[1:(N-2)]>(-15),FALSE))))
sum(with(Training,c(FALSE,FALSE,ifelse(!is.na(DChanRange13[1:(N-2)]),DChanRange13[1:(N-2)]>40,FALSE))))  # ******* 2516,2.27
sum(with(Training,c(FALSE,ifelse(!is.na(lr310ma[1:(N-1)]),lr310ma[1:(N-1)]<(-5),FALSE)))) ##********  1091, 3.83



for (pair in names(TestWeeks$W1)) {
  #pair="CHF_JPY"
JW<-CleanOpen(TestWeeks$W1[[pair]])
#JW<-CleanOpen(df.Price[["GBP_JPY"]])
JW<-LabelUpDown(price=JW,multiple.pairs = FALSE)
JW<-LegBaseCount(price=JW)
JW<-BuildHistory(price=JW)
JW<-AppendIndicators(price=JW)
JW<-AppendFeatures(price=JW)
JW$TrendState<-factor(JW$TrendState)
JW$LegBase<-factor(JW$LegBase)
JW$UpDown<-factor(JW$UpDown)
JW$Formation<-factor(JW$Formation)
JW$RallyDropBase<-factor(JW$RallyDropBase)
mult2<-ifelse(floor(JW$Low[1]/10)>1,100,10000)
JW$Move4<-mult2*c(JW$Close[5:nrow(JW)]-JW$Open[5:nrow(JW)-3],NA,NA,NA,NA)
JW$Move3<-mult2*c(JW$Close[4:nrow(JW)]-JW$Open[4:nrow(JW)-2],NA,NA,NA)
JW$Move2<-mult2*c(JW$Close[3:nrow(JW)]-JW$Open[3:nrow(JW)-1],NA,NA)
JW$Move1<-mult2*c(JW$Close[2:nrow(JW)]-JW$Open[2:nrow(JW)],NA)

N<-nrow(JW)

TestCuts<-with(JW,#ifelse(!is.na(STO),STO<0.15,FALSE) & 
                 #c(FALSE,FALSE,FALSE,ifelse(!is.na(m8dema[1:(N-3)]),m8dema[1:(N-3)]>(-0.001),FALSE)) & # 796, 5.85
                 c(FALSE,FALSE,FALSE,ifelse(!is.na(adx14[1:(N-3)]),adx14[1:(N-3)]<50,FALSE)) & # 761, 6.02
                 c(FALSE,FALSE,FALSE,ifelse(!is.na(demaMetric[1:(N-3)]),demaMetric[1:(N-3)]>(-10),FALSE)) & # 785, 5.91
                 ifelse(!is.na(DChanRange5),DChanRange5>12,FALSE) &
                 ifelse(!is.na(d4Vol),d4Vol>0.18,FALSE)  & 
                 ifelse(!is.na(d2cci13),d2cci13>(-150),FALSE) &
                 #c(FALSE,ifelse(!is.na(d3cci13[1:(N-1)]),d3cci13[1:(N-1)]>(-120),FALSE)) &
                 #c(FALSE,FALSE,ifelse(!is.na(d2ccrs[1:(N-2)]),d2ccrs[1:(N-2)]<2000,FALSE)) & #*******
                 #c(FALSE,FALSE,ifelse(!is.na(d3rsi14[1:(N-2)]),d3rsi14[1:(N-2)]>(-15),FALSE)) &
                 c(FALSE,FALSE,ifelse(!is.na(DChanRange13[1:(N-2)]),DChanRange13[1:(N-2)]>15,FALSE)) &#&  # ******* 2516,2.27
                 c(FALSE,ifelse(!is.na(lr310ma[1:(N-1)]),lr310ma[1:(N-1)]<0,FALSE)) ##********  1091, 3.83
)

if (sum(TestCuts,na.rm = T)>1) {
  ShowCut(CUT = TestCuts,JW,Move=4,Desc = pair)
} else {
  print(paste(pair,": No trades"))
  next
}
}

par(mfrow=c(3,3))
MoveCut<-ifelse(!is.na(JW$Move4),JW$Move4>5,FALSE)
for (s in colnames(JW)[sapply(JW,is.numeric)]) ShowCut(CUT = MoveCut,JW[c(1,1,1,1:(N-3)),],Column = s,Desc=": Lag 3")
HistCut(CUT = MoveCut,JW[c(1,1:(N-1)],],Desc=": Lag 1")


Training$TEST<-NULL
aggregate(Move4~TEST,Training,mean)
# & Training$d2cci13[4:N]>0)
DefaultPlot()
ContourCuts(CUT = Cuts,Price = Training,ColumnX = "emaRank",ColumnY = "Move4")#,YMultiplier = 1000)
ShowCut(CUT = Cuts,Training,Column = "TickVolume")
names(Training)
summary(lm(Move4~emaMetric+d2emaMetric,data = Training[Cuts,]))

-3.31 < 0.2577*d2rsi14 + -0.117*CStoch
