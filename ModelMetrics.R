
GetPredictions<-function(PriceData,EntryModel,ExitModel=NA,ExitTarget=NA) {
  # PriceData<-Training[Training$LegBase=="Base",];EntryModel = Model.M3
  if (!"emacross" %in% names(PriceData) && is.factor(PriceData$emacross)) {
    PriceData<-CleanOpen(PriceData)
    PriceData<-LabelUpDown(price=PriceData,multiple.pairs = FALSE)
    PriceData<-LegBaseCount(price=PriceData)
    PriceData<-BuildHistory(price=PriceData)
    PriceData<-AppendIndicators(price=PriceData)
    PriceData<-AppendFeatures(price=PriceData)
    PriceData$TrendState<-factor(PriceData$TrendState)
    PriceData$LegBase<-factor(PriceData$LegBase)
    PriceData$emacross<-factor(PriceData$emacross)
    PriceData$LegBase<-factor(PriceData$LegBase)
    PriceData$UpDown<-factor(PriceData$UpDown)
    PriceData$Formation<-factor(PriceData$Formation)
    PriceData$RallyDropBase<-factor(PriceData$RallyDropBase)
    mult<-ifelse(floor(PriceData$Low[1]/10)>1,0.001,0.00001)
    mult2<-ifelse(floor(PriceData$Low[1]/10)>1,100,10000)
    PriceData$Move4<-mult2*c(PriceData$Close[5:nrow(PriceData)]-PriceData$Open[5:nrow(PriceData)-3],NA,NA,NA,NA)
    PriceData$Move3<-mult2*c(PriceData$Close[4:nrow(PriceData)]-PriceData$Open[4:nrow(PriceData)-2],NA,NA,NA)
    PriceData$Move2<-mult2*c(PriceData$Close[3:nrow(PriceData)]-PriceData$Open[3:nrow(PriceData)-1],NA,NA)
    PriceData$Move1<-mult2*c(PriceData$Close[2:nrow(PriceData)]-PriceData$Open[2:nrow(PriceData)],NA)
    #   Nnf<-(0+!is.na(EntryModel)[1])+(!is.na(ExitModel)[1]+0)
    #   nf<-layout(matrix(c(1:(2+Nnf)),nrow=(2+Nnf),ncol=1),1,c(1,rep(1,Nnf),3))
  }
  if (grepl("lm",EntryModel$call)) {
    PriceData$EntryMoves<-predict(EntryModel,newdata = PriceData)
  } else if (grepl("gbm",EntryModel$call)) {
    PriceData$EntryMoves<-predict(EntryModel,newdata = PriceData,n.trees = 100)
  }
  if (!is.na(ExitModel)[1] && grepl("lm",ExitModel$call)) {
    PriceData$ExitMoves<-predict(ExitModel,newdata = PriceData)
  } else if (!is.na(ExitModel)[1] && grepl("gbm",ExitModel$call)) {
    PriceData$ExitMoves<-predict(ExitModel,newdata = PriceData,n.trees = 100)
  }
  PriceData  
}

par(mfrow=c(2,2))
plot(density(Output$EntryMoves,na.rm=T),xlim=c(-22,25))
lines(density(Output$Move4,na.rm=T),col=2)
plot(density(Output$ExitMoves,na.rm=T),xlim=c(-20,20))
lines(density(Output$Move3,na.rm=T),col=2)
plot(Output$EntryMoves,Output$Move4)
plot(Output$ExitMoves,Output$Move3)
cor(Output$EntryMoves[which(Output$LegBase=="Leg")[-1]-1],Output$Move4[which(Output$LegBase=="Leg")[-1]-1],use = "p")
cor(Output$ExitMoves[which(Output$LegBase=="Leg")[-1]-1],Output$Move3[which(Output$LegBase=="Leg")[-1]-1],use = "p")

plot(density(Output$Move4 - Output$EntryMoves,na.rm = T))
lines(density(Output$Move4[which(Output$EntryMoves>0.5 & Output$LegBase=="Leg")[-1]-1] - Output$EntryMoves[which(Output$EntryMoves>0.5 & Output$LegBase=="Leg")[-1]-1],na.rm = T),col=4)
lines(density(Output$Move4,na.rm = T),col=2)
plot(density(Output$Move4 - Output$EntryMoves,na.rm = T))
lines(density(Output$Move4[which(Output$EntryMoves<(-0.8) & Output$LegBase=="Leg")[-1]-1] - Output$EntryMoves[which(Output$EntryMoves<(-0.8) & Output$LegBase=="Leg")[-1]-1],na.rm = T),col=4)
lines(density(Output$Move4,na.rm = T),col=2)


PlotPredictions<-function(PriceData,EntryModel,EntryTarget,ExitModel=NA,ExitTarget=NA,...) {
  YLIM = c(-1,1)*1.2*max(abs(PriceData[,EntryTarget]),na.rm = T)
  par(mar=c(0,3,0,1))
  plot(XLIM,YLIM,type="n",ylim=YLIM,xlim=XLIM,xaxt="n")
  rect(PriceData$TimeStamp-CSpace,ifelse(PriceData[,EntryTarget]>0,0,PriceData[,EntryTarget]),
       PriceData$TimeStamp+CSpace,ifelse(PriceData[,EntryTarget]>0,PriceData[,EntryTarget],0),
       col=ifelse(PriceData[,EntryTarget]>0,"green","red"),border = NA)
  TF <- !is.na(EntryMoves*PriceData[,EntryTarget])
  points(PriceData$TimeStamp[TF],EntryMoves[TF],pch=16,col=c(1,7)[1+(EntryMoves[TF]*PriceData[TF,EntryTarget]>0)]) #,col=c(1,7)[1+(EntryMoves*PriceData[,EntryTarget])>1])

  YLIM = c(-1,1)*1.2*max(abs(PriceData[,ExitTarget]),na.rm = T)
  par(mar=c(0,3,0,1))
  plot(XLIM,YLIM,type="n",ylim=YLIM,xlim=XLIM,xaxt="n")
  rect(PriceData$TimeStamp-CSpace,ifelse(PriceData[,ExitTarget]>0,0,PriceData[,ExitTarget]),
       PriceData$TimeStamp+CSpace,ifelse(PriceData[,ExitTarget]>0,PriceData[,ExitTarget],0),
       col=ifelse(PriceData[,ExitTarget]>0,"green","red"),border = NA)
  TF<-!is.na(ExitMoves*PriceData[,EntryTarget])
  points(PriceData$TimeStamp[TF],ExitMoves[TF],pch=16,col=c(1,7)[(ExitMoves[TF]*PriceData[TF,ExitTarget]>0)+1]) #,col=c(1,7)[1+(EntryMoves*PriceData[,EntryTarget])>1])
}
