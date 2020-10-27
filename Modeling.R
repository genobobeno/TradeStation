

source("Initialize.R")
Training<-readRDS("TrainingData5Weeks.rds")
cor(Training[,names(Training)[sapply(Training,is.numeric)]],use="p")

BuildModelDataSet<-function(objList) {
  price1<-readRDS("Week1.rds")
  price2<-readRDS("Week2.rds")
  price3<-readRDS("Week3.rds")
  price4<-readRDS("Week4.rds")
  price5<-readRDS("Week5.rds")
  # 
  price15<-CombinePriceData(price1,price2)
  price15<-CombinePriceData(price15,price3)
  price15<-CombinePriceData(price15,price4)
  price15f<-CombinePriceData(price15,price5)
  price15p<-ProcessPriceData(price15f)
  Training<-CombineModelData(price = price15p)
  saveRDS(Training,"TrainingData5Weeks.rds")
  readRDS("TrainingData5Weeks.rds")
}

QuantAnalysis<-function(x.price,after=50) {
  library(Hmisc)
  library(tidyr)
  cat("If you give this a {df,list}, it {won't,will} loop over pairs.")
  f.n<-f.c<-list()
  stats<-data.frame(Prediction=character(0),Bin=character(0),Coefficient=numeric(0),P=numeric(0))
  preds<-names(x.price)[!grepl("Open|Close|High|Low|Direc|Time|Move",names(x.price))]
  preds<-preds[!preds %in% c("ema8","ema13","ema21","ema50")]
  for (n in preds) {
    if (is.numeric(x.price[,n])) {
      par(mfrow=c(4,1))
      plot(density(x.price[,n],na.rm = T),main=paste0("Density of ",n))
      x1<-cut2(x.price[,n],g = 10)
      boxplot(x.price$Move3[after:(nrow(x.price)-3)]~x1[after:(nrow(x.price)-3)],main="Delta3 v. Quantiles",xlab=n)
      boxplot(x.price$Move2[after:(nrow(x.price)-3)]~x1[after:(nrow(x.price)-3)],main="Delta2 v. Quantiles",xlab=n)
      boxplot(x.price$Move1[after:(nrow(x.price)-3)]~x1[after:(nrow(x.price)-3)],main="Delta1 v. Quantiles",xlab=n)
      f.n[[n]]<-summary(glm(x.price$Move3[after:(nrow(x.price)-3)]~x1[after:(nrow(x.price)-3)]))
      ROWS<-which(coef(f.n[[n]])[,4]<0.1)
      if (length(ROWS)>0) stats<-rbind(stats,
                                       data.frame(Prediction=n,Bin=names(ROWS),
                                                  Coefficient=coef(f.n[[n]])[ROWS,1],
                                                  P=coef(f.n[[n]])[ROWS,4]))
    } else if (is.character(x.price[,n])) {
      par(mfrow=c(4,1))
      barplot(table(x.price[,n]),main=paste0("Density of ",n))
      boxplot(x.price$Move3[after:(nrow(x.price)-3)]~x.price[after:(nrow(x.price)-3),n],main="Delta3 v. Quantiles",xlab=n)
      boxplot(x.price$Move2[after:(nrow(x.price)-3)]~x.price[after:(nrow(x.price)-3),n],main="Delta2 v. Quantiles",xlab=n)
      boxplot(x.price$Move1[after:(nrow(x.price)-3)]~x.price[after:(nrow(x.price)-3),n],main="Delta1 v. Quantiles",xlab=n)
      f.c[[n]]<-summary(glm(x.price$Move3[after:(nrow(x.price)-3)]~x1[after:(nrow(x.price)-3)]))
      ROWS<-which(coef(f.c[[n]])[,4]<0.1)
      if (length(ROWS)>0) stats<-rbind(stats,data.frame(Prediction=n,Bin=names(ROWS),
                                                        Coefficient=coef(f.c[[n]])[ROWS,1],P=coef(f.c[[n]])[ROWS,4]))
    }
  }
  TS3<-spread(aggregate(Move3~TrendState+TrendStateP1,data=x.price,mean,na.rm=T),TrendState,Move3)
  TS2<-spread(aggregate(Move2~TrendState+TrendStateP1,data=x.price,mean,na.rm=T),TrendState,Move2)
  TS1<-spread(aggregate(Move1~TrendState+TrendStateP1,data=x.price,mean,na.rm=T),TrendState,Move1)
  Model3<-glm(Move3~dema+d2ema+dema8+d2rsi14+d3rsi14+d2cci13+rsi14+cci13+d8Vol+HighWick+LowWick+Delta+emacross+m8dema+m13dema,data=x.price)
  Model2<-glm(Move2~dema+d2ema+dema8+d2rsi14+d3rsi14+d2cci13+rsi14+cci13+d8Vol+HighWick+LowWick+Delta+emacross+m8dema+m13dema,data=x.price)
  Model1<-glm(Move1~dema+d2ema+dema8+d2rsi14+d3rsi14+d2cci13+rsi14+cci13+d8Vol+HighWick+LowWick+Delta+emacross+m8dema+m13dema,data=x.price)
  return(list(Models=list(Move1=Model1,Move2=Model2,Move3=Model3),Num.Fits=f.n,Cat.Fits=f.c,Stats=stats,TrendStates=list(Move1=TS1,Move2=TS2,Move3=TS3)))
}

Training$TrendState<-factor(Training$TrendState)
Training$LegBase<-factor(Training$LegBase)
Training$UpDown<-factor(Training$UpDown)
Training$Formation<-factor(Training$Formation)
Training$RallyDropBase<-factor(Training$RallyDropBase)

SampleCut<-c(FALSE,diff(Training$LegBase=="Leg")[-1]==1,FALSE)
SampleCut<-!is.na(Training$Move3) & Training$Move3>10
sum(SampleCut)/length(SampleCut)
cbind(SampleCut,Training$LegBase=="Leg")  
df.Train.M3<-HighCorrDrop(ModelData = Training[SampleCut,!names(Training) %in% c("Move1","Move2","Move4")])
df1<-df.Train.M3
paste(names(df1),sep="",collapse="+")
cor(df1[,names(df1)[sapply(df1,is.numeric)]],use="p")
#Model.M3<-lm(Move3~TickVolume+TrendState+LegBase*NLegBase+adx8+atr14+d2adx+d2ema+dema8+d3cci13+d3ccrs+HighSqueeze+LowSqueeze+TrendUp+LowWick+HighWick+d8Vol+emacross+m13dema,data=df1)
Model.M3<-lm(Move3~TickVolume+TrendState+NLegBase*Formation+adx14+rsi14+d2rsi14+BBKC+HighSqueeze+emacross+HighWick+PreMove1+PreMove4+ratioD4Vol,data=df1) 
summary(Model.M3)
Output<-GetPredictions(PriceData = Training[Training$LegBase=="Base",],EntryModel = Model.M3)
mean(Output$EntryMoves,na.rm=T)
mean(Output$Move3[Output$EntryMoves<(-5)],na.rm=T)
lines(density(Output$Move3[Output$EntryMoves>5]-Output$EntryMoves[Output$EntryMoves>5],na.rm=T),col=2)
var(Output$Move3,na.rm = T)
var(Output$Move3-Output$EntryMoves,na.rm = T)
#saveRDS(Model.M3,"GLM_M3b.rds")
AIC(Model.M3)
AIC(readRDS("GLM_M3b.rds"))
AIC(readRDS("GLM_M3a.rds"))
AIC(readRDS("GLM_M3.rds"))
# "GLM_M3b.rds"




df.Train.M2<-HighCorrDrop(ModelData = Training[,!names(Training) %in% c("Move1","Move3","Move4")],target="Move2")
df1<-df.Train.M2
paste(names(df1),sep="",collapse="+")
Model.M2<-lm(Move2~TickVolume+TrendState+PrevBaseLow+LegBaseRange+adx14+d2cci13+HighSqueeze+TrendUp+emacross+LowWick+ratioD4Vol+m8dema+m13dema,data=df1)
summary(Model.M2)
AIC(Model.M2)
#saveRDS(Model.M2,"GLM_M2b.rds")
AIC(readRDS("GLM_M2b.rds"))
AIC(readRDS("GLM_M2a.rds"))
AIC(readRDS("GLM_M2.rds"))
# "GLM_M2a.rds" best

df.Train.M4<-HighCorrDrop(ModelData = Training[,!names(Training) %in% c("Move1","Move3","Move2")],target="Move4")
df1<-df.Train.M4
paste(names(df1),sep="",collapse="+")
Model.M4<-lm(Move4~TickVolume+TrendState+PrevBaseLow+LegBaseRange+Formation+adx8+dema+d3ccrs+BBKC+HighSqueeze+LowSqueeze+emacross+LowWick+d4Vol+PreMove1+ratioD4Vol+m8dema+m13dema,data=df1)
summary(Model.M4)
AIC(Model.M4)
#saveRDS(Model.M4,"GLM_M4b.rds")
AIC(readRDS("GLM_M4b.rds"))
AIC(readRDS("GLM_M4a.rds"))
AIC(readRDS("GLM_M4.rds"))
# best "GLM_M4b.rds"

df.Train.M1<-HighCorrDrop(ModelData = Training[,!names(Training) %in% c("Move4","Move3","Move2")],target="Move1")
df1<-df.Train.M1
paste(names(df1),sep="",collapse="+")
cor(df1[,names(df1)[sapply(df1,is.numeric)]],use="p")
sapply(df1,function(x) sum(is.na(x)))
Model.M1<-lm(Move1~TickVolume+TrendState+PrevBaseLow+LegBaseRange+Formation+adx14+lr310+lr310ma+d3rsi14+HighSqueeze+TrendUp+emacross+LowWick+HighWick+PreMove3+ratioD4Vol,data=df1)
summary(Model.M1)
AIC(Model.M1)
#saveRDS(Model.M1,"GLM_M1b.rds")
AIC(readRDS("GLM_M1b.rds"))
AIC(readRDS("GLM_M1a.rds"))
AIC(readRDS("GLM_M1.rds"))
# best GLM_M1a.rds



library(gbm)

df1<-df.Train.M3
df1$TrendState<-factor(df1$TrendState)
df1$LegBase<-factor(df1$LegBase)
names(df1)
Model.gbm.M3<-gbm(Move3~TickVolume+LegBase*NLegBase+dema8+d8Vol+HighWick+LowWick+emacross+TrendState+LowSqueeze+HighSqueeze,data=df1[!is.na(df1$Move3),],n.trees = 500,interaction.depth = 4)
summary(Model.gbm.M3)
gbm.perf(Model.gbm.M3, method = "OOB")
print(pretty.gbm.tree(Model.gbm.M3, i.tree = 1))
saveRDS(Model.gbm.M3,"GBM_M3.rds")

df1<-df.Train.M2
df1$emacross<-factor(df1$emacross)
df1$HighSqueeze<-factor(df1$HighSqueeze)
df1$LowSqueeze<-factor(df1$LowSqueeze)
df1$TrendState<-factor(df1$TrendState)
df1$TrendStateP1<-factor(df1$TrendStateP1)
df1$LegBase<-factor(df1$LegBase)
names(df1)
Model.gbm.M2<-gbm(Move2~TickVolume+LegBase*NLegBase+d2ema+d8Vol+HighWick+LowWick+emacross+TrendState+LowSqueeze+HighSqueeze,data=df1[!is.na(df1$Move2),],n.trees = 500,interaction.depth = 4)
summary(Model.gbm.M2)
gbm.perf(Model.gbm.M2, method = "OOB")
print(pretty.gbm.tree(Model.gbm.M2, i.tree = 1))
saveRDS(Model.gbm.M2,"GBM_M2.rds")

df1<-df.Train.M1
df1$emacross<-factor(df1$emacross)
df1$HighSqueeze<-factor(df1$HighSqueeze)
df1$LowSqueeze<-factor(df1$LowSqueeze)
df1$TrendState<-factor(df1$TrendState)
df1$TrendStateP1<-factor(df1$TrendStateP1)
df1$LegBase<-factor(df1$LegBase)
names(df1)
Model.gbm.M1<-gbm(Move1~TickVolume+LegBase*NLegBase+d2ema+dema8+d3cci13+d3rsi14+d8Vol+LowWick+emacross+TrendState+LowSqueeze+HighSqueeze,data=df1[!is.na(df1$Move1),],n.trees = 500,interaction.depth = 4)
summary(Model.gbm.M1)
gbm.perf(Model.gbm.M1, method = "OOB")
print(pretty.gbm.tree(Model.gbm.M1, i.tree = 1))
saveRDS(Model.gbm.M1,"GBM_M1.rds")

df1<-df.Train.M4
df1$emacross<-factor(df1$emacross)
df1$HighSqueeze<-factor(df1$HighSqueeze)
df1$LowSqueeze<-factor(df1$LowSqueeze)
df1$TrendState<-factor(df1$TrendState)
df1$TrendStateP1<-factor(df1$TrendStateP1)
df1$LegBase<-factor(df1$LegBase)
names(df1)
Model.gbm.M4<-gbm(Move4~TickVolume+LegBase*NLegBase+d2ema+d8Vol+HighWick+LowWick+emacross+TrendState+LowSqueeze+HighSqueeze,data=df1[!is.na(df1$Move4),],n.trees = 500,interaction.depth = 4)
summary(Model.gbm.M4)
gbm.perf(Model.gbm.M4, method = "OOB")
print(pretty.gbm.tree(Model.gbm.M4, i.tree = 1))
saveRDS(Model.gbm.M4,"GBM_M4.rds")




UTSModel<-glm(Move3~dema+d2ema+dema8+d3rsi14+rsi14+cci13+LowWick,data=Training[Training$TrendState=="UTS",])

summary(UTSModel)

saveRDS(UTSModel,"UTSModel.rds")

DTSModel<-glm(Move3~dema+d2ema+d3rsi14+rsi14+cci13+d8Vol+HighWick+LowWick,data=Training[Training$TrendState=="DTS",])

summary(DTSModel)

saveRDS(DTSModel,"DTSModel.rds")

library(gbm)


#########  M4

Training$TrendState<-factor(Training$TrendState)
Training$LegBase<-factor(Training$LegBase)
Training$UpDown<-factor(Training$UpDown)
Training$Formation<-factor(Training$Formation)
Training$RallyDropBase<-factor(Training$RallyDropBase)

TF.ROWS.UP <- c(FALSE,FALSE,  Training$TickVolume[4:N]>900 &
          Training$LegBase[2:(N-2)]=="Base" & 
          Training$LegBase[3:(N-1)]=="Leg" & 
          Training$UpDown[3:(N-1)]=="Up" & 
          Training$UpDown[4:N]=="Up" & 
          0.08893*Training$emaMetric[4:N] - 0.08689*Training$d2emaMetric[4:N] > 0.5)

# TF.ROWS.UP <- c(FALSE,Training$LegBase[1:(n-1)]=="Base" & Training$LegBase[2:n]=="Leg" & 
#                Training$NLegBase[1:(n-1)]>=3 & Training$UpDown[2:n]=="Up")

TrainUp<-Training[TF.ROWS.UP,]
TrainUp$Target<-ifelse(!is.na(TrainUp$Move4) & TrainUp$Move4>10,1,0)
df.Train.M4<-HighCorrDrop(ModelData = TrainUp[,!names(TrainUp) %in% c("Move1","Move2","Move4","Move3")],target = "Target")
df1<-df.Train.M4
paste(names(df1),sep="",collapse="+")
 cor(df1[,names(df1)[sapply(df1,is.numeric)]],use="p")
# sapply(Training,function(x) if (is.factor(x)) table(x))
Model.M4.Up<-glm(Target~adx14+d2adx+demaMetric+HighWick,data=df1) 
summary(Model.M4.Up)
Output<-GetPredictions(PriceData = TrainUp,EntryModel = Model.M4.Up)
plot(density(Output$EntryMoves[Output$Target==1],na.rm=T))
lines(density(Output$EntryMoves[Output$Target==0],na.rm=T))
#saveRDS(Model.M4.Up,"GLM_M4_Up.rds")
AIC(Model.M4.Up)

TF.ROWS.DOWN<-c(FALSE,FALSE,  Training$TickVolume[4:N]>800 &
          Training$LegBase[2:(N-2)]=="Base" & 
          Training$LegBase[3:(N-1)]=="Leg" & 
          Training$UpDown[3:(N-1)]=="Down" & 
          Training$UpDown[4:N]=="Down" & 
          0.08893*Training$emaMetric[4:N] - 0.08689*Training$d2emaMetric[4:N] < (-0.2)) 

# TF.ROWS.DOWN <- c(FALSE,Training$LegBase[1:(n-1)]=="Base" & Training$LegBase[2:n]=="Leg" & 
#                Training$NLegBase[1:(n-1)]>=3 & Training$UpDown[2:n]=="Down")
TrainDown<-Training[TF.ROWS.DOWN,]
TrainDown$Target<-ifelse(!is.na(TrainDown$Move4) & TrainDown$Move4<(-5),1,0)
df.Train.M4<-HighCorrDrop(ModelData = TrainDown[,!names(TrainDown) %in% c("Move1","Move2","Move4","Move3")],target = "Target")
df1<-df.Train.M4
paste(names(df1),sep="",collapse="+")
#cor(df1[,names(df1)[sapply(df1,is.numeric)]],use="p")
Model.M4.Down<-glm(Target~cci13+speed+d3rsi14+d2ccrs+HighWick,data=df1) 
summary(Model.M4.Down)
Output<-GetPredictions(PriceData = Training,EntryModel = Model.M4.Down)
#saveRDS(Model.M4.Down,"GLM_M4_Down.rds")

UpMoves<-predict(Model.M4.Up,newdata = TrainUp) #,n.trees = 100,type = "response")
DownMoves<-predict(Model.M4.Down,newdata = TrainDown) #,n.trees=100,type="response")
ScoreCut<-quantile(UpMoves,p=c(0:99)/100,na.rm = T)
plot(c(0,1),c(0,5),type="n")
for (q in 1:100) {
  ratio<-sum(TrainUp$Move4[UpMoves>ScoreCut[q]]>0,na.rm=T)/sum(TrainUp$Move4[UpMoves>ScoreCut[q]]<0,na.rm=T)
  points(ScoreCut[q],ratio,col=2)
}

ScoreCut<-quantile(DownMoves,p=c(0:99)/100,na.rm = T)
plot(c(0,1),c(0,5),type="n")
for (q in 1:100) {
  ratio<-sum(TrainDown$Move4[DownMoves>ScoreCut[q]]<0,na.rm=T)/sum(TrainDown$Move4[DownMoves>ScoreCut[q]]>0,na.rm=T)
  points(ScoreCut[q],ratio,col=4)
}


########## M3

TrainUp$Target<-ifelse(!is.na(TrainUp$Move3) & TrainUp$Move3>5,1,0)
df.Train.M3<-HighCorrDrop(ModelData = TrainUp[,!names(TrainUp) %in% c("Move1","Move2","Move4","Move3")],target = "Target")
df1<-df.Train.M3
paste(names(df1),sep="",collapse="+")
# cor(df1[,names(df1)[sapply(df1,is.numeric)]],use="p")
# sapply(Training,function(x) if (is.factor(x)) table(x))
Model.M3.Up<-glm(Target~adx8+demaMetric+LowWick+HighWick+PreMove2+ratioD4Vol,data=df1) 
summary(Model.M3.Up)
Output<-GetPredictions(PriceData = TrainUp,EntryModel = Model.M3.Up)
plot(density(Output$EntryMoves[Output$Target==1],na.rm=T))
lines(density(Output$EntryMoves[Output$Target==0],na.rm=T))
#saveRDS(Model.M3.Up,"GLM_M3_Up.rds")
AIC(Model.M3.Up)

TrainDown$Target<-ifelse(!is.na(TrainDown$Move3) & TrainDown$Move3<(-5),1,0)
df.Train.M3<-HighCorrDrop(ModelData = TrainDown[,!names(TrainDown) %in% c("Move1","Move2","Move4","Move3")],target = "Target")
df1<-df.Train.M3
paste(names(df1),sep="",collapse="+")
#cor(df1[,names(df1)[sapply(df1,is.numeric)]],use="p")
Model.M3.Down<-glm(Target~ccrs+d3rsi14+STO+Delta,data=df1) 
summary(Model.M3.Down)
Output<-GetPredictions(PriceData = TrainDown,EntryModel = Model.M3.Down)
#saveRDS(Model.M3.Down,"GLM_M3_Down.rds")

UpMoves<-predict(Model.M3.Up,newdata = TrainUp,n.trees = 100,type = "response")
DownMoves<-predict(Model.M3.Down,newdata = TrainDown,n.trees=100,type="response")
ScoreCut<-quantile(UpMoves,p=c(0:99)/100,na.rm = T)
plot(c(0,1),c(0,5),type="n")
for (q in 1:100) {
  ratio<-sum(TrainUp$Move3[UpMoves>ScoreCut[q]]>0,na.rm=T)/sum(TrainUp$Move3[UpMoves>ScoreCut[q]]<0,na.rm=T)
  points(ScoreCut[q],ratio,col=2)
}

ScoreCut<-quantile(DownMoves,p=c(0:99)/100,na.rm = T)
plot(c(0,1),c(0,5),type="n")
for (q in 1:100) {
  ratio<-sum(TrainDown$Move3[DownMoves>ScoreCut[q]]<(0),na.rm=T)/sum(TrainDown$Move3[DownMoves>ScoreCut[q]]>0,na.rm=T)
  points(ScoreCut[q],ratio,col=2)
}



Fits<-QuantAnalysis(Training)
Fits$Stats[order(Fits$Stats$P),]
summary(Fits$Model)
ROWS<-which(coef(Fits$Num.Fits$NLegBase)[,4]<0.1)
row.names(coef(Fits$Num.Fits$NLegBase))
