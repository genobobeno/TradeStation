
source("Initialize.R")
##### Plot Pricing

price1<-readRDS("Week1.rds")
price2<-readRDS("Week2.rds")
price3<-readRDS("Week3.rds")
price4<-readRDS("Week4.rds")
price5<-readRDS("Week5.rds")

t.Start<-Sys.time()-24*14.5*3600
t.Stop<-Sys.time()-24*7.5*3600
CheckTimeTicks(t.start = t.Start,t.stop = t.Stop)
df.Price<-GetAllPrices(t.start = t.Start,t.stop = t.Stop, Minutes = 15)

price15<-CombinePriceData(price1,price2)
price15<-CombinePriceData(price15,price3)
price15<-CombinePriceData(price15,price4)
price15<-CombinePriceData(price15,price5)
price15<-ProcessPriceData(price15)
Training<-CombineModelData(price = price15)

cor(Training[,names(Training)[sapply(Training,is.numeric)]],use="p")
# saveRDS(df.Price,"NiceMorning.rds")
df.Train<-HighCorrDrop(ModelData = Training[,!names(Training) %in% c("Move1","Move2","Move4")])

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

Training<-CombineModelData(price15)
df.Train.M3<-HighCorrDrop(ModelData = Training[,!names(Training) %in% c("Move1","Move2","Move4")])
df1<-df.Train.M3
df1$emacross<-factor(df1$emacross)
df1$HighSqueeze<-factor(df1$HighSqueeze)
df1$LowSqueeze<-factor(df1$LowSqueeze)
df1$TrendState<-factor(df1$TrendState)
df1$TrendStateP1<-factor(df1$TrendStateP1)
df1$LegBase<-factor(df1$LegBase)
df1$TrendUp<-factor(df1$TrendUp)
df1$TrendDown<-factor(df1$TrendDown)
paste(names(df1),sep="",collapse="+")
Model.M3<-lm(Move3~TickVolume+TrendState+LegBase*NLegBase+adx8+atr14+d2adx+d2ema+dema8+d3cci13+d3ccrs+HighSqueeze+LowSqueeze+TrendUp+LowWick+HighWick+d8Vol+emacross+m13dema,data=df1)
summary(Model.M3)
AIC(Model.M3)


df.Train.M2<-HighCorrDrop(ModelData = Training[,!names(Training) %in% c("Move1","Move3","Move4")],target="Move2")
df1<-df.Train.M2
df1$emacross<-factor(df1$emacross)
df1$HighSqueeze<-factor(df1$HighSqueeze)
df1$LowSqueeze<-factor(df1$LowSqueeze)
df1$TrendState<-factor(df1$TrendState)
df1$TrendStateP1<-factor(df1$TrendStateP1)
df1$LegBase<-factor(df1$LegBase)
df1$TrendUp<-factor(df1$TrendUp)
df1$TrendDown<-factor(df1$TrendDown)
paste(names(df1),sep="",collapse="+")
Model.M2<-lm(Move2~TickVolume+LegBase*NLegBase+TrendState+adx5+adx8+atr14+dadx+d2adx+d2ema+BBpctB+HighSqueeze+LowSqueeze+TrendUp+LowWick+HighWick+d8Vol+emacross+m8dema+m13dema,data=df1)
summary(Model.M2)
AIC(Model.M2)

df.Train.M4<-HighCorrDrop(ModelData = Training[,!names(Training) %in% c("Move1","Move3","Move2")],target="Move4")
df1<-df.Train.M4
df1$emacross<-factor(df1$emacross)
df1$HighSqueeze<-factor(df1$HighSqueeze)
df1$LowSqueeze<-factor(df1$LowSqueeze)
df1$TrendState<-factor(df1$TrendState)
df1$TrendStateP1<-factor(df1$TrendStateP1)
df1$LegBase<-factor(df1$LegBase)
df1$TrendUp<-factor(df1$TrendUp)
df1$TrendDown<-factor(df1$TrendDown)
paste(names(df1),sep="",collapse="+")
Model.M4<-lm(Move4~TickVolume+LegBase*NLegBase+TrendState+adx14+dema8+d2ccrs+HighSqueeze+LowSqueeze+TrendUp+Delta+LowWick+HighWick+d8Vol+emacross+m13dema,data=df1)
summary(Model.M4)
AIC(Model.M4)

df.Train.M1<-HighCorrDrop(ModelData = Training[,!names(Training) %in% c("Move4","Move3","Move2")],target="Move1")
df1<-df.Train.M1
df1$emacross<-factor(df1$emacross)
df1$HighSqueeze<-factor(df1$HighSqueeze)
df1$LowSqueeze<-factor(df1$LowSqueeze)
df1$TrendState<-factor(df1$TrendState)
df1$TrendStateP1<-factor(df1$TrendStateP1)
df1$LegBase<-factor(df1$LegBase)
df1$TrendUp<-factor(df1$TrendUp)
df1$TrendDown<-factor(df1$TrendDown)
paste(names(df1),sep="",collapse="+")
Model.M1<-lm(Move1~TickVolume+LegBase*NLegBase+TrendState+dema+d2ema+dema8+d3cci13+d2ccrs+HighSqueeze+LowSqueeze+TrendUp+TrendDown+Delta+LowWick+HighWick+d8Vol,data=df1)
summary(Model.M1)
AIC(Model.M1)

saveRDS(Model.M1,"GLM_M1a.rds")
saveRDS(Model.M2,"GLM_M2a.rds")
saveRDS(Model.M3,"GLM_M3a.rds")
saveRDS(Model.M4,"GLM_M4a.rds")

library(gbm)

df1<-df.Train.M3
df1$emacross<-factor(df1$emacross)
df1$HighSqueeze<-factor(df1$HighSqueeze)
df1$LowSqueeze<-factor(df1$LowSqueeze)
df1$TrendState<-factor(df1$TrendState)
df1$TrendStateP1<-factor(df1$TrendStateP1)
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


Fits<-QuantAnalysis(Training)
Fits$Stats[order(Fits$Stats$P),]
summary(Fits$Model)
ROWS<-which(coef(Fits$Num.Fits$NLegBase)[,4]<0.1)
row.names(coef(Fits$Num.Fits$NLegBase))
