CreateCutPoints<-function(PriceList,Model,PCut=c(50,75,100,125,150,175,200,225,250)/10) {
  CUTS<-list()
  for (pair in PAIRS) {
    cat(pair,";")
    Moves<-c()
    for (w in 1:6) {
      JW<-CleanOpen(PriceList[[paste0("W",w)]][[pair]])
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
      mult<-ifelse(JW$Close[1]/10>1,100,10000)
      if (grepl("lm",Model$call)) {
        Moves<-c(Moves,predict(Model,newdata = JW))
      } else if (grepl("gbm",Model$call)) {
        Moves<-c(Moves,predict(Model,newdata = JW,n.trees = 100,type="response"))
      } 
    }
    CUTS[[pair]]<-quantile(Moves,probs = sort(c(0+PCut/100,1-PCut/100)),na.rm = T)
  }
  CUTS
}


HighCorrDrop<-function(ModelData,r.max = 0.54773, target="Move3",rowLimit=10000) {
  # ModelData<-Training[,!names(Training) %in% c("Move1","Move2","Move4")];r.max = 0.54773; target="target";rowLimit=10000
  TF<-sapply(ModelData,is.numeric)
  rdf<-ModelData[,names(ModelData)[TF]]
  if (nrow(ModelData)>rowLimit) rdf<-rdf[sample(row.names(rdf),size=rowLimit),]
  tdf<-rdf[,target]
  rdf<-rdf[,names(rdf)!=target]
  nTests = sum(TF)^2
  sTest<-0
  s_old <- -1
  rVars<-names(rdf)
  cols2Kill<-c()
  r.t = unlist(sapply(rdf, function(x) (cor(tdf, x, use = "pairwise.complete.obs"))))
  r.t<-r.t[!is.na(r.t)]
  vOrder = names(r.t[order(-abs(r.t))])
  i<-1
  while ( sTest[length(sTest)] < nTests & sum(sTest == s_old) < 20 & i<length(vOrder)) {
    if (i%%10==0) s_old<-sTest[i-1]
    s<-vOrder[i]
    print(s)
    var = rdf[,s]
    rt<-cor(var,tdf,use="p")
    r.v = unlist(sapply(rdf, function(x) (cor(var, x, use = "pairwise.complete.obs"))))
    r<-merge(data.frame(N1 = names(r.t)[order(-abs(r.t))],RT = r.t[order(-abs(r.t))]),data.frame(N1 = names(r.v),R1 = r.v),by="N1")
    r$R1<-abs(r$R1)
    TF<-r$R1>r.max
    cols2Kill1<-r$N1[r$RT<rt & TF]
    cols2Kill1<-cols2Kill1[!is.na(cols2Kill1)]
    sTest<-c(sTest,sTest[i]+length(rdf)*sum(r$RT<rt & TF,na.rm = TRUE))
    vOrder<-vOrder[!vOrder %in% cols2Kill1]
    cols2Kill<-c(cols2Kill,cols2Kill1[cols2Kill1 != target])
    i<-i+1
    cat(paste0(i,":",sTest[length(sTest)],":",nTests,";  "))
  }
  ModelData[,unique(cols2Kill)]<-list(NULL)
  ModelData
}
