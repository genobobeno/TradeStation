
source("Initialize.R")
par(mfrow=c(2,1),mar=c(4,4,2,1))



# Look at the last (l=2)+1 rows and look for which currencies are moving most closely with DELTA
  l=2
  j=nrow(PCA_INDEX$Ind.df)-l
  cor(cbind(c(0,0,0,dot.df$ALL)[j:(j+l)],PCA_INDEX$Ind.df[j:(j+l),-1]))

# If LOTS of the currencies indexes are highly correlated, ALL CURRENCIES ARE MOVING
  sum(abs(cor(cbind(c(0,0,0,dot.df$ALL)[j:(j+l)],PCA_INDEX$Ind.df[j:(j+l),-1])))>0.99)-9 

# Can run a test on an arbitrary split
  # for (j in 1:(length(dot.df$ALL)-l)) {
  # l=3;j=200
  # abline(v=PCA_INDEX$Ind.df$TimeStamp[204])
  # l=3;h.test=20
  # PlotIndices(PCA_INDEX$Ind.df,maxpair = F)
  # for (j in 1:680) {
  #   if (sum(abs(cor(cbind(c(0,0,0,dot.df$ALL)[j:(j+l)],PCA_INDEX$Ind.df[j:(j+l),-1])))>0.99,na.rm = T)>h.test) {
  #     print(paste(j,"  :  ",PCA_INDEX$Ind.df$TimeStamp[j]))
  #     abline(v=PCA_INDEX$Ind.df$TimeStamp[j])
  #   }
  # }
  

# Can visualize momentum and PCA on same graph
PlotIndices(PCA_INDEX$Ind.df,maxpair = F)
lines(Ind.dd1$TimeStamp[-c(1:(s-1))],100*dot.df$ALL/max(dot.df$ALL))



par(mfrow=c(1,1))
plot(c(1,100),c(0,150),type="n")
abline(h=31,col=3)
for (i in 1:100) {
  PCA_INDEX<-GetIndexes(Price = GetAllPrices(Minutes = 1,
                                          LookBackHours = 2),pca.plot = FALSE)
  Ind.d1<-data.frame(TimeStamp=PCA_INDEX$Ind.df$TimeStamp[2:nrow(PCA_INDEX$Ind.df)],apply(PCA_INDEX$Ind.df[,2:9],2,diff))
  Ind.dd1<-data.frame(TimeStamp=Ind.d1$TimeStamp[2:nrow(Ind.d1)],apply(Ind.d1[,2:9],2,diff))
  
  r<-list()
  s<-3
  N<-nrow(Ind.dd1)
  cur.dot<-list()
  ps<-mat.or.vec(nr=(N-s+1),nc=(length(inds[-1])*length(inds))/2)
  colnames(ps)<-paste("V",1:((length(inds[-1])*length(inds))/2))
  d<-1
  for (j in inds) {
    for (k in inds[inds!=j]) {
      if (k>j) {
        dotp<-Ind.d1[-1,j]*Ind.d1[-1,k]+Ind.dd1[,j]*Ind.dd1[,k]
        ps[,d]<-rowSums(outer(1:(N-s+1),1:s,function(ii,jj) {dotp[(jj-1)+ii]}))
        colnames(ps)[d]<-paste0(names(Ind.d1)[j],names(Ind.d1)[k])
        d<-d+1
      }
    }
  }
  cur.dot[[paste0("Window",s)]]<-ps
  dot.df<-as.data.frame(cur.dot[[paste0("Window",s)]])
  dot.df$ALL<-apply(dot.df,1,function(x) sqrt(sum(x^2)))
  #plot(Ind.dd1$TimeStamp[-c(1:(s-1))],dot.df$ALL,main="sum")
  #lines(Ind.dd1$TimeStamp[-c(1:(s-1))],dot.df$ALL)
  l=2;j=nrow(PCA_INDEX$Ind.df)-l
  r<-cor(cbind(c(0,0,0,0,dot.df$ALL)[j:(j+l)],PCA_INDEX$Ind.df[j:(j+l),-1]))
  row.names(r)[1]<-colnames(r)[1]<-"ALL"
  print(r)
  points(i,dot.df$ALL[nrow(dot.df)],pch=16)
  r<-cor(cbind(c(0,0,0,dot.df$ALL)[j:(j+l)],
               PCA_INDEX$Ind.df[j:(j+l),-1]))
  row.names(r)[1]<-colnames(r)[1]<-"ALL"
  points(i,sum(abs(r)>0.99,na.rm = T),pch=17,col=4)
  Sys.sleep(60)
}
