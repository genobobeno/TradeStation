
source("Initialize.R")
my.ind<-GetIndexes(HoursBack = 12,minutes = 1)
Ind.d1<-data.frame(TimeStamp=my.ind$Ind.df$TimeStamp[2:nrow(my.ind$Ind.df)],apply(my.ind$Ind.df[,2:9],2,diff))
Ind.dd1<-data.frame(TimeStamp=Ind.d1$TimeStamp[2:nrow(Ind.d1)],apply(Ind.d1[,2:9],2,diff))

r<-list()
S<-c(2,3,4,5)
N<-nrow(Ind.dd1)
cur.dot<-list()
for (s in S) {
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
}
for (s in 2:3) {
  dot.df<-as.data.frame(cur.dot[[paste0("Window",s)]])
  dot.df$ALL<-apply(dot.df,1,function(x) sqrt(sum(x^2)))
  plot(Ind.dd1$TimeStamp[-c(1:(s-1))],dot.df$ALL,main="sum")
  lines(Ind.dd1$TimeStamp[-c(1:(s-1))],dot.df$ALL)
  l=2;j=nrow(my.ind$Ind.df)-l
  sum(abs(cor(cbind(c(0,0,0,dot.df$ALL)[j:(j+l)],my.ind$Ind.df[j:(j+l),-1])))>0.99)
}



PlotIndices(my.ind$Ind.df,maxpair = F)
lines(Ind.dd1$TimeStamp[-c(1:(s-1))],100*dot.df$ALL/max(dot.df$ALL))


# for (j in 1:(length(dot.df$ALL)-l)) {
l=3;j=200
abline(v=my.ind$Ind.df$TimeStamp[204])
l=3;h.test=20
PlotIndices(my.ind$Ind.df,maxpair = F)
for (j in 1:680) {
  if (sum(abs(cor(cbind(c(0,0,0,dot.df$ALL)[j:(j+l)],my.ind$Ind.df[j:(j+l),-1])))>0.99,na.rm = T)>h.test) {
    print(paste(j,"  :  ",my.ind$Ind.df$TimeStamp[j]))
    abline(v=my.ind$Ind.df$TimeStamp[j])
  }
}

par(mfrow=c(1,1))
plot(c(1,100),c(0,150),type="n")
abline(h=31,col=3)
for (i in 1:100) {
  my.ind<-GetIndexes(HoursBack = 4,minutes = 1,pca.plot = FALSE)
  Ind.d1<-data.frame(TimeStamp=my.ind$Ind.df$TimeStamp[2:nrow(my.ind$Ind.df)],apply(my.ind$Ind.df[,2:9],2,diff))
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
  l=2;j=nrow(my.ind$Ind.df)-l
  print(cor(cbind(c(0,0,0,0,dot.df$ALL)[j:(j+l)],my.ind$Ind.df[j:(j+l),-1])))
  points(i,dot.df$ALL[nrow(dot.df)],pch=16)
  points(i,sum(abs(cor(cbind(c(0,0,0,dot.df$ALL)[j:(j+l)],my.ind$Ind.df[j:(j+l),-1])))>0.99,na.rm = T),pch=17,col=4)
  Sys.sleep(60)
}
