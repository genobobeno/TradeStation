#MomentumPCA

source("Initialize.R")
my.ind<-GetIndexes(HoursBack = 12,minutes = 1)
Ind.d1<-data.frame(TimeStamp=my.ind$Ind.df$TimeStamp[2:nrow(my.ind$Ind.df)],apply(my.ind$Ind.df[,2:9],2,diff))
Ind.dd1<-data.frame(TimeStamp=Ind.d1$TimeStamp[2:nrow(Ind.d1)],apply(Ind.d1[,2:9],2,diff))


#GetIndexes(minutes = 2,t.start = Sys.time()-24*3600,t.stop = Sys.time()-11*3600)
# plot(my.ind$Ind.df$TimeStamp[c(2,nrow(my.ind$Ind.df))], range(apply(my.ind$Ind.df[,2:8],2,diff)),type="n")
# for (j in 2:8) lines(my.ind$Ind.df$TimeStamp[-1], diff(my.ind$Ind.df[,j]))


# plot(range(Ind.d1[,2:9]), range(Ind.dd1[,2:9]),type="n")
# 
# par(mfrow=c(4,2))
# #j=2
# inds<-c(2:9)
# #plot(range(Ind.dd1$TimeStamp),range(my.ind$Ind.df[,2:9]),type="n",main=paste(names(Ind.d1)[j],": Red,   ",names(Ind.d1)[k],": Blue"))
# plot(range(Ind.dd1$TimeStamp),range(my.ind$Ind.df[,2:9]),type="n",main=paste(names(Ind.d1)[j]))
# lines(Ind.dd1$TimeStamp,my.ind$Ind.df[-c(1,2),j],lwd=2,col=2)
# #lines(Ind.dd1$TimeStamp,my.ind$Ind.df[-c(1,2),k],lwd=2,col=4)


r<-list()
S<-c(2,3,4,5)
N<-nrow(Ind.dd1)

p<-mat.or.vec(nr=N,nc=(length(inds[-1])*length(inds))/2)
colnames(p)<-paste("V",1:((length(inds[-1])*length(inds))/2))
d<-1
nrow(Ind.dd1)
for (j in inds) {
  par(mfrow=c(4,2))
  for (k in inds[inds!=j]) {
    if (k>j) {
      print(d)
      plot(Ind.dd1$TimeStamp,Ind.d1[-1,j]*Ind.d1[-1,k]+Ind.dd1[,j]*Ind.dd1[,k],main=names(Ind.d1)[k],type="n")
      lines(Ind.dd1$TimeStamp,Ind.d1[-1,j]*Ind.d1[-1,k]+Ind.dd1[,j]*Ind.dd1[,k],main=names(Ind.d1)[k])
      p[,d]<-Ind.d1[-1,j]*Ind.d1[-1,k]+Ind.dd1[,j]*Ind.dd1[,k]
      colnames(p)[d]<-paste0(names(Ind.d1)[j],names(Ind.d1)[k])
      d<-d+1
    }
  }
}
cor(p)
acf(p[,1])
acf(p[,7])

#p<-mat.or.vec(nr=(N-s+1),nc=length(inds))
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
        # for (n in 1:(N-s+1)) {
        #   #print(paste(n,d,mean(Ind.d1[n:(n+s-1)+1,j]*Ind.d1[n:(n+s-1)+1,k]+Ind.dd1[n:(n+s-1),j]*Ind.dd1[n:(n+s-1),k],na.rm=T)))
        #   ps[n,d]<-mean(Ind.d1[n:(n+s-1)+1,j]*Ind.d1[n:(n+s-1)+1,k]+Ind.dd1[n:(n+s-1),j]*Ind.dd1[n:(n+s-1),k],na.rm = T)
        # }  
        #print(ps[1:5,])
        colnames(ps)[d]<-paste0(names(Ind.d1)[j],names(Ind.d1)[k])
        d<-d+1
      }
    }
    #print(ps[1:5,])
    #print(ps[1:5,])
  }
  cur.dot[[paste0("Window",s)]]<-ps
}
s=2
dot.df<-as.data.frame(cur.dot[[paste0("Window",s)]])
dot.df$ALL<-apply(dot.df,1,function(x) sqrt(sum(x^2)))
plot(Ind.dd1$TimeStamp[-c(1:(s-1))],dot.df[,1],main=colnames(dot.df)[1])

par(mfrow=c(4,2))
for (i in 1:ncol(dot.df)) {
  plot(Ind.dd1$TimeStamp[-c(1:(s-1))],dot.df[,i],main=colnames(dot.df)[i],col=ifelse(dot.df[,i]>0,4,2))
  lines(Ind.dd1$TimeStamp[-c(1:(s-1))],dot.df$ALL/(max(dot.df$ALL))*dot.df[abs(dot.df[,i])==max(abs(dot.df[,i])),i])
}
par(mfrow=c(2,1))
#for (i in 1:ncol(cur.dot$USD_Window5))
plot(Ind.dd1$TimeStamp[-c(1:(s-1))],dot.df$ALL,main="sum")
lines(Ind.dd1$TimeStamp[-c(1:(s-1))],dot.df$ALL)

######## SIMULATION ########### TAKES TIME
dat.y<-density(dot.df$ALL,from = floor(min(dot.df$ALL)),to = ceiling(max(dot.df$ALL)))
par.opt<-data.frame(shp=-101,scl=-101,dif=100000)
for (ii in -100:100) for (jj in -100:100) {
  dat.g<-density(rgamma(5000,shape = 1.6+0.01*ii,scale = 27.5+0.05*jj),from = floor(min(dot.df$ALL)),to = ceiling(max(dot.df$ALL)))
  par.opt<-rbind(par.opt,c(ii,jj,sqrt(sum((dat.y$y[1:105]-dat.g$y[1:105])^2))))
}

#saveRDS(dat.y,"DensityOfWindow2RowSum.rds")
par.opt[which.min(par.opt[,3]),]
plot(density(dot.df$ALL))
#lines(density(rgamma(20000,shape = 1.6+0.01*(-1),scale = 27.5+0.05*(39))),col=2)
gam<-rgamma(20000,shape = 1.9,scale = 45)  #Window 2 ::100 is about 87% of ALL, and 93% of gamma(1.67,26)
dat.g<-density(gam,from = floor(min(dot.df$ALL)),to = ceiling(max(dot.df$ALL)))
lines(density(gam),col=2)
cbind(dat.y$x,cumsum(dat.y$y)/sum(dat.y$y),cumsum(dat.g$y)/sum(dat.g$y))
plot(density(dot.df$ALL*c(1,1+diff(dot.df$ALL)/c(1,diff(dot.df$ALL[-nrow(dot.df)])))))
abline(v=90)
data.frame(TimeStamp = Ind.dd1$TimeStamp[-c(1:(s-1))],dot.df,Lift=dot.df$ALL*c(1,diff(dot.df$ALL)/c(1,1+diff(dot.df$ALL[-nrow(dot.df)]))))[95:200,]


PlotIndices(my.ind$Ind.df,maxpair = F)
lines(Ind.dd1$TimeStamp[-c(1:(s-1))],100*dot.df$ALL/max(dot.df$ALL))


# for (j in 1:(length(dot.df$ALL)-l)) {
l=3;j=200
abline(v=my.ind$Ind.df$TimeStamp[204])
l=3;h.test=13
PlotIndices(my.ind$Ind.df,maxpair = F)
for (j in 1:680) {
  if (sum(cor(cbind(c(0,0,0,dot.df$ALL)[j:(j+l)],my.ind$Ind.df[j:(j+l),-1]))>0.99)>h.test) {
    print(paste(j,"  :  ",my.ind$Ind.df$TimeStamp[j]))
    abline(v=my.ind$Ind.df$TimeStamp[j])
  }
}
  # }
