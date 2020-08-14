
library(combinat)
lowerlims<-c(1.01,1.02,1.03,1.04,1.05)
upperlims<-c(1.07,1.09,1.11,1.13,1.15)
A0<-c(5000,10000,15000)
nWeeks<-50
Wins<-data.frame(nWeeks=nWeeks,lowerBound=lims[1],upperBound=lims[2],accountSize=A0,
           week10=NA,week20=NA,week30=NA,week40=NA,finalWeek=NA)
for (j in 1:length(A0)) {
  for (i in 1:10000) {
    pct<-runif(nWeeks,lims[1],lims[2])
    if (i==1) {
      wins<-matrix(pct,nrow=1,ncol=nWeeks)
    } else {
      wins<-rbind(wins,pct)
    }
  }
  Wins$week10[j]<-mean(apply(wins,1,function(x) A0[j]*prod(x[1:10])))
  Wins$week20[j]<-mean(apply(wins,1,function(x) A0[j]*prod(x[1:20])))
  Wins$week30[j]<-mean(apply(wins,1,function(x) A0[j]*prod(x[1:30])))
  Wins$week40[j]<-mean(apply(wins,1,function(x) A0[j]*prod(x[1:40])))
  Wins$finalWeek[j]<-mean(apply(wins,1,function(x) A0[j]*prod(x)))
  if (j==1) {
    plot(density(apply(wins,1,function(x) A0[j]*prod(x))),xlim=c(0,2000000),col=2)
  } else {
    lines(density(apply(wins,1,function(x) A0[j]*prod(x))),col=1+j)
  }
}
legend("topright",A0,col=c(2,3,4),lty=1)
Wins
