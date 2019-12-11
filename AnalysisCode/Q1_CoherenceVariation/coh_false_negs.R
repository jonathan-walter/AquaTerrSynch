## What is the false negative rate for coherences measured between short (20 year) time series?

library(wsyn)

nreps=1000
tmax=20

pvals<-NULL
shifts<-NULL



for(ii in 1:nreps){
  
  tt=1:tmax
  
  env<-rnorm(tmax)
  y1<-arima.sim(tmax, model=list(ar=0.5), innov=env, n.start=1)
  y2<-arima.sim(tmax, model=list(ar=0.5), innov=env, n.start=1)
  
  shift<-round(runif(1,0,2.49))
  #shift=0
  if(shift==0){y2=y1}
  if(shift==1){y2=c(y1[2:tmax],y1[1])}
  if(shift==2){y2=c(y1[3:tmax],y1[1:2])}
  
  y1<-cleandat(y1, tt, clev=2)$cdat
  y2<-cleandat(y2, tt, clev=2)$cdat
  
  tmp<-coh(y1,y2,tt,norm="powall",sigmethod="fast")
  tmp<-bandtest(tmp,c(2,20))
  
  pvals<-c(pvals, tmp$bandp$p_val[1])
  shifts<-c(shifts,shift)
}

false.neg<-sum(pvals>0.05)/nreps
print(false.neg)

#####################################################

nreps=1000
tmax=50

pvals<-NULL
shifts<-NULL



for(ii in 1:nreps){
  
  tt=1:tmax
  
  env<-rnorm(tmax)
  y1<-arima.sim(tmax, model=list(ar=0.5), innov=env, n.start=1)
  y2<-arima.sim(tmax, model=list(ar=0.5), innov=env, n.start=1)
  
  shift<-round(runif(1,0,2.49))
  #shift=0
  if(shift==0){y2=y1}
  if(shift==1){y2=c(y1[2:tmax],y1[1])}
  if(shift==2){y2=c(y1[3:tmax],y1[1:2])}
  
  y1<-cleandat(y1, tt, clev=2)$cdat
  y2<-cleandat(y2, tt, clev=2)$cdat
  
  tmp<-coh(y1,y2,tt,norm="powall",sigmethod="fast")
  tmp<-bandtest(tmp,c(2,20))
  
  pvals<-c(pvals, tmp$bandp$p_val[1])
  shifts<-c(shifts,shift)
}

false.neg<-sum(pvals>0.05)/nreps
print(false.neg)

