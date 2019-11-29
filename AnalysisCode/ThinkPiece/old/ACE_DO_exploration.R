dat<-read.csv("~/Box Sync/NSF EAGER Synchrony/Data/Iowa Lakes Data/DO_ACE_AllSites.csv", stringsAsFactors = F)

library(lubridate)

dat$SITE[dat$SITE=="4S"]<-"Saylorville"
dat$SITE[dat$SITE=="8S"]<-"Red Rock"
dat$SITE[dat$SITE=="1"]<-"above Saylorville"
dat$SITE[dat$SITE=="5"]<-"below Saylorville"
dat$SITE[dat$SITE=="7"]<-"above Red Rock"
dat$SITE[dat$SITE=="9"]<-"below Red Rock"

dat$sampleDate<-as.POSIXct(dat$DATE_OF_SAMPLE, format="%d-%b-%y")

for(ii in 1:nrow(dat)){
  if(year(dat$sampleDate[ii])==2067){year(dat$sampleDate[ii])<-1967}
  if(year(dat$sampleDate[ii])==2068){year(dat$sampleDate[ii])<-1968}
}


sampling<-matrix(0, nrow=length(unique(dat$SITE)), ncol=12*length(unique(year(dat$sampleDate))))

sites<-unique(dat$SITE)
years<-unique(year(dat$sampleDate))
months<-1:12

monthyear<-expand.grid(months, years)

for(ss in 1:length(sites)){
  for(yy in 1:length(years)){
    for(mm in months){
      sampling[ss, which(monthyear$Var1==mm & monthyear$Var2==years[yy])]<-length(
        dat$SAMPLE_VALUE[dat$SITE==sites[ss] & month(dat$sampleDate)==mm & year(dat$sampleDate)==years[yy]])
    }
  }
}


pdf("~/GitHub/AquaTerrSynch/AnalysisCode/ThinkPiece/ACE_sampFreq.pdf", width=8.5, height=11)
par(mfrow=c(length(sites),1), mar=c(5.1,4.1,2.1,2.1))

for(ss in 1:length(sites)){

plot(sampling[ss,], type="l", ylim=c(0,max(sampling)), xlim=c(1,ncol(sampling)), 
     xlab="Time", ylab="No. samples by month", xaxt="n",
     main=sites[ss])
axis(1,at=seq(from=1,by=12,length.out=length(years)), labels=years, las=2)

}
dev.off()

###########################################################################
## use the "above Saylorville" site 

dat.sel<-dat[dat$SITE=="above Saylorville",]

DO<-rep(NA, ncol=12*length(unique(year(dat$sampleDate))))

years<-unique(year(dat$sampleDate))
months<-1:12

monthyear<-expand.grid(months, years)

for(yy in 1:length(years)){
  for(mm in months){
    DO[which(monthyear$Var1==mm & monthyear$Var2==years[yy])]<-mean(
      dat$SAMPLE_VALUE[month(dat$sampleDate)==mm & year(dat$sampleDate)==years[yy]])
  }
}

which(is.na(DO))

DO<-DO[monthyear$Var2>1967 & monthyear$Var2<2017]

plot(DO, type="l")

acf(DO)#this just shows the seasonal component

library(wsyn)

DO.cln<-cleandat(DO,1:length(DO),clev=5)$cdat

wt.DO<-wt(DO.cln, 1:length(DO.cln))
plotmag(wt.DO)
plot(wt.DO$timescales/12, colMeans(Mod(wt.DO$values)^2, na.rm=T)/wt.DO$timescales,
     type="l", xlab="Timescale (yr)", ylab="Wavelet power")

        