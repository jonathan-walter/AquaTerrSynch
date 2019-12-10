rm(list=ls())

library(lubridate)
library(wsyn)

dat<-read.csv("~/Box Sync/NSF EAGER Synchrony/Data/Iowa Lakes Data/DesMoinesRiver_Site1_AllData.csv", stringsAsFactors = F)

dat$parameter<-rep(NA, nrow(dat))

dat$parameter[dat$PARAM_NUM==4]<-"turbidity_ntu"
dat$parameter[dat$PARAM_NUM==7]<-"tss_mgL"
dat$parameter[dat$PARAM_NUM==13]<-"toc_mgL"
dat$parameter[dat$PARAM_NUM==16]<-"bod_mgL"
dat$parameter[dat$PARAM_NUM==20]<-"nitrate_mgL"
dat$parameter[dat$PARAM_NUM==32]<-"tp_mgL"

dat$sampleDate<-as.POSIXct(dat$DATE_OF_SAMPLE, format="%d-%b-%y")

for(ii in 1:nrow(dat)){
  if(year(dat$sampleDate[ii])==2067){year(dat$sampleDate[ii])<-1967}
  if(year(dat$sampleDate[ii])==2068){year(dat$sampleDate[ii])<-1968}
}

params<-unique(dat$parameter)
years<-1968:2016
months<-1:12
monthyear<-expand.grid(months, years)

param.monthly<-matrix(NA, nrow=length(params), ncol=12*length(years))

for(pp in 1:length(params)){
  for(yy in 1:length(years)){
    for(mm in months){
      param.monthly[pp, which(monthyear$Var1==mm & monthyear$Var2==years[yy])]<-mean(
        dat$SAMPLE_VALUE[dat$parameter==params[pp] & month(dat$sampleDate)==mm & year(dat$sampleDate)==years[yy]])
    }
  }
}

# pdf("~/GitHub/AquaTerrSynch/AnalysisCode/ThinkPiece/DesMoinesAboveSaylorville_allvars.pdf", width=8.5, height=11)
# par(mfrow=c(length(params),1), mar=c(5.1,4.1,2.1,2.1))
# 
# for(pp in 1:length(params)){
#   
#   plot(param.monthly[pp,], type="l", xlim=c(1,ncol(param.monthly)), 
#        xlab="Time", ylab=params[pp], xaxt="n",
#        main=params[pp])
#   axis(1,at=seq(from=1,by=12,length.out=length(years)), labels=years, las=2)
#   
# }
# dev.off()

#forget toc, big gap, possible methods change
turbidity<-param.monthly[1,]
turbidity<-turbidity[monthyear$Var2 %in% 1983:2016]
turbidity.cln<-cleandat(turbidity,1:length(turbidity),clev=1)$cdat
wt.turbidity<-wt(turbidity.cln,1:length(turbidity))

tss<-param.monthly[2,]
tss<-tss[monthyear$Var2 %in% 1974:2016]
tss.cln<-cleandat(tss, 1:length(tss), clev=1)$cdat
wt.tss<-wt(tss.cln,1:length(tss.cln))



bod<-param.monthly[4,]
bod<-bod[monthyear$Var2 %in% 1985:2016]
bod.cln<-cleandat(bod, 1:length(bod), clev=1)$cdat
wt.bod<-wt(bod.cln,1:length(bod.cln))

no3<-param.monthly[5,]
no3<-no3[monthyear$Var2 %in% 1976:2016]
no3.cln<-cleandat(no3, 1:length(no3), clev=1)$cdat
wt.no3<-wt(no3.cln,1:length(no3.cln))

tp<-param.monthly[6,]
tp<-tp[monthyear$Var2 %in% 2000:2016]
tp.cln<-cleandat(tp,1:length(tp),clev=1)$cdat
wt.tp<-wt(tp.cln,1:length(tp.cln))

# pdf("~/GitHub/AquaTerrSynch/AnalysisCode/ThinkPiece/DesMoinesAboveSaylorville_allvars_wts.pdf", width=8.5, height=11)
# par(mfrow=c(5,1), mar=c(5.1,4.1,2.1,2.1))
# 
# plotmag(wt.turbidity, title="turbidity")
# plotmag(wt.tss, title="tss")
# plotmag(wt.bod, title="bod")
# plotmag(wt.no3, title="no3")
# plotmag(wt.tp, title="tp")
# 
# dev.off()
