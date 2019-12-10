rm(list=ls())

library(lubridate)
library(wsyn)

##Data preparation

dat1<-read.csv("~/Box Sync/NSF EAGER Synchrony/Data/Iowa Lakes Data/DesMoinesRiver_Site1_AllData.csv", stringsAsFactors = F)
dat2<-read.csv("~/Box Sync/NSF EAGER Synchrony/Data/Iowa Lakes Data/ACE_Site1_TempFlow.csv", stringsAsFactors = F)

dat<-rbind(dat1,dat2)

dat$parameter<-rep(NA, nrow(dat))

dat$parameter[dat$PARAM_NUM==4]<-"turbidity_ntu"
dat$parameter[dat$PARAM_NUM==7]<-"tss_mgL"
dat$parameter[dat$PARAM_NUM==13]<-"toc_mgL"
dat$parameter[dat$PARAM_NUM==16]<-"bod_mgL"
dat$parameter[dat$PARAM_NUM==20]<-"nitrate_mgL"
dat$parameter[dat$PARAM_NUM==32]<-"tp_mgL"
dat$parameter[dat$PARAM_NUM==2]<-"flow_"  #Ask Grace about units for flow and temp
dat$parameter[dat$PARAM_NUM==3]<-"temp_"


dat$sampleDate<-as.POSIXct(dat$DATE_OF_SAMPLE, format="%d-%b-%y")

for(ii in 1:nrow(dat)){
  if(year(dat$sampleDate[ii])==2067){year(dat$sampleDate[ii])<-1967}
  if(year(dat$sampleDate[ii])==2068){year(dat$sampleDate[ii])<-1968}
}

params<-c("nitrate_mgL","tss_mgL","flow_","temp_")
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

pdf("~/GitHub/AquaTerrSynch/AnalysisCode/ThinkPiece/DesMoinesAboveSaylorville_selectvars.pdf", width=8.5, height=11)
par(mfrow=c(length(params),1), mar=c(5.1,4.1,2.1,2.1))

for(pp in 1:length(params)){

  plot(param.monthly[pp,], type="l", xlim=c(1,ncol(param.monthly)),
       xlab="Time", ylab=params[pp], xaxt="n",
       main=params[pp])
  axis(1,at=seq(from=1,by=12,length.out=length(years)), labels=years, las=2)

}
dev.off()

yy<-1976:2016

no3<-param.monthly[1,]
no3<-no3[monthyear$Var2 %in% yy]
no3.cln<-cleandat(no3, 1:length(no3), clev=5)$cdat
wt.no3<-wt(no3.cln,1:length(no3.cln))

tss<-param.monthly[2,]
tss<-tss[monthyear$Var2 %in% yy]
tss.cln<-cleandat(tss, 1:length(tss), clev=5)$cdat
wt.tss<-wt(tss.cln,1:length(tss.cln))

flow<-param.monthly[3,]
flow<-flow[monthyear$Var2 %in% yy]
flow.cln<-cleandat(flow, 1:length(flow), clev=5)$cdat
wt.flow<-wt(flow.cln,1:length(flow.cln))

temp<-param.monthly[4,]
temp<-temp[monthyear$Var2 %in% yy]
temp.cln<-cleandat(temp, 1:length(temp), clev=5)$cdat
wt.temp<-wt(temp.cln,1:length(temp.cln))


pdf("~/GitHub/AquaTerrSynch/AnalysisCode/ThinkPiece/DesMoinesAboveSaylorville_selectvars_wts.pdf", width=8.5, height=11)
par(mfrow=c(4,1), mar=c(5.1,4.1,2.1,2.1))

plotmag(wt.no3, title="no3")
plotmag(wt.tss, title="tss")
plotmag(wt.flow, title="flow")
plotmag(wt.temp, title="temp")

dev.off()

plot(wt.no3$timescales, colMeans(Mod(wt.no3$values)^2, na.rm=T)/wt.no3$timescales,
     type="l", xlab="Timescale (yr)", ylab="Wavelet power")
abline(v=5); abline(v=7)
abline(v=10); abline(v=14)
abline(v=42); abline(v=54)
abline(v=120); abline(v=162)

#############################################################################
## Do correlation analyses
acf(no3,lag.max=200)
pacf(no3, lag.max=200)

plot(tss,no3)
cor.test(tss,no3)
ccf(no3,tss, lag.max=200)

plot(temp,no3)
cor.test(temp,no3)
ccf(no3,temp, lag.max=200)

plot(flow,no3)
cor.test(flow,no3)
ccf(no3,flow, lag.max=200)

#############################################################################
## Do coherence analyses

b1<-c(5,7)
b2<-c(10,14)
b3<-c(42,54)
b4<-c(108,192)
b5<-c(16,36)

no3Xtss<-coh(no3.cln,tss.cln,1:length(no3.cln), norm="powall", sigmethod="fast", sigma=1.01)
no3Xtss<-bandtest(no3Xtss,b1)
no3Xtss<-bandtest(no3Xtss,b2)
no3Xtss<-bandtest(no3Xtss,b3)
no3Xtss<-bandtest(no3Xtss,b4)
print(no3Xtss$bandp)


no3Xflow<-coh(no3.cln,flow.cln,1:length(no3.cln), norm="powall", sigmethod="fast", sigma=1.01)
no3Xflow<-bandtest(no3Xflow,b1)
no3Xflow<-bandtest(no3Xflow,b2)
no3Xflow<-bandtest(no3Xflow,b3)
no3Xflow<-bandtest(no3Xflow,b4)
print(no3Xflow$bandp)

plot(no3Xflow$timescales, Mod(no3Xflow$coher))

no3Xtemp<-coh(no3.cln,temp.cln,1:length(no3.cln), norm="powall", sigmethod="fast", sigma=1.01)
no3Xtemp<-bandtest(no3Xtemp,b1)
no3Xtemp<-bandtest(no3Xtemp,b2)
no3Xtemp<-bandtest(no3Xtemp,b3)
no3Xtemp<-bandtest(no3Xtemp,b4)
print(no3Xtemp$bandp)

#####################################################################################
## plotting function--this is modified from the 'wsyn' function by Reuman et al.

plotmag.JW<-function(object,zlims=NULL,neat=TRUE,colorfill=NULL,colorbar=TRUE,title=NULL,filename=NA,xlocs=NULL,ylocs=NULL,xlabs=NULL,ylabs=NULL,...)
{
  wav<-Mod(get_values(object))
  times<-get_times(object)
  timescales<-get_timescales(object)
  
  if(is.null(zlims)){
    zlims<-range(wav,na.rm=T)
  }else
  {
    rg<-range(wav,na.rm=T)
    if (rg[1]<zlims[1] || rg[2]>zlims[2])
    {
      stop("Error in plotmag.tts: zlims must encompass the z axis range of what is being plotted")
    }
  }
  if(neat){
    inds<-which(!is.na(colMeans(wav,na.rm=T)))
    wav<-wav[,inds]
    timescales<-timescales[inds]
  }
  if(is.null(colorfill)){
    jetcolors <- c("#00007F", "blue", "#007FFF", "cyan", 
                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")
    colorfill<-grDevices::colorRampPalette(jetcolors)
  }
  if(is.null(xlocs)){
    xlocs<-pretty(times,n=8)
  }
  if(is.null(ylocs)){
    ylocs<-pretty(timescales,n=8)
  }
  if(is.null(xlabs)){
    xlabs<-xlocs
  }
  if(is.null(ylabs)){
    ylabs<-ylocs
  }
  
  if (!is.na(filename))
  {
    grDevices::pdf(paste0(filename,".pdf"))
  }
  if (!colorbar)
  {
    graphics::image(x=times,y=log2(timescales),z=wav,xlab="",zlim=zlims,
                    ylab="Timescale",axes=F,col=colorfill(100),main=title,...)
    graphics::axis(1,at = xlocs,labels=xlabs)
    graphics::axis(2,at = log2(ylocs),labels = ylabs)
  }else
  {
    fields::image.plot(x=times,y=log2(timescales),z=wav,xlab="",zlim=zlims,
                       ylab="Timescale",axes=F,col=colorfill(100),main=title,...)
    graphics::axis(1,at = xlocs,labels=xlabs)
    graphics::axis(2,at = log2(ylocs),labels = ylabs)
  }
  if (!is.na(filename))
  {
    grDevices::dev.off()
  }
}


############################################################################
## Nice plotting

# will need to modify plotting code from 'wsyn' to fix axes and other figure niceties

#pdf("~/GitHub/AquaTerrSynch/AnalysisCode/ThinkPiece/FigX_AnalysisExample.pdf", width=6.5, height=8)

laymat<-matrix(1,nrow=2,ncol=11)
laymat[1,6:10]<-2
laymat[2,1:5]<-3
laymat[2,6:10]<-4
laymat[1,11]<-6
laymat[2,11]<-5

jetcolors <- c("#00007F", "blue", "#007FFF", "cyan", 
               "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")
colorfill<-grDevices::colorRampPalette(jetcolors)

tiff("~/Box Sync/NSF EAGER Synchrony/Manuscripts/ThinkPiece/dmr_example.tif",
    units="in", width=6.5, height=5, res=300)

layout(laymat)
par(mar=c(2.1,3.5,1.1,1.1),oma=c(2.1,0,0,0),mgp=c(2.2,0.8,0))

plot(no3, type="l", xlab="", ylab=expression("NO"[3]*~(mu*"gL"^-1)), xaxt="n")
axis(1,at=seq(0,500,by=60),labels=seq(1976,2016,by=5))
plot(flow,type="l", xlab="", ylab="Flow (cfs)", xaxt="n")
axis(1,at=seq(0,500,by=60),labels=seq(1976,2016,by=5))

plotmag.JW(wt.no3, xaxs="r", colorbar=F, zlim=c(0,8), ylocs=c(0,6,12,24,48,96,192), ylabs=c(0,0.5,1,2,4,8,16),
           xlocs=seq(0,500,by=60), xlabs=seq(1976,2016,by=5))
plotmag.JW(wt.flow, xaxs="r", colorbar=F, zlim=c(0,8), ylocs=c(0,6,12,24,48,96,192), ylabs=c(0,0.5,1,2,4,8,16),
           xlocs=seq(0,500,by=60), xlabs=seq(1976,2016,by=5))

par(mar=c(2.1,2.1,1.1,1.1))
image(y=seq(0,8,length.out=50),matrix(seq(0,8,length.out=50),nrow=1,ncol=50),xaxt="n",yaxt="n",col=colorfill(50))
axis(2,at=pretty(Mod(c(wt.no3$values,wt.flow$values)),n=5))

mtext("Time",1, outer=T, line=0.65)

dev.off()

