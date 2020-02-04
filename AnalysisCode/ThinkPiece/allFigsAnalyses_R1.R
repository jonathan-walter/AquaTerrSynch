# Analysis and Figures for Wilkinson et al. (2020) in Limnology and Oceanography Letters
# 'Beyond the trends: The need to understand multi-annual dynamics in aquatic ecosystems'

rm(list=ls())

if (!require(wsyn)) install.packages('wsyn')
library(wsyn)

##--------------------------------------------------------------
## Figure 1: Des Moines River Nitrate

# Misc objects for ease of analysis
yy<-1976:2016

dec.time<-seq(1976,2017,by=1/12)
dec.time<-dec.time[-length(dec.time)]

# Read in the Data from EDI:
# Package ID: edi.451.2 Cataloging System:https://pasta.edirepository.org.
# Data set title: Monthly nitrate concentrations in the Des Moines River above Saylorville Reservoir, 1976 - 2016.
# Data set creator:  Grace Wilkinson - Iowa State University 
# Data set creator:  William Crumpton - Iowa State University 
# Data set creator:  Chris Rehmann - Iowa State University 
# Data set creator:  Benjamin Noack - Iowa State University 
# Data set creator:  Elena Sandry - Iowa State University 
# Contact:  Grace Wilkinson -  Iowa State University  - wilkinso@iastate.edu
# Stylesheet v2.7 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/451/2/38b4289ab21b71cbea87b8e3400a3670" 
infile1 <- tempfile()
download.file(inUrl1,infile1,method="curl")

dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "Year",     
                 "Month",     
                 "no3_mgL",     
                 "flow_cfs"    ), check.names=TRUE)


# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
if (class(dt1$Month)!="factor") dt1$Month<- as.factor(dt1$Month)
if (class(dt1$no3_mgL)=="factor") dt1$no3_mgL <-as.numeric(levels(dt1$no3_mgL))[as.integer(dt1$no3_mgL) ]
if (class(dt1$flow_cfs)=="factor") dt1$flow_cfs <-as.numeric(levels(dt1$flow_cfs))[as.integer(dt1$flow_cfs) ]



## Wavelet transforms
no3<-dt1$no3_mgL
no3.cln<-cleandat(no3, 1:length(no3), clev=5)$cdat
wt.no3<-wt(no3.cln,1:length(no3.cln))

flow<-dt1$flow_cfs
flow.cln<-cleandat(flow, 1:length(flow), clev=5)$cdat
wt.flow<-wt(flow.cln,1:length(flow.cln))


## Coherence testing
b1<-c(5,7)
b2<-c(10,14)
#b3<-c(42,54)
b4<-c(108,192)
b5<-c(16,36)
b3<-c(36,60)

no3Xflow<-coh(no3.cln,flow.cln,1:length(no3.cln), norm="powall", sigmethod="fast", sigma=1.01)
no3Xflow<-bandtest(no3Xflow,b1)
no3Xflow<-bandtest(no3Xflow,b2)
no3Xflow<-bandtest(no3Xflow,b3)
no3Xflow<-bandtest(no3Xflow,b4)
print(no3Xflow$bandp)

## trend from 1980:1990
lmx<-lm(no3[dec.time>=1981&dec.time<1990]~dec.time[dec.time>=1981&dec.time<1990])
summary(lmx)
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
                    ylab="Timescale (years)",axes=F,col=colorfill(100),main=title,...)
    graphics::axis(1,at = xlocs,labels=xlabs)
    graphics::axis(2,at = log2(ylocs),labels = ylabs)
  }else
  {
    fields::image.plot(x=times,y=log2(timescales),z=wav,xlab="",zlim=zlims,
                       ylab="Timescale (years)",axes=F,col=colorfill(100),main=title,...)
    graphics::axis(1,at = xlocs,labels=xlabs)
    graphics::axis(2,at = log2(ylocs),labels = ylabs)
  }
  if (!is.na(filename))
  {
    grDevices::dev.off()
  }
}

## Nice plotting

laymat<-matrix(1,nrow=2,ncol=11)
laymat[1,6:10]<-2
laymat[2,1:5]<-3
laymat[2,6:10]<-4
laymat[1,11]<-6
laymat[2,11]<-5

jetcolors <- c("#00007F", "blue", "#007FFF", "cyan", 
               "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")
colorfill<-grDevices::colorRampPalette(jetcolors)

tiff("Figure1_Wilkinson_etal_2020_LOLetters.tif",
     units="in", width=6.5, height=5, res=300)

layout(laymat)
par(mar=c(2.1,3.5,1.1,1.1),oma=c(2.1,0,0,0),mgp=c(2.2,0.8,0))

plot(no3, type="l", xlab="", ylab=expression("NO"[3]*~(mg~L^-1)), col="gray40", xaxt="n")
lines((1:length(no3))[dec.time>=1981&dec.time<1990],
      predict(lmx,data.frame(dec.time[dec.time>=1981&dec.time<1990])),col="red", lwd=2)
axis(1,at=seq(0,500,by=60),labels=seq(1976,2016,by=5))
legend("topleft", legend="A", bty='n')

plot(flow,type="l", xlab="", ylab="Flow (cfs)", xaxt="n", col="gray40")
axis(1,at=seq(0,500,by=60),labels=seq(1976,2016,by=5))
legend("topleft", legend="B", bty='n')

plotmag.JW(wt.no3, xaxs="r", colorbar=F, zlim=c(0,8), ylocs=c(0,6,12,24,48,96,192), ylabs=c(0,0.5,1,2,4,8,16),
           xlocs=seq(0,500,by=60), xlabs=seq(1976,2016,by=5))
legend("topleft", legend="C", bty='n')

plotmag.JW(wt.flow, xaxs="r", colorbar=F, zlim=c(0,8), ylocs=c(0,6,12,24,48,96,192), ylabs=c(0,0.5,1,2,4,8,16),
           xlocs=seq(0,500,by=60), xlabs=seq(1976,2016,by=5))
legend("topleft", legend="D", bty='n')

par(mar=c(2.1,2.1,1.1,1.1))
image(y=seq(0,8,length.out=50),matrix(seq(0,8,length.out=50),nrow=1,ncol=50),xaxt="n",yaxt="n",col=colorfill(50))
axis(2,at=pretty(Mod(c(wt.no3$values,wt.flow$values)),n=5))

mtext("Time",1, outer=T, line=0.65)

dev.off()


##--------------------------------------------------------------
## Figure 2: Pedagogical examples

## 1. a real-ish timeseries with multi-annual dynamics

tmax=30
res=0.01

tt<-seq(1,tmax,by=res)

#trend component
b1<-0.075
y1<-b1*tt-mean(b1*tt)
plot(y1,type="l")

#seasonal oscillation
p1<-0.5
y2<-sin(seq(0,2*pi*(length(tt)/(p1/res)),length.out=length(tt)))
plot(tt,y2, type="l")

#4-year oscillation
p2<-11
y3<-sin(seq(0,2*pi*(length(tt)/(p2/res)),length.out=length(tt)))*0.7
plot(tt,y3, type="l")

#white noise
sig<-0.2
y4<-rnorm(length(tt),0,sig)

plot(tt,y1+y2+y3+y4, type="l")

ycomp<-y1+y2+y3+y4


lm1<-lm(ycomp[tt>=5 & tt<=10] ~ tt[tt>=5 & tt<=10])
lm2<-lm(ycomp[tt>=12.5 & tt <=17.5] ~ tt[tt>=12.5 & tt<=17.5])
lm3<-lm(ycomp[tt>=20 & tt<=25] ~ tt[tt>=20 & tt<=25])


tiff("Figure2_Wilkinson_etal_2020_LOLetters.tif", units="in",
    height=5, width=3.25, res=300)

par(mfrow=c(4,1), mar=c(2,2,0,1), xpd=F, oma=c(2,2,1,0), mgp=c(1,1,0))
plot(tt,y1+y2+y3+y4, type="l",xaxt="n", xlab="", yaxt="n", ylab="Integrated",ylim=c(-3,3))
rect(5,-4,10,4, col="skyblue2", xpd=F, border=NA)
rect(12.5,-4,17.5,4,col="darkseagreen2",xpd=F,border=NA)
rect(20,-4,25,4,col="wheat",xpd=F,border=NA)
lines(tt,y1+y2+y3+y4)
lines(tt[tt>=5 & tt<=10], predict(lm1), col="steelblue4", lwd=3)
lines(tt[tt>=12.5 & tt<=17.5], predict(lm2), col="seagreen4", lwd=3)
lines(tt[tt>=20 & tt<=25], predict(lm3), col="goldenrod4", lwd=3)
axis(1,at=c(1,5,10,15,20,25,30),labels=FALSE)
axis(2,at=pretty(ycomp,5),labels=FALSE, xpd=F)
text(0.55,2.7,"A")

plot(tt,y2,type="l",xaxt="n",xlab="", yaxt="n", ylab="Seasonal",ylim=c(-3,3))
axis(1,at=c(1,5,10,15,20,25,30),labels=FALSE)
axis(2,at=pretty(ycomp,5),labels=FALSE,xpd=F)
text(0.55,2.7,"B")

plot(tt,y3,type="l",xaxt="n",xlab="", yaxt="n", ylab="Multi-annual",ylim=c(-3,3))
axis(1,at=c(1,5,10,15,20,25,30),labels=FALSE)
axis(2,at=pretty(ycomp,5),labels=FALSE,xpd=F)
text(0.55,2.7,"C")

plot(tt,y1,type="l",xaxt="n",xlab="", yaxt="n", ylab="Long-term trend",ylim=c(-3,3))
axis(1,at=c(1,5,10,15,20,25,30),labels=TRUE)
axis(2,at=pretty(ycomp,5),labels=FALSE,xpd=F)
text(0.55,2.7,"D")

mtext("Timestep",1,outer=T,cex=0.8,line=0.4)
mtext("Primary productivity",2,outer=T,cex=0.8,line=0.5)

dev.off()
