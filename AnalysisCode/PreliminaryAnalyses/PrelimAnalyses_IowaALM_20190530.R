rm(list=ls())

#you all are working on PCs so your file paths will start differently than mine
setwd("~/Box Sync/NSF EAGER Synchrony/")

library(wsyn) #this package is currently on github. To install, run devtools::install_github("reumandc/wsyn")
library(raster) #these packages are on CRAN, install like normal
library(rgdal)
library(aqts)

almdat<-read.csv("./Data/Iowa Lakes Data/Iowa Synchrony Lakes.csv", stringsAsFactors=F)
lakechar<-read.csv("./Data/Iowa Lakes Data/Iowa Lake Characteristics.csv", stringsAsFactors=F)

almdat<-almdat[almdat$LakeID %in% lakechar$LakeID[lakechar$Include=="Y"],]
lakechar<-lakechar[lakechar$Include=="Y",]

IowaALM.raw<-makeIowaALMannualts(almdat,lakechar,getvars=c("Chlorophyll"))
IowaALM.raw<-fixNamestoLAGOS(IowaALM.raw, origin="Iowa ALM")
IowaALM.cln<-cleanAnnualts(IowaALM.raw, ymin=19)

lakechar$lake_area_ha<-lakechar$SA * 100
lakechar$nhd_ftype<-rep(390, nrow(lakechar))

dbuff<-calcBufferDist(sa=lakechar$lake_area_ha, ltype=lakechar$nhd_ftype, minbuff=2500)

accndvi<-stack("/Users/jonathanwalter/Box Sync/NSF EAGER Synchrony/Data/SatelliteData/accvi1989_2018.img")
maxndvi<-stack("/Users/jonathanwalter/Box Sync/NSF EAGER Synchrony/Data/SatelliteData/mxvi1989_2018.img")

nlcdwater<-raster("/Users/jonathanwalter/Box Sync/NSF EAGER Synchrony/Data/SatelliteData/landcover/nlcd2011_cls11.img")
watermask<-nlcdwater<0.05

accndvi<-mask(accndvi,watermask,maskvalue=0) #mask out pixels that have too much water in them
maxndvi<-mask(maxndvi,watermask,maskvalue=0)

analysislakes<-addAVHRRannualts(IowaALM.cln, accndvi, dbuff)
analysislakes<-addAVHRRannualts(analysislakes, maxndvi, dbuff)

source("~/GitHub/AquaTerrSynch/AnalysisCode/bandtest_coh.R")

tsranges<-rbind(c(2,4),c(4,Inf),c(2,Inf))

coh.chlaXaccndvi<-NULL
coh.chlaXmaxndvi<-NULL

for(lind in 1:length(analysislakes$lakedata)){
  lakedat.ii<-cleandat(analysislakes$lakedata[[lind]], as.numeric(colnames(analysislakes$lakedata[[lind]])), clev=5)$cdat
  chlaXaccndvi<-coh(lakedat.ii[1,], lakedat.ii[2,], as.numeric(colnames(analysislakes$lakedata[[lind]])),
                    norm="powall", sigmethod="fast", nrand=10000)
  chlaXmaxndvi<-coh(lakedat.ii[1,], lakedat.ii[3,], as.numeric(colnames(analysislakes$lakedata[[lind]])),
                    norm="powall", sigmethod="fast", nrand=10000)
  for(rind in 1:nrow(tsranges)){
    chlaXaccndvi<-bandtest.coh(chlaXaccndvi, tsranges[rind,])
    chlaXmaxndvi<-bandtest.coh(chlaXmaxndvi, tsranges[rind,])
  }
  coh.chlaXaccndvi<-rbind(coh.chlaXaccndvi, c(t(as.matrix(chlaXaccndvi$bandp[,3:5]))))
  coh.chlaXmaxndvi<-rbind(coh.chlaXmaxndvi, c(t(as.matrix(chlaXmaxndvi$bandp[,3:5]))))
  
}

coh.chlaXaccndvi<-as.data.frame(coh.chlaXaccndvi)
coh.chlaXmaxndvi<-as.data.frame(coh.chlaXmaxndvi)

colnames(coh.chlaXaccndvi)<-paste0("accndvi",c("p.ts1","phi.ts1","coh.ts1","p.ts2","phi.ts2","coh.ts2","p.ts3","phi.ts3","coh.ts3"))
colnames(coh.chlaXmaxndvi)<-paste0("maxndvi",c("p.ts1","phi.ts1","coh.ts1","p.ts2","phi.ts2","coh.ts2","p.ts3","phi.ts3","coh.ts3"))

coh.chlaXaccndvi$lagoslakeid<-analysislakes$lakeinfo$lagoslakeid
coh.chlaXmaxndvi$lagoslakeid<-analysislakes$lakeinfo$lagoslakeid

hist(coh.chlaXaccndvi$accndvicoh.ts1)
hist(coh.chlaXaccndvi$accndvicoh.ts2)

sum(coh.chlaXaccndvi$accndvip.ts1<0.05)/nrow(coh.chlaXaccndvi)
sum(coh.chlaXaccndvi$accndvip.ts2<0.05)/nrow(coh.chlaXaccndvi)

sum(coh.chlaXmaxndvi$maxndvip.ts1<0.05)/nrow(coh.chlaXmaxndvi)
sum(coh.chlaXmaxndvi$maxndvip.ts2<0.05)/nrow(coh.chlaXmaxndvi)

