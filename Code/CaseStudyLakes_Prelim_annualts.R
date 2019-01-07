# Preliminary analyses of LAGOS case study lakes using annual timeseries

rm(list=ls())

setwd("~/Box Sync/NSF EAGER Synchrony/")

library(wsyn)

source("~/GitHub/AquaTerrSynch/Code/makeLAGOSannualts.R")
source("~/GitHub/AquaTerrSynch/Code/cleanLAGOSannualts.R")
source("~/GitHub/AquaTerrSynch/Code/makeAVHRRannualts.R")
source("~/GitHub/AquaTerrSynch/Code/makeIowaResannualts.R")

## ----------------------------------------------------------------------
## Data input, formatting, and cleaning


selectIDs<-c(
  122517, #Pepacton reservoir
  25980, #Neversink reservoir
  40610, #Cannonsville reservoir
  4664, #Trout lake
  4722, #Crystal lake
  4625, #Allequash lake
  5248, #Big Muskellunge lake
  906, #Sparkling lake
  447 #Acton lake
)

getvars<-c("chla","colora","no2no3","tn","tp","secchi")

#get the LAGOS data
lagos.raw<-makeLAGOSannualts(selectIDs, getvars, aggfun="gsmean")

#manually add the Iowa data in the same format

redrock.raw<-read.csv("./Iowa Lakes Data/Red Rock ACE.csv")
saylorville.raw<-read.csv("./Iowa Lakes Data/Saylorville ACE.csv")

redrock.ann<-makeIowaResAnnualts(redrock.raw)
saylorville.ann<-makeIowaResAnnualts(saylorville.raw)

iowa.inf<-data.frame(lagoslakeid=rep(NA,2),
                     gnis_name=c("Red Rock Reservoir","Saylorville Reservoir"),
                     nhd_lat=c(41.373941,41.708014),
                     nhd_long=c(-92.982938,-93.683214),
                     start=rep(1980,2),
                     end=rep(2016,2)
                     )

alllakes.raw<-lagos.raw
alllakes.raw$lakeinfo<-rbind(lagos.raw$lakeinfo, iowa.inf)
alllakes.raw$lakedata[['Red Rock Reservoir']]<-redrock.ann
alllakes.raw$lakedata[['Saylorville Reservoir']]<-saylorville.ann

#Load AVHRR data and add time series to lake data
accndvi<-stack("./SatelliteData/accvi8915.img")
maxndvi<-stack("./SatelliteData/mxvi8915.img")
avgndvi<-stack("./SatelliteData/avgvi8915.img")

nlcdwater<-raster("./SatelliteData/landcover/nlcd2011_cls11.img")
watermask<-nlcdwater<0.05

accndvi<-mask(accndvi,watermask,maskvalue=0) #mask out pixels that have too much water in them
maxndvi<-mask(maxndvi,watermask,maskvalue=0)
avgndvi<-mask(avgndvi,watermask,makevalue=0)

alllakes.raw<-makeAVHRRannualts(alllakes.raw, accndvi)


#Data filtering
#alllakes.cln<-cleanLAGOSannualts(alllakes.raw)

