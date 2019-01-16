# Apply preliminary analysis workflows to Iowa ALM data

# Preliminary analyses of LAGOS case study lakes using annual timeseries

rm(list=ls())

#you all are working on PCs so your file paths will start differently than mine
setwd("~/Box Sync/NSF EAGER Synchrony/")

library(wsyn) #this package is currently on github. To install, run devtools::install_github("reumandc/wsyn")
library(raster) #these packages are on CRAN, install like normal
library(rgdal)

#The first time you work with LAGOSNE, or if there is a new version released, you will need to run the following lines:
#lagosne_get(version="1.087.1") #They DO NOT need to be run each time.

#may need to change file path to match your own machine
source("~/GitHub/AquaTerrSynch/Code/makeIowaALMannualts.R")
source("~/GitHub/AquaTerrSynch/Code/cleanLAGOSannualts.R")
source("~/GitHub/AquaTerrSynch/Code/makeAVHRRannualts.R")
source("~/GitHub/AquaTerrSynch/Code/AquaTerrSynchPairCoh.R")

almdat<-read.csv("./Iowa Lakes Data/Iowa Synchrony Lakes.csv", stringsAsFactors=F)
lakechar<-read.csv("./Iowa Lakes Data/Iowa Lake Characteristics.csv", stringsAsFactors=F)

almdat<-almdat[almdat$LakeID %in% lakechar$LakeID[lakechar$Include=="Y"],]
lakechar<-lakechar[lakechar$Include=="Y",]

IowaALM.raw<-makeIowaALMannualts(almdat,lakechar,getvars=c("Chlorophyll","TN","TP","Secchi","DO_sat","pH"))

#Load AVHRR data and add time series to lake data
accndvi<-stack("./SatelliteData/accvi8915.img")
maxndvi<-stack("./SatelliteData/mxvi8915.img")
avgndvi<-stack("./SatelliteData/avgvi8915.img")

nlcdwater<-raster("./SatelliteData/landcover/nlcd2011_cls11.img")
watermask<-nlcdwater<0.05 #make mask excluding pixels composed of too much water

accndvi<-mask(accndvi,watermask,maskvalue=0) #apply the mask
maxndvi<-mask(maxndvi,watermask,maskvalue=0)
avgndvi<-mask(avgndvi,watermask,maskvalue=0)

IowaALM.raw<-makeAVHRRannualts(IowaALM.raw, accndvi) #run data processing steps
IowaALM.raw<-makeAVHRRannualts(IowaALM.raw, maxndvi)
IowaALM.raw<-makeAVHRRannualts(IowaALM.raw, avgndvi)

rm(accndvi,maxndvi,avgndvi,watermask)

IowaALM.clean<-cleanLAGOSannualts(IowaALM.raw,ymin=15,maxNA=0.1,timespan=c(1989,2015))

save(IowaALM.cln, file="./Interannual Synchrony/IowaALMdata_20190116.RData")
#load("./Interannual Synchrony/IowaALMdata_20190116.RData")

# ------------------------------------------------------------------
# Produce coherence matrices
st<-c(2,4) #Define a timescale band for the coherence analyses. These are short timescales
lt<-c(4,12) #Others are commented out to preserve what speed we can for this example
at<-c(0,Inf)

cohres.st<-AquaTerrSynchPairCoh(alllakes.cln,band=st)
cohres.lt<-AquaTerrSynchPairCoh(alllakes.cln,band=lt)
cohres.at<-AquaTerrSynchPairCoh(alllakes.cln,band=at)

#again, saving (and loading) objects that take a long time to generate
save(list=c("cohres.lt","cohres.st","cohres.at"),file="./Interannual Synchrony/CoherenceMatrices_IowaALM_20190116.RData")
#load("./Interannual Synchrony/CoherenceMatrices_IowaALM_20190116.RData")

# ------------------------------------------------------------------
# Plot coherence matrices

source("~/GitHub/AquaTerrSynch/Code/plotCoherenceMatrices.R")

# loop through the short timescales stuff
pdf(file="IowaALM_CoherenceMatrices_ts2_4.pdf", onefile=T)
for(lind in 1:length(cohres.st$cohres)){
  plotCoherenceMatrices(cohres.st$cohres[[lind]],
                        title=names(cohres.st$cohres)[lind],
                        filename=NULL)
}
dev.off()

# loop through the long timescales stuff
pdf(file="IowaALM_CoherenceMatrices_ts4_12.pdf", onefile=T)
for(lind in 1:length(cohres.lt$cohres)){
  plotCoherenceMatrices(cohres.lt$cohres[[lind]],
                        title=names(cohres.lt$cohres)[lind],
                        filename=NULL)
}
dev.off()

# loop through the all timescales stuff
pdf(file="IowaALM_CoherenceMatrices_allts.pdf", onefile=T)
for(lind in 1:length(cohres.at$cohres)){
  plotCoherenceMatrices(cohres.at$cohres[[lind]],
                        title=names(cohres.at$cohres)[lind],
                        filename=NULL)
}
dev.off()
