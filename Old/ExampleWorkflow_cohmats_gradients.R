# Preliminary analyses of LAGOS case study lakes using annual timeseries

rm(list=ls())

#you all are working on PCs so your file paths will start differently than mine
setwd("~/Box Sync/NSF EAGER Synchrony/")

library(wsyn) #this package is currently on github. To install, run devtools::install_github("reumandc/wsyn")
library(raster) #these packages are on CRAN, install like normal
library(rgdal)
library(LAGOSNE)

#The first time you work with LAGOSNE, or if there is a new version released, you will need to run the following lines:
#lagosne_get(version="1.087.1") #They DO NOT need to be run each time.

source("~/GitHub/AquaTerrSynch/Code/makeLAGOSannualts.R") #may need to change file path to match your own machine
source("~/GitHub/AquaTerrSynch/Code/cleanLAGOSannualts.R")
source("~/GitHub/AquaTerrSynch/Code/makeAVHRRannualts.R")
source("~/GitHub/AquaTerrSynch/Code/makeIowaResannualts.R")
source("~/GitHub/AquaTerrSynch/Code/AquaTerrSynchPairCoh.R")

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
lagos.raw<-makeLAGOSannualts(selectIDs, getvars, aggfun="gsmean", timespan=NULL)

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
watermask<-nlcdwater<0.05 #make mask excluding pixels composed of too much water

accndvi<-mask(accndvi,watermask,maskvalue=0) #apply the mask
maxndvi<-mask(maxndvi,watermask,maskvalue=0)
avgndvi<-mask(avgndvi,watermask,maskvalue=0)

alllakes.raw<-makeAVHRRannualts(alllakes.raw, accndvi) #run data processing steps
alllakes.raw<-makeAVHRRannualts(alllakes.raw, maxndvi)
alllakes.raw<-makeAVHRRannualts(alllakes.raw, avgndvi)

rm(accndvi,maxndvi,avgndvi,watermask)

#Data cleaning, filtering, and filling
alllakes.cln<-cleanLAGOSannualts(alllakes.raw, timespan=c(1989,2015), ymin=15, maxNA=0.1)

#save the data so we don't need to replicate some long-ish data processing steps.
#This file is on Box Sync, possibly load it since the data processing steps can be very long
#save(alllakes.cln, file="./Interannual Synchrony/PrelimCaseStudyData_20190108.RData")
#load("./Interannual Synchrony/PrelimCaseStudyData_20190108.RData")

# ------------------------------------------------------------------
# Produce coherence matrices
st<-c(2,4) #Define a timescale band for the coherence analyses. These are short timescales
# lt<-c(4,12) #Others are commented out to preserve what speed we can for this example
# at<-c(0,Inf)

cohres.st<-AquaTerrSynchPairCoh(alllakes.cln,band=st)
# cohres.lt<-AquaTerrSynchPairCoh(alllakes.cln,band=lt)
# cohres.at<-AquaTerrSynchPairCoh(alllakes.cln,band=at)

#again, saving (and loading) objects that take a long time to generate
#save(list=c("cohres.lt","cohres.st","cohres.at"),file="./Interannual Synchrony/CoherenceMatrices_PrelimCaseStudy_20190108.RData")
#load("./Interannual Synchrony/CoherenceMatrices_PrelimCaseStudy_20190108.RData")

# ------------------------------------------------------------------
# Make some coherence matrix plots

source("~/GitHub/AquaTerrSynch/Code/plotCoherenceMatrices.R")

# loop through the short timescales stuff
#pdf(file="PrelimCaseStudy_CoherenceMatrices_ts2_4.pdf", onefile=T) #lines to export to PDF commented out for example

for(lind in 1:length(cohres.st$cohres)){
  plotCoherenceMatrices(cohres.st$cohres[[lind]],
                        title=names(cohres.st$cohres)[lind],
                        filename=NULL)
}

#dev.off()

# -------------------------------------------------------------------
# Do lake characteristics gradient correlations

#acquire lake characteristics we care about from LAGOS -- 
lagoslakes.sel<-c(na.omit(alllakes.cln$lakeinfo$lagoslakeid))

dt<-lagosne_load(version = "1.087.1")

lakegeom<-lagosne_select(table="locus", vars=c("lagoslakeid","lake_area_ha","lake_perim_meters")) #pull lake info
lakedepth<-lagosne_select(table="lakes_limno", vars=c("lagoslakeid","meandepth","maxdepth"))

lakegeom<-lakegeom[lakegeom$lagoslakeid %in% lagoslakes.sel,] #select just the lakes we want
lakedepth<-lakedepth[lakedepth$lagoslakeid %in% lagoslakes.sel,]

# Get the medians of all of the coherence variables (if present for each lake) 
cohvars<-c("chla","colora","colort","no2no3","tn","tp","secchi","accndvi","maxndvi","avgndvi")
varmeds<-matrix(NA, nrow=length(alllakes.cln$lakedata), ncol=length(cohvars))
colnames(varmeds)<-cohvars
for(lind in 1:length(alllakes.cln$lakedata)){
  dat.lind<-alllakes.cln$lakedata[[lind]]
  for(vind in 1:length(cohvars)){
    if(any(cohvars[vind]==rownames(dat.lind))){
      varmeds[lind, vind]<-median(dat.lind[cohvars[vind]==rownames(dat.lind),], na.rm=T)
    }
  }
}

#merge dataframes together
graddat<-cbind(alllakes.cln$lakeinfo,varmeds)
graddat<-merge(graddat,lakedepth,by="lagoslakeid",all.x=T)
graddat<-merge(graddat,lakegeom,by="lagoslakeid", all.x=T)

#TODO: manually add some info from the Iowa reservoirs (if it materializes)

#add the coherence response variables

#short timescales
resp.st<-matrix(NA, nrow=length(cohres.st$cohres), ncol=6)
colnames(resp.st)<-c("coh.accndvi.st","coh.avgndvi.st","coh.maxndvi.st",
                     "phi.accndvi.st","phi.avgndvi.st","phi.maxndvi.st")
resp.st<-as.data.frame(resp.st)

for(lind in 1:length(cohres.st$cohres)){
  
  dat.lind<-cohres.st$cohres[[lind]]
  coh<-dat.lind$cohmat
  coh[is.na(coh)]<-0
  coh<-coh+t(coh)
  phi<-dat.lind$phimat
  phi[is.na(phi)]<-0
  phi<-phi+t(phi)
  
  resp.st$coh.accndvi.st[lind]<-coh[rownames(coh)=="chla",colnames(coh)=="accndvi"]
  resp.st$coh.avgndvi.st[lind]<-coh[rownames(coh)=="chla",colnames(coh)=="accndvi"]
  resp.st$coh.maxndvi.st[lind]<-coh[rownames(coh)=="chla",colnames(coh)=="maxndvi"]
  resp.st$phi.accndvi.st[lind]<-phi[rownames(phi)=="chla",colnames(phi)=="accndvi"]
  resp.st$phi.avgndvi.st[lind]<-phi[rownames(phi)=="chla",colnames(phi)=="accndvi"]
  resp.st$phi.maxndvi.st[lind]<-phi[rownames(phi)=="chla",colnames(phi)=="maxndvi"]
}
graddat.st<-cbind(graddat,resp.st)

#Write a function for making correlation biplots
biplots<-function(graddat, yvar, xvars=c("chla","no2no3","tn","tp","secchi",
                                         "accndvi","maxndvi","avgndvi","meandepth",
                                         "maxdepth","lake_area_ha","lake_perim_meters"),
                  title){
  
  nn<-ceiling(sqrt(length(xvars)))
  par(mfrow=c(nn,nn), mar=c(4.1,4.1,1.1,1), mgp=c(2,.8,0), oma=c(0,0,2,0))
  
  yy<-graddat[,colnames(graddat)==yvar]
  if(grepl("phi",yvar)){yy<-cos(yy)}
  
  for(vname in xvars){
    xx<-graddat[,colnames(graddat)==vname]
    
    corxy<-cor.test(xx,yy)
    plot(xx,yy, ylab=yvar, xlab=vname)
    mtext(paste0("cor=",round(corxy$estimate,3),", p=",round(corxy$p.value,3)),3)
    mtext(title, 3, outer=T, line=.5)
  }
}

#Apply it to finally make some results happen
#pdf("PrelimCaseStudy_GradientCorrelations_st.pdf", width=8.5, height=11)

biplots(graddat.st, yvar="coh.accndvi.st",title="coherence with accndvi, short ts")
biplots(graddat.st, yvar="phi.accndvi.st",title="cos(phi) with accndvi, short ts")
biplots(graddat.st, yvar="coh.avgndvi.st",title="coherence with avgndvi, short ts")
biplots(graddat.st, yvar="phi.avgndvi.st",title="cos(phi) with avgndvi, short ts")
biplots(graddat.st, yvar="coh.maxndvi.st",title="coherence with maxndvi, short ts")
biplots(graddat.st, yvar="phi.maxndvi.st",title="cos(phi) with maxndvi, short ts")

#dev.off()
