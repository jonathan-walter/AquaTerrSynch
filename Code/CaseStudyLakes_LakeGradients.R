## Look at relationships between coherence and various gradients

## Response variables: Coherence magnitude, phase
## Predictor variables: Area, depth, total P, total N, Secchi, apparent color, true color, nitrate + nitrite

rm(list=ls())

library(dplyr) #load library dependencies
library(LAGOSNE)
library(lubridate)
library(stringr)

setwd("~/Box Sync/NSF EAGER Synchrony/Interannual Synchrony")

load("PrelimCaseStudyData_20190108.RData")

lagoslakes.sel<-c(na.omit(alllakes.cln$lakeinfo$lagoslakeid))

dt<-lagosne_load(version = "1.087.1")

lakegeom<-lagosne_select(table="locus", vars=c("lagoslakeid","lake_area_ha","lake_perim_meters")) #pull lake info
lakedepth<-lagosne_select(table="lakes_limno", vars=c("lagoslakeid","meandepth","maxdepth"))

lakegeom<-lakegeom[lakegeom$lagoslakeid %in% lagoslakes.sel,]
lakedepth<-lakedepth[lakedepth$lagoslakeid %in% lagoslakes.sel,]

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

graddat<-cbind(alllakes.cln$lakeinfo,varmeds)
graddat<-merge(graddat,lakedepth,by="lagoslakeid",all.x=T)
graddat<-merge(graddat,lakegeom,by="lagoslakeid", all.x=T)

#manually add some info from the Iowa lakes (if it materializes)

#add the coherence response variables
load("CoherenceMatrices_PrelimCaseStudy_20190108.RData")

varpairs<-expand.grid("chla",c("accndvi","avgndvi","maxndvi"))

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

#long timescales
resp.lt<-matrix(NA, nrow=length(cohres.lt$cohres), ncol=6)
colnames(resp.lt)<-c("coh.accndvi.lt","coh.avgndvi.lt","coh.maxndvi.lt",
                     "phi.accndvi.lt","phi.avgndvi.lt","phi.maxndvi.lt")
resp.lt<-as.data.frame(resp.lt)

for(lind in 1:length(cohres.lt$cohres)){
  
  dat.lind<-cohres.lt$cohres[[lind]]
  coh<-dat.lind$cohmat
  coh[is.na(coh)]<-0
  coh<-coh+t(coh)
  phi<-dat.lind$phimat
  phi[is.na(phi)]<-0
  phi<-phi+t(phi)
  
  resp.lt$coh.accndvi.lt[lind]<-coh[rownames(coh)=="chla",colnames(coh)=="accndvi"]
  resp.lt$coh.avgndvi.lt[lind]<-coh[rownames(coh)=="chla",colnames(coh)=="accndvi"]
  resp.lt$coh.maxndvi.lt[lind]<-coh[rownames(coh)=="chla",colnames(coh)=="maxndvi"]
  resp.lt$phi.accndvi.lt[lind]<-phi[rownames(phi)=="chla",colnames(phi)=="accndvi"]
  resp.lt$phi.avgndvi.lt[lind]<-phi[rownames(phi)=="chla",colnames(phi)=="accndvi"]
  resp.lt$phi.maxndvi.lt[lind]<-phi[rownames(phi)=="chla",colnames(phi)=="maxndvi"]
}

#all timescales
resp.at<-matrix(NA, nrow=length(cohres.at$cohres), ncol=6)
colnames(resp.at)<-c("coh.accndvi.at","coh.avgndvi.at","coh.maxndvi.at",
                     "phi.accndvi.at","phi.avgndvi.at","phi.maxndvi.at")
resp.at<-as.data.frame(resp.at)

for(lind in 1:length(cohres.at$cohres)){
  
  dat.lind<-cohres.at$cohres[[lind]]
  coh<-dat.lind$cohmat
  coh[is.na(coh)]<-0
  coh<-coh+t(coh)
  phi<-dat.lind$phimat
  phi[is.na(phi)]<-0
  phi<-phi+t(phi)
  
  resp.at$coh.accndvi.at[lind]<-coh[rownames(coh)=="chla",colnames(coh)=="accndvi"]
  resp.at$coh.avgndvi.at[lind]<-coh[rownames(coh)=="chla",colnames(coh)=="accndvi"]
  resp.at$coh.maxndvi.at[lind]<-coh[rownames(coh)=="chla",colnames(coh)=="maxndvi"]
  resp.at$phi.accndvi.at[lind]<-phi[rownames(phi)=="chla",colnames(phi)=="accndvi"]
  resp.at$phi.avgndvi.at[lind]<-phi[rownames(phi)=="chla",colnames(phi)=="accndvi"]
  resp.at$phi.maxndvi.at[lind]<-phi[rownames(phi)=="chla",colnames(phi)=="maxndvi"]
}


graddat.st<-cbind(graddat,resp.st)
graddat.lt<-cbind(graddat,resp.lt)
graddat.at<-cbind(graddat,resp.at)



#biplots

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


pdf("PrelimCaseStudy_GradientCorrelations.pdf", width=8.5, height=11)

biplots(graddat.st, yvar="coh.accndvi.st",title="coherence with accndvi, short ts")
biplots(graddat.st, yvar="phi.accndvi.st",title="cos(phi) with accndvi, short ts")
biplots(graddat.st, yvar="coh.avgndvi.st",title="coherence with avgndvi, short ts")
biplots(graddat.st, yvar="phi.avgndvi.st",title="cos(phi) with avgndvi, short ts")
biplots(graddat.st, yvar="coh.maxndvi.st",title="coherence with maxndvi, short ts")
biplots(graddat.st, yvar="phi.maxndvi.st",title="cos(phi) with maxndvi, short ts")

biplots(graddat.lt, yvar="coh.accndvi.lt",title="coherence with accndvi, long ts")
biplots(graddat.lt, yvar="phi.accndvi.lt",title="cos(phi) with accndvi, long ts")
biplots(graddat.lt, yvar="coh.avgndvi.lt",title="coherence with avgndvi, long ts")
biplots(graddat.lt, yvar="phi.avgndvi.lt",title="cos(phi) with avgndvi, long ts")
biplots(graddat.lt, yvar="coh.maxndvi.lt",title="coherence with maxndvi, long ts")
biplots(graddat.lt, yvar="phi.maxndvi.lt",title="cos(phi) with maxndvi, long ts")

biplots(graddat.at, yvar="coh.accndvi.at",title="coherence with accndvi, all ts")
biplots(graddat.at, yvar="phi.accndvi.at",title="cos(phi) with accndvi, all ts")
biplots(graddat.at, yvar="coh.avgndvi.at",title="coherence with avgndvi, all ts")
biplots(graddat.at, yvar="phi.avgndvi.at",title="cos(phi) with avgndvi, all ts")
biplots(graddat.at, yvar="coh.maxndvi.at",title="coherence with maxndvi, all ts")
biplots(graddat.at, yvar="phi.maxndvi.at",title="cos(phi) with maxndvi, all ts")

dev.off()
