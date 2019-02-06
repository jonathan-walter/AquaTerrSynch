## Make plots of the Prelim Case Study Data

rm(list=ls())

setwd("~/Box Sync/NSF EAGER Synchrony/Interannual Synchrony")

source("~/GitHub/AquaTerrSynch/Code/plotTsWpow.R")

load("PrelimCaseStudyData_20190108.RData")

pdf("rawDataPlots.pdf", width=8.5, height=11)

for(lind in 1:length(alllakes.cln$lakedata)){
  
  lakename<-names(alllakes.cln$lakedata)[lind]
  plotTsWpow(alllakes.cln$lakedata[[lind]], title=lakename)
  
}

dev.off()