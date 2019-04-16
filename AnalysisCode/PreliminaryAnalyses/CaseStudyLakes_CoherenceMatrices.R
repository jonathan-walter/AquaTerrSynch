rm(list=ls())

setwd("~/Box Sync/NSF EAGER Synchrony/Interannual Synchrony")

load("CoherenceMatrices_PrelimCaseStudy_20190108.RData")

source("~/GitHub/AquaTerrSynch/Code/plotCoherenceMatrices.R")

# loop through the short timescales stuff
pdf(file="PrelimCaseStudy_CoherenceMatrices_ts2_4.pdf", onefile=T)

for(lind in 1:length(cohres.st$cohres)){
  plotCoherenceMatrices(cohres.st$cohres[[lind]],
                        title=names(cohres.st$cohres)[lind],
                        filename=NULL)
}

dev.off()

# loop through the long timescales stuff
pdf(file="PrelimCaseStudy_CoherenceMatrices_ts4_12.pdf", onefile=T)

for(lind in 1:length(cohres.lt$cohres)){
  plotCoherenceMatrices(cohres.lt$cohres[[lind]],
                        title=names(cohres.lt$cohres)[lind],
                        filename=NULL)
}

dev.off()

# loop through the all timescales stuff
pdf(file="PrelimCaseStudy_CoherenceMatrices_allts.pdf", onefile=T)

for(lind in 1:length(cohres.at$cohres)){
  plotCoherenceMatrices(cohres.at$cohres[[lind]],
                        title=names(cohres.at$cohres)[lind],
                        filename=NULL)
}

dev.off()
