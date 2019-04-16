### get LAGOS R package from CRAN and explore it to determine how many lakes appear to be useable


rm(list=ls())

#install.packages("LAGOSNE")

library(LAGOSNE)
library(lubridate)
library(rgdal)
library(dplyr)

states<-readOGR("~/Box Sync/NSF EAGER Synchrony/MiscData/statesp020.shp")
states<-states[!states@data$STATE %in% c("Alaska","Hawaii","Puerto Rico"),]

#lagosne_get(version="1.087.1") #only need to run once or to update to new database version

dt<-lagosne_load(version = "1.087.1")

vars.raw<-lagosne_select(table="epi_nutr", vars=c("lagoslakeid","chla","sampledate"))
coords<-lagosne_select(table="locus", vars=c("lagoslakeid","nhd_lat","nhd_long"))


test<-lagosne_select(table="epi_nutr", vars=c("colora","colort","colora_labmethodname","colort_labmethodname"))
test<-test[complete.cases(test),]


vars<-left_join(vars.raw,coords)

vars<-vars[complete.cases(vars),] #remove obs with missing data

lim.x=c(floor(min(coords$nhd_long)),ceiling(max(coords$nhd_long)))
lim.y<-c(floor(min(coords$nhd_lat)),ceiling(max(coords$nhd_lat)))

###############################################################################
## Do some first-pass data checks
###############################################################################
length(unique(vars$lagoslakeid))

check.20obs<-NULL #these lakes have at least 20 observations
check.20yrs<-NULL #these lakes have data spanning at least 20 years
check.20consec<-NULL #these lakes have data spanning 20 consecutive years

for(id in unique(vars$lagoslakeid)){
  vars.sub<-vars[vars$lagoslakeid == id,]
  #id lakes with >= 20 observations of chla
  if(nrow(vars.sub) >= 20) 
  {
    check.20obs=c(check.20obs, id)
  } 
  
  #id lakes with data spanning >= 20 years, 
  vars.sub$year<-as.numeric(simplify2array(strsplit(as.character(vars.sub$sampledate),"/"))[3,])
  if(max(vars.sub$year)-min(vars.sub$year) >= 20) 
  {
    check.20yrs<-c(check.20yrs, id)
  }
  
  #id lakes with >= 20 consecutive years of data
  if(length(unique(vars.sub$year))>=20) 
  {
    ydiff<-diff(vars.sub$year[order(vars.sub$year)])
    if(sum(ydiff==1)>=20)
    {
      check.20consec<-c(check.20consec, id)
    }
  }
}

d20consec<-vars[vars$lagoslakeid %in% check.20consec,]

plot(states, main="lakes with 20 consecutive years of chla observations", xlim=lim.x, ylim=lim.y)
points(d20consec$nhd_long,d20consec$nhd_lat, pch=".", cex=3, col="red")

####################################################################################
## Take a closer look at lakes with at least 20 observations
## 
## Create a data matrix that contains:
## - number of years with data
## - average and sd number of observations per year
## - earliest month with data
## - latest month with data
####################################################################################
summdf.20obs<-NULL
for(id in check.20obs){
  vars.sub<-vars[vars$lagoslakeid == id,]
  vars.sub$sampledatePOSIXct<-as.POSIXct(vars.sub$sampledate,format="%m/%d/%Y")
  vars.sub<-vars.sub[order(vars.sub$sampledatePOSIXct),]
  
  tabbyyr<-table(year(vars.sub$sampledatePOSIXct))
  
  tmpout<-c(id,
            length(unique(year(vars.sub$sampledatePOSIXct))),
            mean(table(year(vars.sub$sampledatePOSIXct))),
            sd(table(year(vars.sub$sampledatePOSIXct))),
            min(month(vars.sub$sampledatePOSIXct)),
            max(month(vars.sub$sampledatePOSIXct))
            )
  summdf.20obs<-rbind(summdf.20obs,tmpout)
}
colnames(summdf.20obs)<-c("lagoslakeid","no.yrs","avg.obs_yr","sd.obs_yr","min.month","max.month")

colMeans(summdf.20obs, na.rm=T)

write.csv(summdf.20obs, file="~/Box Sync/NSF EAGER Synchrony/lake_sampling_info_20obs.csv", row.names=F)

#how many lakes may have year-round data?
sum(summdf.20obs[,5]==1 & summdf.20obs[,6]==12)

summdf.20obs.allyr<-summdf.20obs[summdf.20obs[,5]==1 & summdf.20obs[,6]==12 & summdf.20obs[,2]>11,]

d20obs.allyr<-vars[vars$lagoslakeid %in% summdf.20obs.allyr[,"lagoslakeid"],]
plot(states, main="20 chla observations, year-round data", xlim=lim.x, ylim=lim.y)
points(d20obs.allyr$nhd_long,d20obs.allyr$nhd_lat, pch=".",cex=3, col="red")

####################################################################################
## Take a closer look at lakes that have been sampled for at least 20 consecutive years
## 
## Create a data frame that contains:
## - number of years with data
## - average and sd number of observations per year
## - earliest month with data
## - latest month with data
####################################################################################
summdf.20consec<-NULL
for(id in check.20consec){
  vars.sub<-vars[vars$lagoslakeid == id,]
  vars.sub$sampledatePOSIXct<-as.POSIXct(vars.sub$sampledate,format="%m/%d/%Y")
  vars.sub<-vars.sub[order(vars.sub$sampledatePOSIXct),]
  
  tabbyyr<-table(year(vars.sub$sampledatePOSIXct))
  
  tmpout<-c(id,
            length(unique(year(vars.sub$sampledatePOSIXct))),
            mean(table(year(vars.sub$sampledatePOSIXct))),
            sd(table(year(vars.sub$sampledatePOSIXct))),
            min(month(vars.sub$sampledatePOSIXct)),
            max(month(vars.sub$sampledatePOSIXct))
  )
  summdf.20consec<-rbind(summdf.20consec,tmpout)
}
colnames(summdf.20consec)<-c("lagoslakeid","no.yrs","avg.obs_yr","sd.obs_yr","min.month","max.month")

colMeans(summdf.20consec, na.rm=T)

write.csv(summdf.20consec, file="~/Box Sync/NSF EAGER Synchrony/lake_sampling_info_20consec.csv", row.names=F)

d20consec<-vars[vars$lagoslakeid %in% summdf.20consec[,"lagoslakeid"],]

# plot(states, main="records in 20 consecutive years", xlim=lim.x, ylim=lim.y)
# points(d20consec$nhd_long,d20consec$nhd_lat, pch=".", cex=3, col="red")


#########################################################################
## LAGOS epi_limno format
#########################################################################
# Format
# 
# A data frame with 289482 observations of 93 variables:
#   
# eventida1087: unique combination of programid, lakeid, and date for each sampling event in LAGOSNE
# lagoslakeid: unique integer identifier for each lake in LAGOSNE
# programname: name of the sampling/monitoring program that collected the data
# programtype: categorical description of the type of sampling/monitoring effort (Federal Agency, LTER = Long Term Ecological Research program, National Survey Program, Non-Profit Agency, State Agency, State Agency/Citizen Monitoring Program, State Agency/University/Citizen Monitoring Program, State Agency/Citizen Monitoring Program, Tribal Agency, University)
# lagosversion: current version of LAGOSNE that the data record belongs to
# sampledate: date at which the sample was collected, stored in date format (YYYY-MM-DD)
# chla: chlorophyll a (µg/l)
# colora: color, apparent (PCU)
# colort: color, true (PCU)
# dkn: nitrogen, dissolved Kjeldahl (µg/l as N)
# doc: carbon, dissolved organic (µg/l as C)
# nh4: nitrogen, ammonium NH4 (µg/l as N)
# no2: nitrogen, nitrite NO2 (µg/l as N)
# no2no3: nitrogen, nitrite NO2 + nitrate NO3 nitrogen (µg/l as N)
# srp: phosphorus, soluble reactive (µg/l as P)
# tdn: nitrogen, total dissolved (µg/l as N)
# tdp: phosphorus, total dissolved (µg/l as P)
# tkn: nitrogen, total Kjeldahl (µg/l as N)
# tn: nitrogen, total (µg/l as N)
# toc: carbon, total organic (µg/l as C)
# ton: nitrogen, total organic (µg/l as N)
# tp: phosphorus, total (µg/l as P)
# secchi: Secchi disk transparency (m). More secchi data from additional lakes is available from the secchi table.
# *_qual: data flags (qualifiers) from the source program for each water quality parameter (* = all water quality parameters listed above). For example, "chla_qual" is the variable name for the data flag for chlorophyll a observations.
# *_censorcode: identifies whether a data value is censored and the censor type for each water quality parameter (* = all water quality parameters listed above). For example, "chla_censorcode" is the variable name for censor information regarding chlorophyll a observations.
# 
# NC1: has detection limit and data value is above detection limit and has no qualifier or comments
# NC2: has detection limit and data value is above detection limit, and has qualifier or comments
# NC3: has no detection limit and has qualifier or comments
# NC4: has no detection limit and has no qualifiers or comments
# LE1: has detection limit, data value is less than or equal to detection limit, has qualifier or comments
# LE2: has detection limit, data value is less than or equal to detection limit, has no qualifier or comments
# LE3: has no detection limit, < than comes from source program, has qualifier or comments
# LE4: has no detection limit, < than comes from source program, has no qualifier or comments
# *_detectionlimit: the detection limit used by the source program for each water quality parameter (* = all water quality parameters listed above). For example, "chla_detectionlimit" is the name of the variable that reports detection limits for chlorophyll a observations.
# *_labmethodname: analytical procedure, from a standards body if available for each water quality parameter (* = all water quality parameters listed above). For example, "chla_labmethodname" is the variable name for lab methods associated with each chlorophyll a observation.
# chla_methodinfo: flag to indicate variable was sampled using different methods. "CHLA_UNFILTERED" means that a sample was
# secchi_methodinfo: flag to indicate variable was sampled using different methods. "SECCHI_VIEW" means that Secchi depth was measured using a viewer box, whereas "SECCHI_VIEW_UNKNOWN" reports observations where it is unclear if a Secchi viewer box was used. A secchi observation without a methodinfo code means the Secchi depth was made without the viewer box.
# sampleyear: year (4-digit) in which sample was collected
# samplemonth: month in which sample was collected
