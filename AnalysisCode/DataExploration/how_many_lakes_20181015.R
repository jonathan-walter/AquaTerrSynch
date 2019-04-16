### get LAGOS R package from CRAN and explore it to determine how many lakes appear to be useable


rm(list=ls())

#install.packages("LAGOSNE")

library(LAGOSNE)
library(lubridate)

#lagosne_get(version="1.087.1") #only need to run once or to update to new database version

dt<-lagosne_load(version = "1.087.1")

vars.raw<-lagosne_select(table="epi_nutr", vars=c("lagoslakeid","chla","sampledate"))

vars<-vars.raw #make a copy to manipulate
vars<-vars[complete.cases(vars),] #remove obs with missing data


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



