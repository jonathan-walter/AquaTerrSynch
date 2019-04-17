### get LAGOS R package from CRAN and explore it to determine how many lakes appear to be useable


rm(list=ls())

#install.packages("LAGOSNE")

library(LAGOSNE)
library(lubridate)

#lagosne_get(version="1.087.1") #only need to run once or to update to new database version

dt<-lagosne_load(version = "1.087.1")

lakeinfo<-lagosne_select(table="locus", vars=c("lagoslakeid","gnis_name"))


vars.raw<-lagosne_select(table="epi_nutr", vars=c("lagoslakeid","chla","sampledate"))

vars<-vars.raw #make a copy to manipulate
vars<-vars[complete.cases(vars),] #remove obs with missing data


###############################################################################
## Do some first-pass data checks
###############################################################################
length(unique(vars$lagoslakeid))

check.20yrs<-NULL #these lakes have data spanning at least 20 years


for(id in unique(vars$lagoslakeid)){
  vars.sub<-vars[vars$lagoslakeid == id,]
  #id lakes with data spanning >= 20 years, 
  vars.sub$year<-as.numeric(simplify2array(strsplit(as.character(vars.sub$sampledate),"/"))[3,])
  if(max(vars.sub$year)-min(vars.sub$year) >= 20) 
  {
    check.20yrs<-c(check.20yrs, id)
  }
  
}

write.csv(check.20yrs, file="~/GitHub/AquaTerrSynch/AnalysisCode/lakes20yrs_chla.csv", row.names=F)
