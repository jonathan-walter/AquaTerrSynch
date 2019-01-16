#Make timeseries from Iowa ALM data in same format as output from makeLAGOSannualts

#Arguments
#almdat =  a data frame of Iowa ambient lakes monitoring data
#aggfun = function for aggregating data to annual timeseries
#timespan = a two-element vector giving the min and max of years to extract data from
#minmos = there must be data spanning at least this number of months in the growing season, or else NA is returned
#minobs = there must be this many number of observations per year

makeIowaALMannualts<-function(almdat,aggfun="gsmean", timespan=NULL,
                              minmos=3, minobs=3){
  library(dplyr) #load library dependencies
  library(lubridate)
  library(stringr)
  
  
  
}