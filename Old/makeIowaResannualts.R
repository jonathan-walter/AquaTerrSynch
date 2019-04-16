# Function for creating annual time series from LAGOS time series

#Arguments
#lakedat = an iowa reservoir data dataframe
#aggfun = a string indicating how to summarize multiple observations to an annual timescale.
#timespan = optional, a length-2 vector indicating bounding years to draw data from.
#minmos = the minimum number of months required to have data from for an annual value to be computed
#minobs = the minumum number of observations, spread over >= minmos, for an annual value to be computed

#Details
#currently, the only aggfun implemented is "gsmean," corresponding to the growing season mean.
#We consider the growing season to last from May to September.

#Value
#Returns a data frame of Iowa reservoir data, formatted like output from makeLAGOSannualts

makeIowaResAnnualts<-function(lakedat, aggfun="gsmean", timespan=NULL,
                            minmos=3, minobs=5)
{
  library(dplyr) #load library dependencies
  library(lubridate)
  library(stringr)
  
  epidat<-lakedat[,c(1,4,8,9)]
  names(epidat)<-c("sampledate","tp","chla","secchi")
  
  date.tmp<-simplify2array(strsplit(as.character(epidat$sampledate),"/")) #reformat the sampledate columnn
  epidat$sampledate<-paste(str_pad(date.tmp[1,],width=2,pad=0),
                           str_pad(date.tmp[2,],width=2,pad=0),
                           date.tmp[3,],sep="/")
  epidat$sampledate<-as.POSIXct(epidat$sampledate,format="%m/%d/%Y")
  epidat<-epidat[month(epidat$sampledate)>=5 & month(epidat$sampledate)<=9,] #remove observations outside growing season -- may need to move this in future!
  epidat.ii<-epidat[order(epidat$sampledate),] #order by sampledate
    
  if(!is.null(timespan)){ #if necessary, limit data to selected timespan
    epidat<-epidat[year(epidat$sampledate)>=min(timespan) &
                           year(epidat$sampledate)<=max(timespan),]
  }
  lakeyears<-min(year(epidat$sampledate)):max(year(epidat$sampledate))
  aggdat<-NULL
  for(yy in lakeyears){
    tmp.yy<-epidat[year(epidat$sampledate)==yy,]
    
    if(length(tmp.yy)==0){ydat<-rep(NA, ncol(epidat)-1)} 
    else{
      ydat<-NULL
      if(aggfun=="gsmean"){
        #compute variable
        for(nn in 1:ncol(tmp.yy)){
          if(colnames(tmp.yy)[nn] %in% c("sampledate")){next}
          if(nrow(tmp.yy)>=minobs & length(unique(month(tmp.yy$sampledate)))>=minobs){
            ydat<-c(ydat,mean(tmp.yy[,nn], na.rm=TRUE))
          }
          else(ydat<-c(ydat,NA))
        }
      }
    }
  aggdat<-cbind(aggdat,ydat)  
  }#close year loop
  aggdat[is.nan(aggdat)]<-NA
  rownames(aggdat)<-c("tp","chla","secchi")
  colnames(aggdat)<-lakeyears
  
  return(aggdat)
}