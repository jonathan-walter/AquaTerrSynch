#Make timeseries from Iowa ALM data in same format as output from makeLAGOSannualts

#Arguments
#almdat =  a data frame of Iowa ambient lakes monitoring data, from Iowa Synchrony Lakes.csv
#lakechar = a data frame of Iowa Lakes Characteristics, from Iowa Lake Characteristics.csv
#getvars = a character vector of variable names for extraction
#aggfun = function for aggregating data to annual timeseries
#timespan = a two-element vector giving the min and max of years to extract data from
#minmos = there must be data spanning at least this number of months in the growing season, or else NA is returned
#minobs = there must be this many number of observations per year

#Values
#A list of matrices containing time series for Iowa lakes

makeIowaALMannualts<-function(almdat, lakechar, getvars, aggfun="gsmean", timespan=NULL,
                              minmos=3, minobs=3){
  library(dplyr) #load library dependencies
  library(lubridate)
  library(stringr)
  
  almdat<-almdat[order(almdat$LakeID),]
  selvars<-c("LakeID","LakeName","SampleDate",getvars)
  almdat<-almdat[colnames(almdat) %in% selvars]
  
  lakechar<-lakechar[order(lakechar$LakeID),]
  lakeinfo<-lakechar[,colnames(lakechar) %in% c("LakeID","LakeName","Latitude","Longitude")]
  colnames(lakeinfo)<-c("lagoslakeid","gnis_name","nhd_lat","nhd_long") #TODO reconcile column names in lake info
  
  lakedata<-list()
  
  date.tmp<-simplify2array(strsplit(as.character(almdat$SampleDate),"/")) #reformat the sampledate columnn
  almdat$sampledate<-paste(str_pad(date.tmp[1,],width=2,pad=0),
                           str_pad(date.tmp[2,],width=2,pad=0),
                           date.tmp[3,],sep="/")
  almdat$sampledate<-as.POSIXct(almdat$sampledate,format="%m/%d/%Y")
  
  almdat<-almdat[month(almdat$sampledate)>=5 & month(almdat$sampledate)<=9,] #remove observations outside growing season -- may need to move this in future!
  
  lakeIDs<-unique(almdat$LakeID)
  lakenames<-unique(almdat$LakeName)
  
  for(ii in 1:length(lakeIDs)){
    almdat.ii<-almdat[almdat$LakeID==lakeIDs[ii],]
    almdat.ii<-almdat[order(almdat$sampledate),] #order by sampledate
    
    if(!is.null(timespan)){ #if necessary, limit data to selected timespan
      almdat<-almdat[year(almdat$sampledate)>=min(timespan) &
                       year(almdat$sampledate)<=max(timespan),]
    }
    lakeyears<-min(year(almdat$sampledate)):max(year(almdat$sampledate))
    aggdat<-NULL
    for(yy in lakeyears){
      tmp.yy<-almdat[year(almdat$sampledate)==yy,]
      
      if(length(tmp.yy)==0){ydat<-rep(NA, ncol(almdat)-1)} 
      else{
        ydat<-NULL
        if(aggfun=="gsmean"){
          #compute variable
          for(nn in 1:ncol(tmp.yy)){
            if(colnames(tmp.yy)[nn] %in% c("sampledate","LakeName","SampleDate","LakeID")){next}
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
    rownames(aggdat)<-getvars
    colnames(aggdat)<-lakeyears
    lakedata[[paste0(lakenames[ii])]]<-aggdat
    lakeinfo$start[lakeinfo$lagoslakeid==lakeIDs[ii]]<-min(lakeyears)
    lakeinfo$end[lakeinfo$lagoslakeid==lakeIDs[ii]]<-max(lakeyears)
  }#close lake loop
  return(list(lakeinfo=lakeinfo,lakedata=lakedata))
}

#test<-makeIowaALMannualts(almdat,getvars,aggfun="gsmean")
