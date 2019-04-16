# Function for creating annual time series from LAGOS time series

#Arguments
#lakes = a vector of LAGOS lake identifying numbers
#vars = a character vector of variables to extract
#aggfun = a string indicating how to summarize multiple observations to an annual timescale.
#timespan = optional, a length-2 vector indicating bounding years to draw data from.
#minmos = the minimum number of months required to have data from for an annual value to be computed
#minobs = the minumum number of observations, spread over >= minmos, for an annual value to be computed

#Details
#currently, the only aggfun implemented is "gsmean," corresponding to the growing season mean.
#We consider the growing season to last from May to September.

#Value
#makeLAGOSannualts returns a list, with the slots 'lakeinfo' and 'lakedata'.
#'lakeinfo' is a data frame containing the numeric id code, name, and geographic coordinates of 
#each lake in 'lakes'.
#'lakedata' is a list of data frames. Each element corresponds to a lake in 'lakes',
#and each element is a data frame in which columns are variables in 'vars' and rows correspond
#to years.

makeLAGOSannualts<-function(lakes, getvars=c("chla"), aggfun="gsmean", timespan=NULL,
                            minmos=3, minobs=5, lagosversion="1.087.1")
{
  library(dplyr) #load library dependencies
  library(LAGOSNE)
  library(lubridate)
  library(stringr)
  
  getvars<-c("lagoslakeid","sampledate",getvars) #makesure lagoslakeid and sampledate are always pulled
  getvars<-unique(getvars)
  
  dt<-lagosne_load(version = lagosversion)
  
  epidat<-lagosne_select(table="epi_nutr", vars=getvars[getvars != "secchi"]) #pull lake ecological data
  info<-lagosne_select(table="locus", vars=c("lagoslakeid","gnis_name","nhd_lat","nhd_long")) #pull lake info
  
  if("secchi" %in% getvars){ #add secchi depth information stored in separate table
    secchi<-lagosne_select(table="secchi",vars=c("lagoslakeid","sampledate","secchi"))
    secchi$lakedate<-paste(secchi$lagoslakeid,secchi$sampledate,sep="_")
    epidat$lakedate<-paste(epidat$lagoslakeid,epidat$sampledate,sep="_")
    epidat<-left_join(epidat,secchi[,3:4],by="lakedate")
    epidat<-epidat[,colnames(epidat)!="lakedate"]
    rm(secchi)
  }
  date.tmp<-simplify2array(strsplit(as.character(epidat$sampledate),"/")) #reformat the sampledate columnn
  epidat$sampledate<-paste(str_pad(date.tmp[1,],width=2,pad=0),
                           str_pad(date.tmp[2,],width=2,pad=0),
                           date.tmp[3,],sep="/")
  epidat$sampledate<-as.POSIXct(epidat$sampledate,format="%m/%d/%Y")
  epidat<-epidat[month(epidat$sampledate)>=5 & month(epidat$sampledate)<=9,] #remove observations outside growing season -- may need to move this in future!
  
  
  lakeinfo<-info[info$lagoslakeid %in% lakes,] #extract lake information to output
  lakeinfo$gnis_name<-as.character(lakeinfo$gnis_name)
  lakeinfo$start=rep(NA,length(lakes))
  lakeinfo$end=rep(NA,length(lakes))
  lakeinfo<-lakeinfo[order(lakeinfo$gnis_name),]
  
  lakedata<-list() #initialize output list
  
  for(ii in lakeinfo$lagoslakeid){ #start loop through selected lakes
    epidat.ii<-epidat[epidat$lagoslakeid == ii,] #select data from lake ii
    epidat.ii<-epidat.ii[order(epidat.ii$sampledate),] #order by sampledate
    
    if(!is.null(timespan)){ #if necessary, limit data to selected timespan
      epidat.ii<-epidat.ii[year(epidat.ii$sampledate)>=min(timespan) &
                             year(epidat.ii$sampledate)<=max(timespan),]
    }
    lakeyears<-min(year(epidat.ii$sampledate)):max(year(epidat.ii$sampledate))
    aggdat<-NULL
    for(yy in lakeyears){
      tmp.yy<-epidat.ii[year(epidat.ii$sampledate)==yy,]
      
      if(length(tmp.yy)==0){ydat<-rep(NA, ncol(epidate.ii)-2)} 
      else{
        ydat<-NULL
        if(aggfun=="gsmean"){
          #compute variable
          for(nn in 1:ncol(tmp.yy)){
            if(colnames(tmp.yy)[nn] %in% c("sampledate","lagoslakeid")){next}
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
    rownames(aggdat)<-getvars[!getvars %in% c("sampledate","lagoslakeid")]
    colnames(aggdat)<-lakeyears
    lakedata[[paste0(lakeinfo$gnis_name[lakeinfo$lagoslakeid==ii])]]<-aggdat
    lakeinfo$start[lakeinfo$lagoslakeid==ii]<-min(lakeyears)
    lakeinfo$end[lakeinfo$lagoslakeid==ii]<-max(lakeyears)
  }#close lake loop
  
  return(list(lakeinfo=lakeinfo, lakedata=lakedata))
}

#test<-makeLAGOSannualts(lakes=c(122517,25980,4664))
