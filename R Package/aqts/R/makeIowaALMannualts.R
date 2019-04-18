#' Create annualized time series from Iowa ALM data
#' 
#' \code{makeIowaALMannualts} formats Iowa ALM data to match output from \code{makeLAGOSannualts}.
#' 
#' @param almdat a data frame of Iowa Ambient Lakes Monitoring (ALM) data, typically 'Iowa Synchrony Lakes.csv' on Box
#' @param lakechar a lake characteristics data frame, typically 'Iowa Lake Characteristics.csv' on Box
#' @param tsvars a character vector of variables to extract and construct time series from.
#' @param infovars a character vector of lake characteristic variables to include in \code{lakeinfo}. See details.
#' @param aggfun a string indicating how to summarize multiple observations to an annual timescale. See details.
#' @param timespan a vector giving the min and max of a range of years which data will be restricted to.
#' @param minmos the minimum number of months required to have data from for an annual value to be computed.
#' @param minobs the minimum number of observations, spread over \code{>= minmos}, for an annual value to be computed
#' 
#' @return \code{makeIowaALMannualts} returns an object of class \code{list}. Slots are:
#' \item{lakeinfo}{A data frame of lake information.}
#' \item{lakedata}{A list of data frames containing the lake data.}
#' 
#' @details Currently, the only accepted value of \code{aggfun} is \code{"gsmean"}, corresponding to the growing season mean.
#' We consider the growing season to last May to September. Output \code{lakeinfo} is a data frame containing the numeric id code, name, 
#' and geographic coordinates of lakes, plus any further variables listed in \code{infovars}. Each element of \code{lakedata} corresponds to a lake
#' in \code{lakes} and is a data frame in which columns are variables in \code{tsvars} and rows correspond to years.
#' 
#' @author Jonathan Walter, \email{jaw3es@@virginia.edu}
#' 
#' @seealso \code{\link{makeLAGOSannualts}}
#' 
#' @examples
#' #need to add some
#' 
#' @export

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
  colnames(lakeinfo)<-c("lagoslakeid","gnis_name","nhd_lat","nhd_long")
  
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
            if(nrow(tmp.yy)>=minobs & length(unique(month(tmp.yy$sampledate)))>=minmos){
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
