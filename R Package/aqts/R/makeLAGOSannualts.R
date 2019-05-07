#' Create annualized time series from the LAGOSNE database
#' 
#' \code{makeLAGOSannualts} extracts lake attributes and annualized limnological time series from LAGOSNE.
#' 
#' @param lakes a numeric vector with entries corresponding to selected \code{lagoslakeid} lake unique identifying codes.
#' @param tsvars a character vector of variables to extract and construct time series from.
#' @param infovars a character vector of lake characteristic variables to include in \code{lakeinfo}. See details.
#' @param aggfun a string indicating how to summarize multiple observations to an annual timescale. See details.
#' @param timespan a vector giving the min and max of a range of years which data will be restricted to.
#' @param minmos the minimum number of months required to have data from for an annual value to be computed.
#' @param minobs the minimum number of observations, spread over \code{>= minmos}, for an annual value to be computed
#' 
#' @return \code{makeLAGOSannualts} returns an object of class \code{list}. Slots are:
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
#' @seealso \code{\link{makeIowaALMannualts}}
#' 
#' @examples
#' #need to add some
#' 
#' @export

makeLAGOSannualts<-function(lakes, tsvars=c("chla"), infovars=NULL, aggfun="gsmean", timespan=NULL,
                            minmos=3, minobs=5, lagosversion="1.087.1")
{
  library(dplyr) #load library dependencies
  library(LAGOSNE)
  library(lubridate)
  library(stringr)
  
  dt<-lagosne_load(version = lagosversion)
  
  tsvars<-unique(c("lagoslakeid","sampledate",tsvars)) #makesure lagoslakeid and sampledate are always pulled
  
  infovars<-unique(c("lagoslakeid","gnis_name","nhd_lat","nhd_long", infovars)) #make sure some vars are always pulled
  
  epidat<-lagosne_select(table="epi_nutr", vars=tsvars[tsvars != "secchi"]) #pull lake ecological data
  info<-lagosne_select(table="locus", vars=infovars) #pull lake info
  
  if("secchi" %in% tsvars){ #add secchi depth information stored in separate table
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
  lakeinfo<-lakeinfo[order(lakeinfo$lagoslakeid),]
  
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
      
      if(length(tmp.yy)==0){ydat<-rep(NA, ncol(epidat.ii)-2)} 
      else{
        ydat<-NULL
        if(aggfun=="gsmean"){
          #compute variable
          for(nn in 1:ncol(tmp.yy)){
            if(colnames(tmp.yy)[nn] %in% c("sampledate","lagoslakeid")){next}
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
    rownames(aggdat)<-tsvars[!tsvars %in% c("sampledate","lagoslakeid")]
    colnames(aggdat)<-lakeyears
    lakedata[[paste0(ii)]]<-aggdat
    lakeinfo$start[lakeinfo$lagoslakeid==ii]<-min(lakeyears)
    lakeinfo$end[lakeinfo$lagoslakeid==ii]<-max(lakeyears)
  }#close lake loop
  
  return(list(lakeinfo=lakeinfo, lakedata=lakedata))
}

