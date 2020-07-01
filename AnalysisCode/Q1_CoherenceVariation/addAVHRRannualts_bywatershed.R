#' Add terrestrial AVHRR time series to limnological time series
#' 
#' \code{addAVHRRannualts} extracts terrestrial AVHRR time series and appends them to limnological time series produced by,
#' e.g., \code{makeLAGOSannualts}.
#' 
#' @param lakelist a list containing \code{lakeinfo} and \code{lakedata}, as output from \code{makeLAGOSannualts}.
#' @param avhrrdata a raster stack of annual AVHRR metrics
#' @param buffdist the distance (in meters) of the radius around each lake to draw terrestrial productivity information from.
#' @param aggfun a string indicating how to aggregate values from multiple satellite measurements within the buffer. Defaults to "mean",
#' "median" is also available.
#' 
#' @return \code{addAVHRRannualts} returns an object of class \code{list}. Slots are:
#' \item{lakeinfo}{A data frame of lake information.}
#' \item{lakedata}{A list of data frames containing the lake data. The avhrr data are appended here.}
#' 
#' @details Nope.
#' 
#' @author Jonathan Walter, \email{jaw3es@@virginia.edu}
#' 
#' @seealso \code{\link{addAVHRRnnualts}}, \code{\link{makeIowaALMannualts}}
#' 
#' @examples
#' #need to add some
#' 
#' @export

addAVHRRannualts_byWatershed<-function(lakelist, avhrrdata, watersheds, aggfun="mean"){
  
  library(raster)
  library(rgdal)
  library(sp)
  
  lakeinfo<-lakelist$lakeinfo
  lakedata<-lakelist$lakedata
  

  #loop through lakes and extract AVHRR time series
  for(ii in 1:nrow(lakeinfo)){
    lakeyrs<-lakeinfo$start[ii]:lakeinfo$end[ii]
    watershed.ii<-watersheds[watersheds$lagoslakeid == lakeinfo$lagoslakeid[ii],]
    if(length(watershed.ii)==0){
      ts.ii<-rep(NA, length(lakeyrs))
    }
    else{
      avhrrstack.ii<-crop(avhrrdata, watershed.ii)
      avhrrstack.ii<-mask(avhrrstack.ii, watershed.ii)
      
      if(sum(!is.na(avhrrstack.ii@data@values[,1]))<=3){
        avhrrstack.ii<-crop(avhrrdata, watershed.ii)
      }
      
      if(!aggfun %in% c("mean","median")){stop("aggfun argument must be 'mean' or 'median'.")}
      else if(aggfun=="mean"){
        ts.ii<-cellStats(avhrrstack.ii, mean, na.rm=TRUE)
      }
      else if(aggfun=="median"){
        ts.ii<-cellStats(avhrrstack.ii, median, na.rm=TRUE)
      }
      
    }

    ts.out<-rep(NA, length(lakeyrs))
    ts.out[lakeyrs %in% 1989:2018]<-ts.ii[1989:2018 %in% lakeyrs]
    varnames<-rownames(lakedata[[ii]])
    lakedata[[ii]]<-rbind(lakedata[[ii]],ts.out)
    rownames(lakedata[[ii]])<-c(varnames,deparse(substitute(avhrrdata)))
    
  }
  return(list(lakeinfo=lakeinfo, lakedata=lakedata))
}


