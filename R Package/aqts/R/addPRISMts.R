#' Add terrestrial AVHRR time series to limnological time series
#' 
#' \code{addPRISMts} extracts PRISM time series and appends them to limnological time series produced by,
#' e.g., \code{makeLAGOSannualts}.
#' 
#' @param lakelist a list containing \code{lakeinfo} and \code{lakedata}, as output from \code{makeLAGOSannualts}.
#' @param prismdata a raster stack of annual AVHRR metrics
#' @param var what variable is being handled? currently, ppt and tmean are accepted.
#' 
#' @return \code{addPRISMts} returns an object of class \code{list}. Slots are:
#' \item{lakeinfo}{A data frame of lake information.}
#' \item{lakedata}{A list of data frames containing the lake data. The PRISM data are appended here.}
#' 
#' @details Extracts the PRISM data using latitude-longitude coordinates provided by LAGOS
#' 
#' @author Jonathan Walter, \email{jaw3es@@virginia.edu}
#' 
#' @seealso \code{\link{addAVHRRannualts}}, \code{\link{makeIowaALMannualts}}
#' 
#' @examples
#' #need to add some
#' 
#' @export

addPRISMts<-function(lakelist, prismdata, var){
  
  library(raster)
  library(rgdal)
  library(sp)
  
  if(!var %in% c("ppt","tmean")){stop("var must be ppt or tmean")}
  
  lakeinfo<-lakelist$lakeinfo
  lakedata<-lakelist$lakedata
  
  #make lake points 
  lakepts<-SpatialPoints(coords=data.frame(x=lakeinfo$nhd_long,y=lakeinfo$nhd_lat),
                            proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
  prismproj<-crs(prismdata)
  lakepts.prj<-spTransform(lakepts, prismproj)
  
  
  #loop through lakes and extract AVHRR time series
  for(ii in 1:nrow(lakeinfo)){
    lake.ii<-lakepts.prj[ii,]
    lakeweather<-prismdata[lake.ii]
    if(var=="ppt"){
      df<-data.frame(year=substr(colnames(lakeweather),24,27),
                     val=as.numeric(lakeweather))
      df.agg<-aggregate(df$val,list(df$year),FUN="sum")
    }
    if(var=="tmean"){
      df<-data.frame(year=substr(colnames(lakeweather),26,29),
                     val=as.numeric(lakeweather))
      df.agg<-aggregate(df$val,list(df$year),FUN="mean")
    }
    
    colnames(df.agg)<-c("year","val")

    lakeyrs<-lakeinfo$start[ii]:lakeinfo$end[ii]
    wyrs<-df.agg$year
    ts.out<-rep(NA, length(lakeyrs))
    ts.out[lakeyrs %in% wyrs]<-df.agg$val[wyrs %in% lakeyrs]
    varnames<-rownames(lakedata[[ii]])
    lakedata[[ii]]<-rbind(lakedata[[ii]],ts.out)
    rownames(lakedata[[ii]])<-c(varnames,deparse(substitute(prismdata)))
    
  }
  return(list(lakeinfo=lakeinfo, lakedata=lakedata))
}


