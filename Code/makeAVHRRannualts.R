# Function for making AVHRR annual time series

#Inputs
#lakeinfo = a lake information data frame, as output bu 'makeLAGOSannualts,' ideally 
# after cleaning by 'cleanLAGOSannualts'.
#avhrrdata = a raster stack of annual AVHRR metrics
#buffdist = the distance (in m) of the radius around each lake to draw terrestrial productivity information from
#avhrrmask = a binary raster layer indicating areas to exclude from terrestial productivity sample,
# if NULL, use all pixels within the buffer
#aggfun = a function for spatially aggregating points within the buffer. Defaults to "mean", 
# "median" is also available


#Values
#A list, consisting of "lakeinfo" and "AVHRRdata," which is, itself, a list of time series

#Details

makeAVHRRannualts<-function(lakeinfo, avhrrdata, buffdist=5000, avhrrmask=NULL, aggfun="mean"){
  
  library(raster)
  library(rgdal)
  library(sp)
  
  avhrrout<-list()
  
  #make lake points and buffer
  lakepts<-SpatialPoints(coords=data.frame(x=lakeinfo$nhd_long,y=lakeinfo$nhd_lat),
                            proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
  avhrrproj<-crs(avhrrdata)
  lakepts.prj<-spTransform(lakepts, avhrrproj)
  lakebuffer<-buffer(lakepts.prj, width=buffdist, dissolve=FALSE)
  
  #loop through lakes and extract AVHRR time series
  for(ii in 1:nrow(lakeinfo)){
    lakebuffer.ii<-lakebuffer[ii,]
    avhrrstack.ii<-mask(avhrrdata,lakebuffer.ii)
    if(!is.null(avhrrmask)){avhrrstack.ii<-mask(avhrrdata,avhrrmask)}
    
    if(!aggfun %in% c("mean","median")){stop("aggfun argument must be 'mean' or 'median'.")}
    else if(aggfun=="mean"){
      ts.ii<-cellStats(avhrrstack.ii, mean, na.rm=TRUE)
    }
    else if(aggfun=="median"){
      ts.ii<-cellStats(avhrrstack.ii, median, na.rm=TRUE)
    }
    # names(ts.ii)<-as.character(1989:2015)
    # ts.ii<-ts.ii[names(ts.ii) %in% lakeinfo$start[ii]:lakeinfo$end[ii]]
    
    avhrrout[[paste0(lakeinfo$gnis_name[ii])]]<-ts.ii
  }
  return(list(lakeinfo=lakeinfo, terrdata=avhrrout))
}

test3<-makeAVHRRannualts(test$lakeinfo,avhrrdata)

