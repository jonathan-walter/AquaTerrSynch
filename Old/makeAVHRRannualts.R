# Function for making AVHRR annual time series

#Inputs
#lakelist = a list containing 'lakeinfo' and 'lakeldata', as output from 'makeLAGOSannualts'
#avhrrdata = a raster stack of annual AVHRR metrics
#buffdist = the distance (in m) of the radius around each lake to draw terrestrial productivity information from
#aggfun = a function for spatially aggregating points within the buffer. Defaults to "mean", 
# "median" is also available


#Values
#Adds terrestrial ndvi data to the lake data

#Details

makeAVHRRannualts<-function(lakelist, avhrrdata, buffdist=5000, aggfun="mean"){
  
  library(raster)
  library(rgdal)
  library(sp)
  
  lakeinfo<-lakelist$lakeinfo
  lakedata<-lakelist$lakedata
  
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
    
    if(!aggfun %in% c("mean","median")){stop("aggfun argument must be 'mean' or 'median'.")}
    else if(aggfun=="mean"){
      ts.ii<-cellStats(avhrrstack.ii, mean, na.rm=TRUE)
    }
    else if(aggfun=="median"){
      ts.ii<-cellStats(avhrrstack.ii, median, na.rm=TRUE)
    }
    # names(ts.ii)<-as.character(1989:2015)
    # ts.ii<-ts.ii[names(ts.ii) %in% lakeinfo$start[ii]:lakeinfo$end[ii]]
    
    #avhrrout[[paste0(lakeinfo$gnis_name[ii])]]<-ts.ii
    lakeyrs<-lakeinfo$start[ii]:lakeinfo$end[ii]
    ts.out<-rep(NA, length(lakeyrs))
    ts.out[lakeyrs %in% 1989:2015]<-ts.ii[1989:2015 %in% lakeyrs]
    varnames<-rownames(lakedata[[ii]])
    lakedata[[ii]]<-rbind(lakedata[[ii]],ts.out)
    rownames(lakedata[[ii]])<-c(varnames,deparse(substitute(avhrrdata)))
    
  }
  return(list(lakeinfo=lakeinfo, lakedata=lakedata))
}

#test3<-makeAVHRRannualts(test$lakeinfo,avhrrdata)

