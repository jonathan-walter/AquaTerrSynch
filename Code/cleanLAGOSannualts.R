# function for cleaning, filtering, and filling LAGOS annual time series

#Arguments
#indat =  an input data list, output from 'makeLAGOSannualts'
#ymin = minimum time series length
#maxNA = maximum number of missing observations
#fill.method = method for filling missing observations, currently only 'median' is implemented

#Details
#Currently, 'cleanLAGOSannualts' will fill missing values according to 'fill.method' only if
#missing values are non-consecutive. Leading and trailing NAs are trimmed from the time series.
#If no variables meet requirements, the whole lake is dropped from the output. If some, but not all
#variables meet requirements, data are returned with the offending variables omitted. 

#This function does some routine cleaning and filtering. Data should still be inspected and explored
#prior to analysis, and some manual bespoke cleaning may be necessary for certain lakes and variables.
#Especially for long but gappy time series, it may be possible to construct a usable time series after excluding
#gaps.


cleanLAGOSannualts<-function(indat, ymin=20, maxNA=2, fill.method="median"){
  
  library(zoo)
  
  cleandat<-indat$lakedat
  cleaninfo<-indat$lakeinfo
  
  for(lind in 1:length(indat$lakedata)){
    
    dat.lind<-indat$lakedata[[lind]]
    
    #Drop leading or trailing NAs
    dat.lind<-t(na.trim(t(dat.lind),sides="both",is.na="any"))
    
    #check for ymin
    droplakes<-NULL
    if(diff(range(as.numeric(colnames(dat.lind)))) < ymin){
      droplakes<-c(droplakes,lind)
    }
    
    else{
      #check for max NA
      dropvars<-NULL
      for(vind in 1:nrow(dat.lind)){
        if(sum(is.na(dat.lind[vind,]))>maxNA){
          dropvars<-c(dropvars,vind)
          next
        }
        #check if there are consecutive NAs
        rle.vind<-rle(is.na(dat.lind[vind,]))
        if(any(rle.vind$lengths[rle.vind$values]>1)){
          dropvars<-c(dropvars,vind)
        }
        if(!is.null(dropvars)){dat.lind<-dat.lind[-dropvars,]}
      }
      
      #fill time series if sporadic NAs
      for(vind in 1:nrow(dat.lind)){
        if(fill.method!="median"){stop("only fill.method=median is implemented")}
        else{
          dat.lind[vind,is.na(dat.lind[vind,])]<-median(dat.lind[vind], na.rm=T)
        }
      }
    }
    cleandat[[lind]]<-dat.lind
    if(length(dat.lind)==0){droplakes<-c(droplakes, lind)}
  }
  if(!is.null(droplakes)){
    cleandat<-cleandat[-droplakes]
    cleaninfo<-cleaninfo[-droplakes,]
    }
  #adjust start and end columns of lakeinfo
  
  for(ii in 1:nrow(cleaninfo)){
    cleaninfo$start[ii]<-min(as.numeric(colnames(cleandat[[ii]])))
    cleaninfo$end[ii]<-max(as.numeric(colnames(cleandat[[ii]])))
  }
  
  return(list(lakeinfo=cleaninfo,lakedat=cleandat,cleaned=TRUE))
  
}

test2<-cleanLAGOSannualts(test)

