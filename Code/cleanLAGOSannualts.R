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


cleanLAGOSannualts<-function(indat, ymin=20, maxNA=2, fill.method="median"){
  
  library(zoo)
  
  cleanreport<-indat$lakeinfo
  cleandat<-indat$lakedat
  
  for(lind in 1:length(indat$lakedata)){
    
    dat.lind<-indat$lakedata[[lind]]
    
    #Drop leading or trailing NAs
    dat.lind<-t(na.trim(t(dat.lind),sides="both",is.na="any"))
    
    #check for ymin 
    if(diff(range(as.numeric(colnames(xx)))) < ymin){
      cleandat<-cleandat[-lind]
    }
    
    else{
      #check for max NA
      for(vind in 1:nrow(dat.lind)){
        if(diff(range(as.numeric(colnames(xx)))) < ymin |
           sum(is.na(vind))
        )
      }
      
      
    }
    
    
  }
  
  
  
  
  
  
  
  #
  
  
  
}

