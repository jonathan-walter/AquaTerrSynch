#' Create annualized time series from MN MPCA data
#' 
#' \code{makeMNMPCAannualts} formats MN MPCA data to match output from \code{makeLAGOSannualts}.
#' 
#' @param dir a file path where the data files are stored
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
#' @details These are data partly contained in LAGOS-NE that we obtained in order to grow our list of lakes with >=20 years of data.
#' Currently, the only accepted value of \code{aggfun} is \code{"gsmean"}, corresponding to the growing season mean.
#' We consider the growing season to last May to September. Output \code{lakeinfo} is a data frame containing the numeric id code, name, 
#' and geographic coordinates of lakes, plus any further variables listed in \code{infovars}. Each element of \code{lakedata} corresponds to a lake
#' in \code{lakes} and is a data frame in which columns are variables in \code{tsvars} and rows correspond to years.
#' 
#' Options for \code{tsvars} are "Phosphorus as P", "Orthophosphate as P", "Inorganic nitrogen (nitrate and nitrite) as N",
#' "Ammonia-nitrogen as N", "Nitrate as N", "Chloride", "Total suspended solids", "Chlorophyll a, corrected for pheophytin",
#' "Chlorophyll a, uncorrected for pheophytin", "Depth, Secchi disk depth", "Temperature, water", "Specific conductance",
#' "pH", "Dissolved oxygen (DO)".
#' 
#' @author Jonathan Walter, \email{jaw3es@@virginia.edu}
#' 
#' @seealso \code{\link{makeLAGOSannualts}}
#' 
#' @examples
#' #need to add some
#' 
#' @export

makeMNMPCAannualts<-function(dir, tsvars="Chlorophyll a, corrected for pheophytin", infovars=NULL, aggfun="gsmean", timespan=NULL,
                            minmos=3, minobs=3, lagosversion="1.087.1"){
  library(dplyr) #load library dependencies
  library(LAGOSNE)
  library(lubridate)
  library(stringr)
  
  flist<-list.files(path=dir)
  lagosids<-as.numeric(unlist(regmatches(flist, gregexpr("[[:digit:]]+", flist))))
  flist<-flist[order(as.numeric(lagosids))]
  lagosids<-lagosids[order(as.numeric(lagosids))]
  
  #Pull lake info from LAGOSNE
  infovars<-unique(c("lagoslakeid","gnis_name","nhd_lat","nhd_long", infovars)) #make sure some vars are always pulled
  info<-lagosne_select(table="locus", vars=infovars) #pull lake info
  lakeinfo<-info[info$lagoslakeid %in% lagosids,] #extract lake information to output
  lakeinfo$gnis_name<-as.character(lakeinfo$gnis_name)
  lakeinfo$start=rep(NA,length(lagosids))
  lakeinfo$end=rep(NA,length(lagosids))
  
  #Build lake data 
  lakedata<-list()
  
  for(ff in 1:length(flist)){
    
    dat.ff<-read.csv(paste(dir,flist[ff],sep="/"),stringsAsFactors = F, na.strings = "(null)")
    dat.ff<-dat.ff[dat.ff$parameter %in% tsvars, colnames(dat.ff) %in% c("parameter","result","sampleDate")]
    
    if(nrow(dat.ff)==0){
      lakeinfo<-lakeinfo[lakeinfo$lagoslakeid != lagosids[ff], ]
      next
    }
    
    dat.ff$result<-as.numeric(dat.ff$result)
    #fix date format and convert to POSICXct
    date.tmp<-simplify2array(strsplit(as.character(dat.ff$sampleDate),"/")) #reformat the sampledate columnn
    dat.ff$sampleDate<-paste(str_pad(date.tmp[1,],width=2,pad=0),
                             str_pad(date.tmp[2,],width=2,pad=0),
                             date.tmp[3,],sep="/")
    dat.ff$sampleDate<-as.POSIXct(dat.ff$sampleDate,format="%m/%d/%Y")
    
    dat.ff<-dat.ff[order(dat.ff$sampleDate),]
    
    if(!is.null(timespan)){ #if necessary, limit data to selected timespan
      dat.ff<-dat.ff[year(dat.ff$sampleDate)>=min(timespan) &
                             year(dat.ff$sampleDate)<=max(timespan),]
    }
    
    lakeyears<-min(year(dat.ff$sampleDate)):max(year(dat.ff$sampleDate))
    
    for(vind in 1:length(tsvars)){
      
      tmp.vind<-dat.ff[dat.ff$parameter==tsvars[vind],]
      
      aggdat<-NULL
      for(yy in lakeyears){
        tmp.yy<-tmp.vind[year(tmp.vind$sampleDate)==yy,]
        
        if(length(tmp.yy)==0){ydat<-rep(NA, length(lakeyears))} 
        else{
          ydat<-NULL
          if(aggfun=="gsmean"){
            #compute variable
              if(nrow(tmp.yy)>=minobs & length(unique(month(tmp.yy$sampleDate)))>=minmos){
                ydat<-c(ydat,mean(tmp.yy$result, na.rm=TRUE))
              }
              else(ydat<-c(ydat,NA))
            }
          }
          aggdat<-cbind(aggdat,ydat) 
        }
    }#close variable loop
    aggdat[is.nan(aggdat)]<-NA
    rownames(aggdat)<-tsvars[!tsvars %in% c("sampledate","lagoslakeid")]
    colnames(aggdat)<-lakeyears
    lakedata[[paste0(lagosids[ff])]]<-aggdat
    lakeinfo$start[lakeinfo$lagoslakeid==lagosids[ff]]<-min(lakeyears)
    lakeinfo$end[lakeinfo$lagoslakeid==lagosids[ff]]<-max(lakeyears)
  }#close lake loop

  return(list(lakeinfo=lakeinfo, lakedata=lakedata))  
}

#test<-makeMNMPCAannualts(dir="/Users/jonathanwalter/Box Sync/NSF EAGER Synchrony/Data/LAGOS Extended/MN_MPCA")
