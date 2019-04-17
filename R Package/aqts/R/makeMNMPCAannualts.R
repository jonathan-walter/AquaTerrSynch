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
#' Options for \code{infovars} are "Phosphorus as P", "Orthophosphate as P", "Inorganic nitrogen (nitrate and nitrite) as N",
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

dir<-"~/Box Sync/NSF EAGER Synchrony/Data/LAGOS Extended/MN_MPCA"

makeLAGOSannualts<-function(dir, tsvars=c("chla"), infovars=NULL, aggfun="gsmean", timespan=NULL,
                            minmos=3, minobs=5, lagosversion="1.087.1"){
  library(dplyr) #load library dependencies
  library(LAGOSNE)
  library(lubridate)
  library(stringr)
  
  flist<-list.files(path=dir)
  lagosids<-as.numeric(unlist(regmatches(flist, gregexpr("[[:digit:]]+", flist))))

  for(ff in 1:length(flist)){
    
    dat.ff<-read.csv(paste(dir,flist[ff],sep="/"),stringsAsFactors = F)
    
    
  }