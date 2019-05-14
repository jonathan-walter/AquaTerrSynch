#' Fix variable names to match LAGOS conventions
#' 
#' \code{fixNamestoLAGOS} takes variables from disparate datasets and reconciles their names to match LAGOS conventions
#' 
#' @param datalist a data list, as output from \code{makeIowaALMannualts} or \code{makeMNMPCAannualts}.
#' @param origin a character string indicating the origin of the data. See details for options.
#' 
#' @return \code{fixNamestoLAGOS} returns an object of class \code{list}. Slots are:
#' \item{lakeinfo}{A data frame of lake information.}
#' \item{lakedata}{A list of data frames containing the lake data.}
#' 
#' @details Options of \code{origin} include "Iowa ALM", "Minnesota MPCA", and "Wisconsin DNR". 
#' 
#' @author Jonathan Walter, \email{jaw3es@@virginia.edu}
#' 
#' @seealso \code{\link{makeLAGOSannualts}}
#' 
#' @examples
#' #need to add some
#' 
#' @export

fixNamestoLAGOS<-function(datalist, origin){
  
  #Check inputs
  if(!all(names(datalist) %in% c("lakedata","lakeinfo"))){
    stop("invalid datalist object")
  }
  if(length(origin)!=1){
    stop("'origin' must have length==1")
    }
  if(!origin %in% c("Iowa ALM", "Minnesota MPCA", "Wisconsin DNR")){
    stop("invalid origin name")
  }
  
  datalist.out<-datalist
  
  #Iowa ALM
  if(origin=="Iowa ALM"){
    
    for(lind in 1:length(datalist.out$lakedata)){
      oldnames<-rownames(datalist.out$lakedata[[lind]])
      newnames<-NULL
      for(vind in 1:length(oldnames)){
        if(oldnames[vind]=="Chlorophyll"){newnames<-c(newnames,"chla")}
        if(oldnames[vind]=="TN"){newnames<-c(newnames,"tn")}
        if(oldnames[vind]=="TP"){newnames<-c(newnames,"tp")}
        if(oldnames[vind]=="SRP"){newnames<-c(newnames,"srp")}
        if(oldnames[vind]=="Secchi"){newnames<-c(newnames,"secchi")}
      }
      rownames(datalist.out$lakedata[[lind]])<-newnames
    }

  }
  
  #Minnesota MPCA
  if(origin=="Minnesota MPCA"){
    for(lind in 1:length(datalist.out$lakedata)){
    oldnames<-rownames(datalist.out$lakedata[[lind]])
    newnames<-NULL
      for(vind in 1:length(oldnames)){
        if(oldnames[vind]=="Chlorophyll a, uncorrected for pheophytin"){
          warning("using uncorrected Chlorophyll a")
          newnames<-c(newnames,"chla")
        }
        if(oldnames[vind]=="Chlorophyll a, corrected for pheophytin"){newnames<-c(newnames, "chla")}
        if(oldnames[vind]=="Depth, Secchi disk depth"){newnames<-c(newnames, "secchi")}
        if(oldnames[vind]=="Inorganic nitrogen (nitrate and nitrite) as N"){newnames<-c(newnames, "no2no3")}
        if(oldnames[vind]=="Kjeldahl nitrogen as N"){newnames<-c(newnames, "tkn")}
        #if(oldnames[vind]=="Phosphorous as P"){newnames<-c(newnames, "tp")} #<----------------Check this!
        #if(oldnames[vind]=="Nutrient-nitrogen as N"]){newnames<-c(newnames, "tn")}<-----------Check this!
      }
      rownames(datalist.out$lakedata[[lind]])<-newnames
    }
  
  }
  #Wisconsin DNR
  if(origin=="Wisconsin DNR"){
    for(lind in 1:length(datalist.out$lakedata)){
      oldnames<-rownames(datalist.out$lakedata[[lind]])
      newnames<-NULL
      for(vind in 1:length(oldnames)){
        #Group Seq No	 Start Date	 Secchi (Feet)	Secchi Hit Bottom?	Secchi (Meters)	Chlorophyll(ug/l)	Total Phosphorus(ug/l)	
        #Secchi TSI	Total Phosphorus TSI	Chlorophyll TSI	Lake Level	Staff Gauge	Appearance	Color	Perception
        if(oldnames[vind]=="Secchi..Feet."){
          warning("Converting feet to meters")
          datalist.out$lakedata[[lind]][vind, ]<-0.3048*datalist.out$lakedata[[lind]][vind, ]
          newnames<-c(newnames, "secchi")
        }
        if(oldnames[vind]=="Secchi..Meters."){newnames<-c(newnames, "secchi")}
        if(oldnames[vind]=="Chlorophyll.ug.l."){newnames<-c(newnames, "chla")}
        if(oldnames[vind]=="Total.Phosphorous.ug.l."){newnames<-c(newnames, "tp")}
      }
      rownames(datalist.out$lakedata[[lind]])<-newnames
    }
  }
  return(datalist.out)
}