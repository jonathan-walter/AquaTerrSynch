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
  if(!all(names(datalist) %in% c("lakedata","lakeinfo")){
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
    
    for(lind in 1:length(datalist$lakedata)){
      oldnames<-rownames(datalist$lakedata[[lind]])
      newnames<-NULL
      for(vind in 1:length(oldnames)){
        
      }
      rownames(datalist$lakedata[[lind]]<-newnames
    }

  }
  
  #Minnesota MPCA
  if(origin=="Minnesota MPCA"){

  }
  
  #Wisconsin DNR
  if(origin=="Wisconsin DNR"){

  }
  return(datalist)
}