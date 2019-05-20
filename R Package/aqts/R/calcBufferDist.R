#' Create annualized time series from the LAGOSNE database
#' 
#' \code{calcBufferDist} calculates a buffer distance based on lake surface area
#' 
#' @param sa a numeric vector of lake surface areas (hectares)
#' @param ltype lake type; see details
#' @param minbuff the minimum buffer radius (meters)
#' 
#' @return \code{calcBufferDist} returns a numeric vector of buffer distances (meters)
#' 
#' @details Computed buffer distances are based on a log-linear relationship between lake surface area and watershed area,
#' parameterized on empirical data (Walter, et al., unpublished data) with a further simplifying assumption that lakes are circular. 
#' Setting a minimum buffer radius is useful for cases when lakes are small relative to the grain size of satellite data.
#' 
#' @author Jonathan Walter, \email{jaw3es@@virginia.edu}
#' 
#' @seealso \code{\link{makeIowaALMannualts},\link{addAVHRRannualts}}
#' 
#' @examples
#' #need to add some
#' 
#' @export

calcBufferDist<-function(sa, ltype, minbuff=2500){
  
  out<-rep(NA, length(sa))
  for(ii in 1:length(sa)){
    if(ltype[ii]==436){#reservoirs
      wa.ii = (10^(1.8 + 1.05*log10(sa[ii])))*10000 #watershed area in square meters
      buff.ii = sqrt(wa.ii/pi)
    }
    
    if(ltype[ii]==390){#lakes
      wa.ii = (10^(1.4 + 1.05*log10(sa[ii])))*10000
      buff.ii = sqrt(wa.ii/pi)
    }
    out[ii]<-buff.ii
  }
  
  out[out<minbuff]<-minbuff
  return(out)
}