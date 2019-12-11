#' Aggregate significance across a timescale band
#' 
#' This is essentially a duplicate of 'bandtest' in package \code{'wsyn'} with minor functionality additions.
#' 
#' Computes the aggregate significance of coherence (\code{coh}) across a timescale band, 
#' accounting for non-independence of timescales. Also gets the 
#' average phase and average coherence across the band.
#' 
#' @param object An object of class \code{coh}, must have a non-\code{NA} 
#' \code{signif} slot
#' @param band A length-two numeric vector indicating a timescale band
#' @param ... Passed from the generic to specific methods. Not currently used.
#' 
#' @return \code{bandtest.coh} returns an object of the same class as its first input but with a
#' \code{bandp} slot added. Or if there was already a \code{bandp} slot, the output has a 
#' \code{bandp} slot with an additional row. For a \code{coh} object, the \code{bandp} slot 
#' is a data frame with four columns, the first two indicating the timescale band and the third 
#' an associated p-value for the test of coherence over that band. The fourth column is the 
#' average phase over the band.
#' 
#' @author Thomas Anderson, \email{anderstl@@gmail.com}, Jon Walter, \email{jaw3es@@virginia.edu}; Lawrence 
#' Sheppard, \email{lwsheppard@@ku.edu}; Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @references 
#' Sheppard, L.W., et al. (2016) Changes in large-scale climate alter spatial synchrony of aphid 
#' pests. Nature Climate Change. DOI: 10.1038/nclimate2881
#' 
#' @seealso \code{\link{coh}},
#' 
#' @examples 
#' #Example for a coh object
#' times<-(-3:100)
#' ts1<-sin(2*pi*times/10)
#' ts2<-5*sin(2*pi*times/3)
#' artsig_x<-matrix(NA,11,length(times)) #the driver
#' for (counter in 1:11)
#' {
#'   artsig_x[counter,]=ts1+ts2+rnorm(length(times),mean=0,sd=1.5)
#' }
#' times<-0:100
#' artsig_y<-matrix(NA,11,length(times)) #the driven
#' for (counter1 in 1:11)
#' {
#'   for (counter2 in 1:101)
#'   {
#'     artsig_y[counter1,counter2]<-mean(artsig_x[counter1,counter2:(counter2+2)])
#'   }
#' }
#' artsig_y<-artsig_y+matrix(rnorm(length(times)*11,mean=0,sd=3),11,length(times))
#' artsig_x<-artsig_x[,4:104]
#' artsig_x<-cleandat(artsig_x,times,1)$cdat
#' artsig_y<-cleandat(artsig_y,times,1)$cdat
#' cohobj<-coh(dat1=artsig_x,dat2=artsig_y,times=times,norm="powall",sigmethod="fast",nrand=1000,
#'             f0=0.5,scale.max.input=28)
#' cohobj<-bandtest.coh(cohobj,c(2,4))
#'   
#' #Example for a wlmtest object - see vignette
#'   
#' @export 

bandtest.coh<-function(object,band)
{
  #error checking
  if (any(is.na(object$signif)))
  {
    stop("Error in bandtest.coh: signif cannot be NA")
  }
  if (!is.numeric(band))
  {
    stop("Error in bandtest.coh: band must be numeric")
  }
  if (!is.vector(band))
  {
    stop("Error in bandtest.coh: band must be a length-two numeric vector")
  }
  if (length(band)!=2)
  {
    stop("Error in bandtest.coh: band must be a length-two numeric vector")
  }
  band<-sort(band)
  timescales<-get_timescales(object)
  if (band[1]>max(timescales) || band[2]<min(timescales))
  {
    stop("Error in bandtest.coh: band must include some of the timescales")
  }
  
  #add ranks if necessary
  if (any(is.na(object$ranks)))
  {
    object<-wsyn:::addranks(object)
  }
  
  #get the p-value
  x<-mean(object$ranks$coher[timescales>=band[1] & timescales<=band[2]]) #mean rank across timescales of interest, data
  sx<-apply(FUN=mean,
            X=object$ranks$scoher[,timescales>=band[1] & timescales<=band[2],drop=FALSE],
            MARGIN=1) #mean ranks, surrogates
  pval<-(sum(sx>=x)+1)/(length(sx)+1)
  
  #get the average phase
  x<-object$coher[timescales>=band[1] & timescales<=band[2]]
  mnphs<-wsyn:::mnphase(x)
  mncoh<-Mod(mean(x))
  
  #form the result and return it
  if (any(is.na(object$bandp)))
  {
    bandp<-data.frame(ts_low_bd=band[1],ts_hi_bd=band[2],p_val=pval,mn_phs=mnphs,mn_coh=mncoh)    
    object$bandp<-bandp
    return(object)
  }else
  {
    object$bandp[dim(object$bandp)[1]+1,]<-c(band,pval,mnphs,mncoh)
    return(object)
  }
}