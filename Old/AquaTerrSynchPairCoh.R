# Do pairwise wavelet coherences with significance testing 

#Arguments
#datalist = a data list, as output from cleanLAGOSannualts
#band =  a length-two numeric vector indicating a timescale band. Passed to 'coh' in package 'wsyn'.
#norm = the normalization of wavelet transforms. Defaults to "powall", "phase" may also be useful. Passed to 'coh' in package 'wsyn'.
#sigmethod = a method for testing statistical significance of coherence. See 'coh' in package 'wsyn' for details
#nrand = the number of surrogates upon which significance tests are based. Passed to 'coh' in package 'wsyn'.

#Details
#This function performs pairwise wavelet coherences between all variables in datalist$lakedata for each lake.

#Values
#a list containing:
#lakeinfo, a data frame of lake attributes
#cohres, a list (indexed by lake), where each element is a list containing:
#'cohmat', a matrix of wavelet coherences for band
#'phimat', a matrix of phase differences for band
#'sigmat', a matrix of p-values for band


AquaTerrSynchPairCoh<-function(datalist, band, norm="powall", sigmethod="fast", nrand=1000){

  library(wsyn)  
  
  lakeinfo<-datalist$lakeinfo
  lakedata<-datalist$lakedata
  
  cohres<-list()
  
  for(lind in 1:length(lakedata)){
    
    dat.lind<-lakedata[[lind]]
    dat.lind<-dat.lind[order(rownames(dat.lind)),]#sort variables alphabetically
    years<-as.numeric(colnames(dat.lind))
    
    cohmat<-matrix(NA, nrow(dat.lind), nrow(dat.lind))
    rownames(cohmat)<-rownames(dat.lind)
    colnames(cohmat)<-rownames(dat.lind)
    phimat<-cohmat
    sigmat<-cohmat
    
    dat.lind<-cleandat(dat.lind, years, clev=5)$cdat
    
    for(var.x in 2:nrow(dat.lind)){
      for(var.y in 1:(var.x-1)){
        tmpres<-coh(dat.lind[var.x,], dat.lind[var.y,], times=years,
                    norm=norm, sigmethod=sigmethod, nrand=nrand)
        tmpres<-bandtest(tmpres, band=band)
        cohmat[var.x,var.y]<-Mod(mean(tmpres$coher[tmpres$timescales > min(band) & tmpres$timescales < max(band)]))
        phimat[var.x,var.y]<-tmpres$bandp$mn_phs[1]
        sigmat[var.x,var.y]<-tmpres$bandp$p_val[1]
      }
    }
    
    cohres[[paste0(names(lakedata)[lind])]]<-list(cohmat=cohmat,phimat=phimat,sigmat=sigmat)
    
  }
  return(list(lakeinfo=lakeinfo,cohres=cohres))
}