# A function for plotting raw time series and wavelet power

#Arguments:
#indata = a dataframe where rows correspond to different variables, and columns correspond to timesteps
#filename = optionally, pass the root of a filename (without extension) and a PDF will be output.
# if NULL (the default), the active graphics device is used.

#Values:
#a plot showing the raw time series and wavelet power

plotTsWpow<-function(indata, title=NULL, filename=NULL){
 
  library(wsyn)
  library(RColorBrewer)
  
  if(nrow(indata)>10){warning("more than 10 variables in 'indata', plotting only 1:10")}
  
  #make plot layout matrix
  laymat=matrix(c(1,1,2:11), nrow=6, ncol=2, byrow=T)
  #get color pallette for time series plots
  pal<-brewer.pal(10, "Set3")
  
  nvar<-min(10,nrow(indata))
  years<-as.numeric(colnames(indata))
  
  dd<-t(scale(t(indata)))
  
  if(!is.null(filename)){pdf(paste0(filename,".pdf"),width=8.5,height=11)}
  
  par(mar=c(3.1,3.1,1.6,8.6), oma=c(0,0,2,0), xpd=T)
  layout(laymat)
  #make the time series plot
  plot(NA,NA,ylim=c(-4,4),xlim=c(min(years),max(years)), xlab="Year",
       ylab="Standardized variation")
    for(ii in 1:nvar){
      lines(years,dd[ii,],col=pal[ii],lwd=3)
    }
  legend("right",legend=rownames(indata)[1:nvar], lty=1, lwd=2, col=pal[1:nvar],
         inset=-.15)
  mtext(title,side=3,line=0.2,outer=T)
  par(mar=c(3.1,3.1,1.6,.6))
  
  dd.cln<-cleandat(dd,years,clev=5)$cdat
  for(ii in 1:nvar){
    wt.ii<-wt(dd.cln[ii,],years)
    plotmag(wt.ii, colorbar=F)
    mtext(rownames(dd)[ii],3)
  }
  if(!is.null(filename)){dev.off()}
}