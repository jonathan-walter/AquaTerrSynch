#Function for plotting coherence matrices

#Arguments
#cohres = a list containing 'cohmat', 'phimat', and 'sigmat' for a lake
#title = a title, generally the name of the lake
#filename = a filename to use for outputting a PDF. If NULL, the default graphics device is used.
#alpha = type-1 error rate, defaults to 0.05

#Values
#Plots a coherence matrix plot with indications of statistical significance and phase difference

plotCoherenceMatrices<-function(cohres, title, filename, alpha=0.05){
  
  
  if(!is.null(filename)){
    pdf(paste0(filename,".pdf"))
  }
  
  par(mar=c(7,7,2,1), fig=c(0,0.8,0,1))
  
  nvar=nrow(cohres$cohmat)
  
  pal=colorRampPalette(colors=c("blue","white","red"))
  
  graphics::image(1:nvar, 1:nvar, t(cohres$cohmat)[,nvar:1], 
                  xaxt="n",yaxt="n",col=pal(15), xlab="", ylab="")
  mtext(title, 3)
  axis(1, at=1:nvar, labels=rownames(cohres$cohmat), las=2)
  axis(2, at=1:nvar, labels=rev(rownames(cohres$cohmat)), las=1)
  
  xy<-expand.grid(nvar:1, 1:nvar)
  phi<-round(cohres$phimat/pi, 2)
  text(xy[,1],xy[,2],phi[nvar:1,nvar:1])
  
  sigmat2<-cohres$sigmat
  sigmat2[is.na(sigmat2)]<-1
  sig<-xy[sigmat2 < alpha,]
  points(sig[,2],sig[,1], pch="*", cex=2)
  
  par(fig=c(0.85,1,0,0.75),
      mar=c(7,1,1,1),
      new=T)
  image(matrix(1:15,nrow=1,ncol=15),col=pal(15),xaxt="n",yaxt="n")
  axis(2, at=c(0,0.5,1), labels=round(c(min(cohres$cohmat, na.rm=T),
                                  min(cohres$cohmat, na.rm=T)+(diff(range(cohres$cohmat,na.rm=T))/2),
                                  max(cohres$cohmat, na.rm=T)),2))
  mtext("coh",3)
  
  if(!is.null(filename)){
    dev.off()
  }
  
}
