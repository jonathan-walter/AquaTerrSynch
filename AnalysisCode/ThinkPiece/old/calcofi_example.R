#CALCOFI analyses

rm(list=ls())

library("wsyn")

dat<-read.csv("~/GitHub/AquaTerrSynch/AnalysisCode/ThinkPiece/org_larvae.csv", stringsAsFactors = F)

avg.spp<-aggregate(dat$larvae_10m2,by=list(dat$scientific_name),"mean")
avg.spp<-avg.spp[order(avg.spp$x,decreasing=T),]

nspp<-50
spp.sel<-avg.spp$Group.1[1:nspp]

dat.sel<-dat[dat$scientific_name %in% spp.sel,]

dat.sel$year<-substr(dat.sel$cruise,1,4)

dens.spp.annual<-aggregate(dat.sel$larvae_10m2, by=list(dat.sel$scientific_name,dat.sel$year), "mean")

colnames(dens.spp.annual)<-c("species","year","larvae_10m2.avg")

years<-min(dens.spp.annual$year):max(dens.spp.annual$year)

dmat<-matrix(NA, nspp, length(years))

for(spp in 1:nspp){
  for(yy in 1:length(years)){
    if(length(dens.spp.annual$larvae_10m2.avg[dens.spp.annual$species==spp.sel[spp] & 
                                              dens.spp.annual$year==years[yy]])!=0)
    {
      dmat[spp,yy]<-dens.spp.annual$larvae_10m2.avg[dens.spp.annual$species==spp.sel[spp] & 
                                                    dens.spp.annual$year==years[yy]]
    }
  }
}

colnames(dmat)<-years
rownames(dmat)<-spp.sel

dmat<-dmat[rowSums(is.na(dmat))<10,]

dmat<-dmat[rownames(dmat)!="Unidentified",]


for(ii in 1:nrow(dmat)){
  dmat[ii,is.na(dmat[ii,])]<-median(dmat[ii,],na.rm=T)
}

dmat.cln<-cleandat(dmat,years,clev=5)$cdat

wt.enmo<-wt(dmat.cln[1,],years)
wt.mepr<-wt(dmat.cln[2,],years)
wt.sasa<-wt(dmat.cln[3,],years)
wt.vilu<-wt(dmat.cln[4,],years)
wt.sexx<-wt(dmat.cln[5,],years)
wt.lest<-wt(dmat.cln[6,],years)
wt.sejo<-wt(dmat.cln[7,],years)
wt.stle<-wt(dmat.cln[8,],years)
wt.trsy<-wt(dmat.cln[9,],years)

# quartz()
# par(mfrow=c(5,2), mar=c(3,3,1,1))
# 
# plotmag(wt.enmo,title="Engraulis mordax")
# plotmag(wt.mepr,title="Merluccius productus")
# plotmag(wt.sasa,title="Sardinops sagax")
# plotmag(wt.vilu,title="Vinciguerria lucetia")
# plotmag(wt.sexx,title="Sebastes spp.")
# plotmag(wt.lest,title="Leuroglossus stilbius")
# plotmag(wt.sejo,title="Sebasets jordani")
# plotmag(wt.stle,title="Stenobrachius leucopsarus")
# plotmag(wt.trsy,title="Trachurus symmetricus")

enmoXsasa<-coh(dmat.cln[1,],dmat.cln[3,],years,norm="powall",sigma=1.01,sigmethod="fast")
enmoXsasa<-bandtest(enmoXsasa,c(2,4))
enmoXsasa<-bandtest(enmoXsasa,c(4,10))
enmoXsasa<-bandtest(enmoXsasa,c(10,26))
enmoXsasa<-bandtest(enmoXsasa,c(2,26))
print(enmoXsasa$bandp)

enmoXsejo<-coh(dmat.cln[1,],dmat.cln[7,],years,norm="powall",sigma=1.01,sigmethod="fast")
enmoXsejo<-bandtest(enmoXsejo,c(2,4))
enmoXsejo<-bandtest(enmoXsejo,c(4,10))
enmoXsejo<-bandtest(enmoXsejo,c(10,26))
enmoXsejo<-bandtest(enmoXsejo,c(2,26))
print(enmoXsejo$bandp)

enmoXmepr<-coh(dmat.cln[1,],dmat.cln[2,],years,norm="powall",sigma=1.01,sigmethod="fast")
enmoXmepr<-bandtest(enmoXmepr,c(2,4))
enmoXmepr<-bandtest(enmoXmepr,c(4,10))
enmoXmepr<-bandtest(enmoXmepr,c(10,26))
enmoXmepr<-bandtest(enmoXmepr,c(2,26))
print(enmoXmepr$bandp)

sexxXlest<-coh(dmat.cln[5,],dmat.cln[6,],years,norm="powall",sigma=1.01,sigmethod="fast")
sexxXlest<-bandtest(sexxXlest,c(2,4))
sexxXlest<-bandtest(sexxXlest,c(4,10))
sexxXlest<-bandtest(sexxXlest,c(10,26))
sexxXlest<-bandtest(sexxXlest,c(2,26))
print(sexxXlest$bandp)

#Plotting

## need to re-do using layout()

laymat<-matrix(1,nrow=2,ncol=11)
laymat[1,6:10]<-2
laymat[2,1:5]<-3
laymat[2,6:10]<-4
laymat[1,11]<-6
laymat[2,11]<-5

jetcolors <- c("#00007F", "blue", "#007FFF", "cyan", 
               "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")
colorfill<-grDevices::colorRampPalette(jetcolors)

# tiff("~/Box Sync/NSF EAGER Synchrony/Manuscripts/ThinkPiece/calcofi_example.tif",
#     units="in", height=5, width=6.5, res=300)

layout(laymat)

par(mar=c(2.1,2.1,1.5,1.1), xpd=F, oma=c(2.1,2.1,0,0))

plot(years,dmat[5,], type="l", main="Rockfish", xlab="",ylab="")
plot(years,dmat[6,], type="l", main="California smoothtongue", xlab="", ylab="")
plotmag(wt.sexx,colorbar=F, xaxs="r", zlim=c(0,3))
plotmag(wt.lest,colorbar=F, xaxs="r", zlim=c(0,3))

image(y=seq(0,3,length.out=50),matrix(seq(0,3,length.out=50),nrow=1,ncol=50),xaxt="n",yaxt="n",col=colorfill(50))
axis(2,at=pretty(Mod(c(wt.sexx$values,wt.lest$values)),n=5))

mtext("Time",1, outer=T, line=0.65)
mtext(expression(paste("Larvae 10m"^"-2")),2,outer=T, line=0.5, at=0.75)
mtext("Timescale (yr)",2, outer=T, line=0.5, at=0.25)

#dev.off()

