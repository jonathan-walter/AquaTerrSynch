## Display items for think piece

rm(list=ls())

## 1. a real-ish timeseries with multi-annual dynamics

tt = 1:30

#trend component
b1<-0.1
y1<-b1*tt-mean(b1*tt)
plot(y1,type="l")

#10-year oscillation
p1<-11
y2<-sin(seq(0,2*pi*(max(tt)/p1),length.out=length(tt)))
plot(y2, type="l")

#4-year oscillation
p2<-3
y3<-sin(seq(0,2*pi*(max(tt)/p2),length.out=length(tt)))
plot(y3, type="l")

#white noise
sig<-0.1
y4<-rnorm(length(tt),0,sig)

plot(tt,y1+y2+y3+y4, type="l")

ycomp<-y1+y2+y3+y4

df.lm2<-data.frame(x=seq(12.5,17.5,1), y=c(mean(ycomp[12:13]), mean(ycomp[13:14]), mean(ycomp[14:15]), mean(ycomp[15:16]),
                                           mean(ycomp[16:17]), mean(ycomp[17:18])))

lm1<-lm(ycomp[tt>=5 & tt<=10] ~ tt[tt>=5 & tt<=10])
lm2<-lm(y~x, data=df.lm2)
lm3<-lm(ycomp[tt>=20 & tt<=25] ~ tt[tt>=20 & tt<=25])


png("~/Box Sync/NSF EAGER Synchrony/Manuscripts/ThinkPiece/superimposed_signals_v2.png", units="in",
    height=5, width=3.25, res=300)

par(mfrow=c(4,1), mar=c(2,2,0,1), xpd=F, oma=c(2,0,1,0), mgp=c(1,1,0))
plot(tt,y1+y2+y3+y4, type="l",xaxt="n", xlab="", yaxt="n", ylab="y1+y2+y3")
rect(5,-4,10,4, col="skyblue2", xpd=F, border=NA)
rect(12.5,-4,17.5,4,col="palegreen2",xpd=F,border=NA)
rect(20,-4,25,4,col="gold",xpd=F,border=NA)
lines(tt,y1+y2+y3+y4)
lines(tt[tt>=5 & tt<=10], predict(lm1), col="royalblue3", lwd=3)
lines(seq(12.5,17.5,1), predict(lm2), col="seagreen", lwd=3)
lines(tt[tt>=20 & tt<=25], predict(lm3), col="goldenrod4", lwd=3)
axis(1,at=c(1,5,10,15,20,25,30),labels=FALSE)
axis(2,at=pretty(ycomp,5),labels=FALSE, xpd=F)
text(1,2.6,"A")

plot(tt,y3,type="l",xaxt="n",xlab="", yaxt="n")
axis(1,at=c(1,5,10,15,20,25,30),labels=FALSE)
axis(2,at=pretty(y3,5),labels=FALSE,xpd=F)
text(1,0.85,"B")

plot(tt,y2,type="l",xaxt="n",xlab="", yaxt="n")
axis(1,at=c(1,5,10,15,20,25,30),labels=FALSE)
axis(2,at=pretty(y2,5),labels=FALSE,xpd=F)
text(1,0.85,"C")

plot(tt,y1,type="l",xaxt="n",xlab="", yaxt="n")
axis(1,at=c(1,5,10,15,20,25,30),labels=TRUE)
axis(2,at=pretty(y1,5),labels=FALSE,xpd=F)
text(1,1.25,"D")

mtext("Timestep",1,outer=T,cex=0.8,line=0.4)

dev.off()


# ## 2. A long-timescale oscillation where if you catch a particular few years you get a very different answer.
# 
# p3<-12
# res=0.1
# tmax=20
# tt<-seq(1,tmax,length.out=tmax/res)
# 
# y5<-sin(seq(0,2*pi*(tmax/p3), length.out=tmax/res))
# 
# png("~/Box Sync/NSF EAGER Synchrony/Manuscripts/ThinkPiece/cycle_trends_v1.png", units="in",
#     height=2.5, width=6.5, res=300)
# par(mar=c(4,2.5,2,2), mgp=c(2,0.8,0))
# plot(tt,y5,type="l", ylab="", xlab="Timestep")
# 
# rect(4,-1.05,7,1.05, border="grey")
# rect(8,-1.05,11,1.05, border="grey")
# rect(12,-1.05,15,1.05, border="grey")
# 
# lm.w1<-lm(y5[tt>4&tt<7]~tt[tt>4&tt<7])
# lm.w2<-lm(y5[tt>8&tt<11]~tt[tt>8&tt<11])        
# lm.w3<-lm(y5[tt>12&tt<15]~tt[tt>12&tt<15])
# 
# lines(tt[tt>4&tt<7], predict(lm.w1), col="red")
# lines(tt[tt>8&tt<11], predict(lm.w2), col="red")
# lines(tt[tt>12&tt<15], predict(lm.w3), col="red")
# 
# dev.off()