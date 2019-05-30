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

png("~/Box Sync/NSF EAGER Synchrony/Manuscripts/ThinkPiece/superimposed_signals_v1.png", units="in",
    height=5, width=3.25, res=300)

par(mfrow=c(4,1), mar=c(2,4,0,1), xpd=T, oma=c(2,0,1,0))
plot(tt,y1+y2+y3+y4, type="l",xaxt="n", xlab="")
axis(1,at=c(1,5,10,15,20,25,30),labels=FALSE)
plot(tt,y1,type="l",xaxt="n",xlab="")
axis(1,at=c(1,5,10,15,20,25,30),labels=FALSE)
plot(tt,y2,type="l",xaxt="n",xlab="")
axis(1,at=c(1,5,10,15,20,25,30),labels=FALSE)
plot(tt,y3,type="l",xaxt="n",xlab="")
axis(1,at=c(1,5,10,15,20,25,30),labels=TRUE)
mtext("Timestep",1,outer=T,cex=0.8,line=0.4)

dev.off()


## 2. A long-timescale oscillation where if you catch a particular few years you get a very different answer.

p3<-12
res=0.1
tmax=20
tt<-seq(1,tmax,length.out=tmax/res)

y5<-sin(seq(0,2*pi*(tmax/p3), length.out=tmax/res))

plot(tt,y5,type="l")

rect(4,-1.05,7,1.05)
rect(8,-1.05,11,1.05)
rect(12,-1.05,15,1.05)

lm.w1<-lm(y5[tt>4&tt<7]~tt[tt>4&tt<7])
lm.w2<-lm(y5[tt>8&tt<11]~tt[tt>8&tt<11])        
