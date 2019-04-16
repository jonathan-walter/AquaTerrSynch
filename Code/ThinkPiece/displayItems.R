## Display items for think piece

rm(list=ls())

## 1. a real-ish timeseries with multi-annual dynamics

tt = 1:30

#trend component
b1<-0.1
y1<-b1*tt-mean(b1*tt)
plot(y1,type="l")

#10-year oscillation
p1<-10
y2<-sin(seq(0,2*pi*(max(tt)/p1),length.out=length(tt)))
plot(y2, type="l")

#4-year oscillation
p2<-4
y3<-sin(seq(0,2*pi*(max(tt)/p2),length.out=length(tt)))
plot(y3, type="l")
