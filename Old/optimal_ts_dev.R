## working on optimal time segment idenfitication


xx<-rnorm(30)
xx[1]<-NA
xx[2]<-NA
xx[3]<-NA
xx[6]<-NA
xx[7]<-NA
xx[19]<-NA

plot(xx, type="l")

rle.xx<-rle(!is.na(xx))
rle.xx$values

lmin=20
max.na=floor(lmin*.1)

## Attempt 2
rle.xx<-rle(!is.na(xx))
if(cumsum(rle.xx$lengths[rle.xx$values])<(lmin-max.na)){stop("too few observations")}
for(step in 1:length(rle.xx$lengths)){
  
  
  
}


## Attempt 1 -- temporarily abandoned because I think I can do this in
## RLE space more efficiently
# for(step in 1:length(xx)){
#   if(step==1){tmp<-xx}
#   rle.tmp<-rle(is.na(tmp))
#   #remove anything with consecutive NAs
#   
#   #this will kill the ts if there are consecutive NAs late in the time series
#   if(any(rle.tmp$lengths[rle.tmp$values]>1)){
#     cut<-min(which(rle.xx$lengths[rle.xx$values]>1))
#     tmp<-tmp[-c(1:cumsum(rle.tmp$lengths[1:cut]))]
#     if(length(tmp<lmin)){break}
#     else{next}
#   }
#   #now start checking 
# }