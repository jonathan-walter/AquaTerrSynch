## working on optimal time segment idenfitication


xx<-rnorm(30)
# xx[1]<-NA
# xx[2]<-NA
# xx[3]<-NA
# xx[6]<-NA
xx[7]<-NA
xx[19]<-NA
#xx[20]<-NA
xx[28]<-NA
xx[29]<-NA

plot(xx, type="l")

rle.xx<-rle(!is.na(xx))
rle.xx$values

lmin=20
max.na=floor(lmin*.1)

## Attempt 3
rle.xx<-rle(!is.na(xx))
nsteps<-length(rle.xx$lengths)
xx.out<-xx
for(step in 1:nsteps){
  if(sum(rle.xx$lengths[rle.xx$values])<(lmin-max.na)){stop("too few observations")}
  #find out if there are consecutive NAs, and if so take the longer segment
  if(any(rle.xx$lengths[rle.xx$values==FALSE]>1)){
    cut<-min(which(rle.xx$lengths>1 & !rle.xx$values))
    end<-length(rle.xx$lengths)
    if(sum(rle.xx$lengths[1:cut]) > sum(rle.xx$lengths[(cut+1):length(rle.xx$lengths)])){
      #take the early segment
      xx.out<-xx.out[1:sum(rle.xx$lengths[1:(cut-1)])]
      rle.xx$lengths<-rle.xx$lengths[1:(cut-1)]
      rle.xx$values<-rle.xx$values[1:(cut-1)]
    }
    else if(sum(rle.xx$lengths[1:cut]) <= sum(rle.xx$lengths[(cut+1):length(rle.xx$lengths)])){
      #take the late segment
      xx.out<-xx.out[(sum(rle.xx$lengths[1:cut])+1):sum(rle.xx$lengths)]
      rle.xx$lengths<-rle.xx$lengths[(cut+1):end]
      rle.xx$values<-rle.xx$values[(cut+1):end]
    }
  }
  if(sum(rle.xx$lengths[rle.xx$values])>(lmin-max.na) & 
     sum(rle.xx$lengths[!rle.xx$values])<=max.na &
     max(rle.xx$lengths[!rle.xx$values])==1){break}
}


# ## Attempt 2--this works but need to translate back to subsetting the original time series
# rle.xx<-rle(!is.na(xx))
# nsteps<-length(rle.xx$lengths)
# for(step in 1:nsteps){
#   if(sum(rle.xx$lengths[rle.xx$values])<(lmin-max.na)){stop("too few observations")}
#   #find out if there are consecutive NAs, and if so take the longer segment
#   if(any(rle.xx$lengths[rle.xx$values==FALSE]>1)){
#     cut<-min(which(rle.xx$lengths[rle.xx$values==FALSE]>1))
#     end<-length(rle.xx$lengths)
#     if(sum(rle.xx$lengths[1:cut]) > sum(rle.xx$lengths[(cut+1):length(rle.xx$lengths)])){
#       #take the early segment
#       rle.xx$lengths<-rle.xx$lengths[1:cut]
#       rle.xx$values<-rle.xx$values[1:cut]
#     }
#     if(sum(rle.xx$lengths[1:cut]) <= sum(rle.xx$lengths[(cut+1):length(rle.xx$lengths)])){
#       #take the late segment
#       
#       rle.xx$lengths<-rle.xx$lengths[(cut+1):end]
#       rle.xx$values<-rle.xx$values[(cut+1):end]
#     }
#   }
#   if(sum(rle.xx$lengths[rle.xx$values])>(lmin-max.na) & 
#      sum(rle.xx$lengths[!rle.xx$values])<=max.na &
#      max(rle.xx$lengths[!rle.xx$values])==1){break}
# }
#   
  


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