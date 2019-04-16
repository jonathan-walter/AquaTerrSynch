#rm(list=ls())
library(roxygen2)
library(devtools)

setwd("~/GitHub/AquaTerrSynch/R package")

build("aqts")   # need a folder named 'Reumannplatz', in which a file named DESCRIPTION and a sub-folder named 'R'. This will creat a zip file
document("aqts") # this will create a file 'NAMESPACE' and a folder 'man'
check("aqts")
install("aqts")
#install_github("reumandc/reumannplatz",force=T)
library("aqts")

