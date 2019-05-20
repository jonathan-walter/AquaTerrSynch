load(url("https://github.com/limnoliver/CSI-Nutrient-Time-Series/blob/72c8269902e53c7ec6a2cfbe13a0239d13062dc8/Data/huc4.RData?raw=true"))
plot(huc4, lty = 1, lwd=1, border=TRUE, col = "lightgray")
head(huc4@data)

out<-data.frame(hu4_zoneid=huc4@data$ZoneID, huc4_code=as.character(huc4@data$HUC4), huc2_code=as.character(substr(huc4@data$HUC4,1,2)))

write.csv(out, "~/GitHub/AquaTerrSynch/AnalysisCode/match_huc_codes.csv", row.names=FALSE)

test<-read.csv("~/GitHub/AquaTerrSynch/AnalysisCode/match_huc_codes.csv", colClasses = 'character')
