train.control <- trainControl(method="cv", number = 10)
# Train the model
test <- train(accndvicoh.ts2 ~ shoredev + lake_area_ha +  maxdepth + pct.ag + chla + tsi.cat + 
              hu4_zoneid + cv.accndvi + pct.wetlands + prcp.normal,
              data = modvars.accndvi, method = "cforest",
              trControl = train.control)
print(test)

ttree<-ctree(accndvicoh.ts1 ~ shoredev + lake_area_ha +  maxdepth + pct.ag + chla + tsi.cat + doc +
               hu4_zoneid + cv.accndvi + pct.wetlands + prcp.normal,
             data = modvars.accndvi, controls=ctree_control(mincriterion = 0.9))

plot(ttree)

predtest<-predict(ttree,newdata=modvars.accndvi)

cor.test(modvars.accndvi$accndvicoh.ts1,predtest)

plot(predtest,modvars.accndvi$accndvicoh.ts1)
