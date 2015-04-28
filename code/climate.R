## Winterisation analysis based on bioclim dataset http://www.worldclim.org/ 
# temperature data are in °C * 10. This means that a value of 231 represents 23.1 °C.

# interesting variables to look at are:

# BIO5 = Max Temperature of Warmest Month 
# BIO6 = Min Temperature of Coldest Month 
# BIO10 = Mean Temperature of Warmest Quarter 
# BIO11 = Mean Temperature of Coldest Quarter
# BIO13 = Precipitation of Wettest Month 
# BIO19 = Precipitation of Coldest Quarter

# tmin = getData('worldclim', var='bio', res=0.5, lon=44, lat=32)

## Tiles can be downloaded here: http://www.worldclim.org/tiles.php

bio5 <- raster("data/shp/bio5_17.tif", native = T)
bio6 <- raster("data/shp/bio6_17.tif", native = T)
bio10 <- raster("data/shp/bio10_17.tif", native = T)
bio11 <- raster("data/shp/bio11_17.tif", native = T)
bio12 <- raster("data/shp/bio12_17.tif", native = T)
bio13 <- raster("data/shp/bio13_17.tif", native = T)
bio14 <- raster("data/shp/bio14_17.tif", native = T)
bio15 <- raster("data/shp/bio15_17.tif", native = T)
bio16 <- raster("data/shp/bio16_17.tif", native = T)
bio17 <- raster("data/shp/bio17_17.tif", native = T)
bio18 <- raster("data/shp/bio18_17.tif", native = T)
bio19 <- raster("data/shp/bio19_17.tif", native = T)

# Create a raster stack and extract values for all rasters.
bioall <- stack(bio5, bio6, bio10, bio11, bio12, bio13, bio14, bio15, bio16, bio17, bio18, bio19)
rm(bio5)
rm(bio6)
rm(bio10)
rm(bio11)
rm(bio12)
rm(bio13)
rm(bio14)
rm(bio15)
rm(bio16)
rm(bio17)
rm(bio18)
rm(bio19)


# plot(rdr)

# Analysing the raster cellStats(tmin, stat='mean') cellStats(tmin, stat='skew') quantile(tmin, probs = c(0.25, 0.50, 0.75))

## extraction information per location
rm(masterloc)
masterloc <- aggregate(cbind(total, Master.Families, IDPs.in.Camps.or.transit.camps, School.Building, Mosques.Holly.Shrines, Abandoned.public.buildings.under.construction, 
    Collective.centres, Informal.settlements, Military.Camps, Unknown.or.other) ~ Governorate + District + Place + Longitude + Latitude, data = master, FUN = sum, na.rm = TRUE)

# Converting the dataframe in a spatial dataframe
coordinates(masterloc) <- c("Longitude", "Latitude")


# extract values
rm(masterlocal.bioall)
masterlocal.bioall <- extract(bioall, masterloc, df = TRUE)
## reformat temperature
masterlocal.bioall$bio5_17 <- masterlocal.bioall$bio5_17/10
masterlocal.bioall$bio6_17 <- masterlocal.bioall$bio6_17/10
masterlocal.bioall$bio10_17 <- masterlocal.bioall$bio10_17/10
masterlocal.bioall$bio11_17 <- masterlocal.bioall$bio11_17/10

## Classify temperature

masterlocal.bioall$maxtemp <- as.factor(findCols(classIntervals(masterlocal.bioall$bio5_17, n = 11, style = "fixed", fixedBreaks = c(-10, -5, 0, 5, 10, 15, 20, 25, 30, 35, 
    40, 45, 50, 55))))
masterlocal.bioall$maxtemp <- revalue(masterlocal.bioall$maxtemp, c(`1` = "-10 to -5°C", `2` = "-5 to 0°C", `3` = "0 to 5°C", `4` = "5 to 10°C", `5` = "10 to 15°C", 
    `6` = "15 to 20°C", `7` = "20 to 25°C", `8` = "25 to 30°C", `9` = "30 to 35°C", `10` = "35 to 40°C", `11` = "40 to 45°C", `12` = "45 to 50°C", `13` = "50 to 55°C"))

masterlocal.bioall$mintemp <- as.factor(findCols(classIntervals(masterlocal.bioall$bio6_17, n = 11, style = "fixed", fixedBreaks = c(-10, -5, 0, 5, 10, 15, 20, 25, 30, 35, 
    40, 45, 50, 55))))
masterlocal.bioall$mintemp <- revalue(masterlocal.bioall$mintemp, c(`1` = "-10 to -5°C", `2` = "-5 to 0°C", `3` = "0 to 5°C", `4` = "5 to 10°C", `5` = "10 to 15°C", 
    `6` = "15 to 20°C", `7` = "20 to 25°C", `8` = "25 to 30°C", `9` = "30 to 35°C", `10` = "35 to 40°C", `11` = "40 to 45°C", `12` = "45 to 50°C", `13` = "50 to 55°C"))

masterlocal.bioall$meanwarm <- as.factor(findCols(classIntervals(masterlocal.bioall$bio10_17, n = 11, style = "fixed", fixedBreaks = c(-10, -5, 0, 5, 10, 15, 20, 25, 30, 
    35, 40, 45, 50, 55))))
masterlocal.bioall$meanwarm <- revalue(masterlocal.bioall$meanwarm, c(`1` = "-10 to -5°C", `2` = "-5 to 0°C", `3` = "0 to 5°C", `4` = "5 to 10°C", `5` = "10 to 15°C", 
    `6` = "15 to 20°C", `7` = "20 to 25°C", `8` = "25 to 30°C", `9` = "30 to 35°C", `10` = "35 to 40°C", `11` = "40 to 45°C", `12` = "45 to 50°C", `13` = "50 to 55°C"))

masterlocal.bioall$meancold <- as.factor(findCols(classIntervals(masterlocal.bioall$bio11_17, n = 11, style = "fixed", fixedBreaks = c(-10, -5, 0, 5, 10, 15, 20, 25, 30, 
    35, 40, 45, 50, 55))))
masterlocal.bioall$meancold <- revalue(masterlocal.bioall$meancold, c(`1` = "-10 to -5°C", `2` = "-5 to 0°C", `3` = "0 to 5°C", `4` = "5 to 10°C", `5` = "10 to 15°C", 
    `6` = "15 to 20°C", `7` = "20 to 25°C", `8` = "25 to 30°C", `9` = "30 to 35°C", `10` = "35 to 40°C", `11` = "40 to 45°C", `12` = "45 to 50°C", `13` = "50 to 55°C"))


## merge this with points
rm(masterlocation)
masterlocation <- aggregate(cbind(total, Master.Families, IDPs.in.Camps.or.transit.camps, School.Building, Mosques.Holly.Shrines, Abandoned.public.buildings.under.construction, 
    Collective.centres, Informal.settlements, Military.Camps, Unknown.or.other) ~ Governorate + District + Place + Longitude + Latitude, data = master, FUN = sum, na.rm = TRUE)
masterlocation <- merge(x = masterlocation, y = masterlocal.bioall, by = "row.names", all.x = TRUE)
# write to file
write.csv(masterlocation, "out/temperature-all.csv", row.names = TRUE)


############ For the camps

## We need to load the existing camp description with capacity and current population
idpcamp <- read.csv("data/idpcamp.csv")
coordinates(idpcamp) <- c("LONGITUDE", "LATITUDE")

# extract values
rm(idpcamp.bioall)
idpcamp.bioall <- extract(bioall, idpcamp, df = TRUE)
## reformat temperature
idpcamp.bioall$bio5_17 <- idpcamp.bioall$bio5_17/10
idpcamp.bioall$bio6_17 <- idpcamp.bioall$bio6_17/10
idpcamp.bioall$bio10_17 <- idpcamp.bioall$bio10_17/10
idpcamp.bioall$bio11_17 <- idpcamp.bioall$bio11_17/10

## Classify temperature
idpcamp.bioall$maxtemp <- as.factor(findCols(classIntervals(idpcamp.bioall$bio5_17, n = 11, style = "fixed", fixedBreaks = c(-10, -5, 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 
    50, 55))))
idpcamp.bioall$maxtemp <- revalue(idpcamp.bioall$maxtemp, c(`1` = "-10 to -5°C", `2` = "-5 to 0°C", `3` = "0 to 5°C", `4` = "5 to 10°C", `5` = "10 to 15°C", `6` = "15 to 20°C", 
    `7` = "20 to 25°C", `8` = "25 to 30°C", `9` = "30 to 35°C", `10` = "35 to 40°C", `11` = "40 to 45°C", `12` = "45 to 50°C", `13` = "50 to 55°C"))
idpcamp.bioall$mintemp <- as.factor(findCols(classIntervals(idpcamp.bioall$bio6_17, n = 11, style = "fixed", fixedBreaks = c(-10, -5, 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 
    50, 55))))
idpcamp.bioall$mintemp <- revalue(idpcamp.bioall$mintemp, c(`1` = "-10 to -5°C", `2` = "-5 to 0°C", `3` = "0 to 5°C", `4` = "5 to 10°C", `5` = "10 to 15°C", `6` = "15 to 20°C", 
    `7` = "20 to 25°C", `8` = "25 to 30°C", `9` = "30 to 35°C", `10` = "35 to 40°C", `11` = "40 to 45°C", `12` = "45 to 50°C", `13` = "50 to 55°C"))
idpcamp.bioall$meanwarm <- as.factor(findCols(classIntervals(idpcamp.bioall$bio10_17, n = 11, style = "fixed", fixedBreaks = c(-10, -5, 0, 5, 10, 15, 20, 25, 30, 35, 40, 
    45, 50, 55))))
idpcamp.bioall$meanwarm <- revalue(idpcamp.bioall$meanwarm, c(`1` = "-10 to -5°C", `2` = "-5 to 0°C", `3` = "0 to 5°C", `4` = "5 to 10°C", `5` = "10 to 15°C", `6` = "15 to 20°C", 
    `7` = "20 to 25°C", `8` = "25 to 30°C", `9` = "30 to 35°C", `10` = "35 to 40°C", `11` = "40 to 45°C", `12` = "45 to 50°C", `13` = "50 to 55°C"))
idpcamp.bioall$meancold <- as.factor(findCols(classIntervals(idpcamp.bioall$bio11_17, n = 11, style = "fixed", fixedBreaks = c(-10, -5, 0, 5, 10, 15, 20, 25, 30, 35, 40, 
    45, 50, 55))))
idpcamp.bioall$meancold <- revalue(idpcamp.bioall$meancold, c(`1` = "-10 to -5°C", `2` = "-5 to 0°C", `3` = "0 to 5°C", `4` = "5 to 10°C", `5` = "10 to 15°C", `6` = "15 to 20°C", 
    `7` = "20 to 25°C", `8` = "25 to 30°C", `9` = "30 to 35°C", `10` = "35 to 40°C", `11` = "40 to 45°C", `12` = "45 to 50°C", `13` = "50 to 55°C"))

## merge this with points
rm(idpcampbio)
idpcampbio <- read.csv("data/idpcamp.csv")
idpcampbio <- merge(x = idpcampbio, y = idpcamp.bioall, by = "row.names", all.x = TRUE)
# write to file
write.csv(idpcampbio, "out/temperaturecamp.csv", row.names = TRUE)

## Let's prepare a few plot

# ?extract


rm(bioall)
rm(idpcamp)
rm(masterloc)
# rm(masterlocation)
rm(idpcamp.bioall)
rm(masterlocal.bioall)



# Governorate maxtemp mintemp meanwarm meancold
masterlocation.temp <- melt(masterlocation, id = c(2, 30:33), measure = c(9:16))

masterlocation.temp.mintemp <- dcast(masterlocation.temp, mintemp ~ variable, sum)
write.csv(masterlocation.temp.mintemp, "out/masterlocationtemperaturemintemp.csv", row.names = TRUE)
masterlocation.temp.meancold <- dcast(masterlocation.temp, meancold ~ variable, sum)
write.csv(masterlocation.temp.meancold, "out/masterlocationtemperaturemeancold.csv", row.names = TRUE)

## Plot per accomodation and temp




plotcold <- ggplot(data = masterlocation.temp, aes(x = mintemp, y = value, fill = variable)) + geom_bar(stat = "identity") + labs(x = "Minimum temperature", y = "Total IDP Ind.") + 
    scale_y_continuous(labels = format_si()) + ggtitle("Total IDPs per Min temp") + theme_bw()
# theme_wsj()
ggsave("plot/climate/plot-cold.png", plotcold, width = 8, height = 6, units = "in", dpi = 300)


plotcoldgov <- ggplot(data = masterlocation.temp, aes(x = mintemp, y = value)) + geom_bar(stat = "identity") + labs(x = "Minimum temperature", y = "Total IDP Ind.") + scale_y_continuous(labels = format_si()) + 
    ggtitle("Total IDPs per Min temp") + facet_wrap(~variable, ncol = 3) + theme_bw()
# theme_wsj()
ggsave("plot/climate/plot-coldgov.png", plotcoldgov, width = 8, height = 6, units = "in", dpi = 300)


rm(plotcold)
rm(plotcoldgov)

rm(idpcampbio)
rm(masterlocation.melt)
rm(masterlocation.temp)
rm(masterlocation.temp.meancold)
rm(masterlocation.temp.mintemp)
rm(masterlocation.temp.sum)
rm(masterlocation.meancold) 
