#### generate list of prioritised

## Link DTM data with site baseline

# the approach is to prioritize the locations to be assessed through 4 criteria: - Number of individuals per site - Number of site per location - Proximity to existing
# services (10km buffer to district capital) - Security (buffer around AOG area)

# load site baseline

rm(baseline)
baseline <- read.csv("data/dtm_baseline.csv")


# - Number of site per location

baseline$totalsites <- psum(baseline$sites.Religious.Buildings, baseline$sites.Unfinished.Buildings, baseline$sites.Open.Air, na.rm = TRUE)
baseline$totalfamilies <- psum(baseline$families.Religious.Buildings, baseline$families.Unfinished.Buildings, baseline$families.Open.Air, na.rm = TRUE)

## - Number of individuals per site
baseline$familysite <- baseline$totalfamilies/baseline$totalsites

## define rank based on jenk classification
baseline$class.totalsites <- findCols(classIntervals(baseline$totalsites, n = 5, style = "jenks"))
baseline$class.familysite <- findCols(classIntervals(baseline$familysite, n = 5, style = "jenks"))

## mergin with location from IOM dtm
baseline.merge <- merge(x = baseline, y = masterloc, by = "PlaceID", all.x = TRUE)

write.csv(baseline.merge, "out/baselinemerge.csv", row.names = TRUE)

## For reference IOM locations where we have the 3 categories

masterlocnorth <- subset(masterloc, (Governorate == "Dahuk" | Governorate == "Erbil" | Governorate == "Sulaymaniyah" | District == "Akre" | District == "Al-Shikhan" | District == 
    "Kifri" | District == "Khanaqin"))

masterlocnorthp <- subset(masterlocnorth, (Mosques.Holly.Shrines > 0 | Abandoned.public.buildings.under.construction > 0 | Informal.settlements > 0))
write.csv(masterlocnorthp, "out/masterlocnorthp.csv", row.names = TRUE)

masterlocnorthp.merge <- merge(x = masterlocnorthp, y = baseline, by = "PlaceID", all.x = TRUE)
write.csv(masterlocnorthp.merge, "out/masterlocnorthpmerge.csv", row.names = TRUE) 
