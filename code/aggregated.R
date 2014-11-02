
# write.csv(master, file = 'out/master.csv')


### Extract for a dataviz with dc.js masterviz <- melt(master, id=c(5,7,8,32:36,38,19:31,43), measure=c(42)) names(masterviz)

# masterviz2 <- dcast(masterviz, Origin.Governorate + Governorate + District + sufficient.access.to.food + adequate.access.to.potable.water + in.need.of.CRIs +
# adequate.sanitation.facilities + access.to.functioning.health.facilities + has.received.assistance + IDPs.in.Camps.or.transit.camps + Rented.Hotel + Rented.House +
# IDP.Owned.House + With.Relative + With.HC.non.Relative + School.Building + Mosques.Holly.Shrines + Abandoned.public.buildings.under.construction + Collective.centres +
# Informal.settlements + Military.Camps + Unknown.or.other + Month.Displacement ~ variable, sum)

# write.csv(masterviz2, file = 'out/masterviz.csv') rm(masterviz2) rm(masterviz)

# extractplacecount <- aggregate(cbind( rep(1,length(total)), rep(1,length(IDPs.in.Camps.or.transit.camps)), rep(1,length(School.Building)),
# rep(1,length(Mosques.Holly.Shrines)), rep(1,length(Abandoned.public.buildings.under.construction)), rep(1,length(Collective.centres)),
# rep(1,length(Informal.settlements)), rep(1,length(Military.Camps)), rep(1,length(Unknown.or.other)) ) ~ Place , data = master, FUN = sum, na.rm = TRUE)




extractgov <- aggregate(cbind(total, IDPs.in.Camps.or.transit.camps, School.Building, Mosques.Holly.Shrines, Abandoned.public.buildings.under.construction, Collective.centres, 
    Informal.settlements, Military.Camps, Unknown.or.other, Hosted.or.Owned.Accomodation, Rented.Accomodation, Organised.site, Improvised.site, Squatted.schools, Open.air, 
    communal.setting, private.setting) ~ Governorate, data = master, FUN = sum, na.rm = TRUE)

write.csv(extractgov, file = "out/extractgov.csv")


irq_census_admin1 <- read.csv("data/irq_census_admin1.csv")

IraqEstimatedPopulation2014 <- read.csv("data/IraqEstimatedPopulation2014.csv")

# names(IraqEstimatedPopulation2014)
pop2014gov <- aggregate(cbind(male.urban, female.urban, total.urban, male.rural, female_rural, total.rural, male.pop, female.pop, total.pop) ~ gov_en, data = IraqEstimatedPopulation2014, 
    FUN = sum, na.rm = TRUE)


refugee_data_irq_26Aug2014 <- read.csv("data/refugee_data_irq_26Aug2014.csv")

# names(refugee_data_irq_26Aug2014) write.csv(levels(refugee_data_irq_26Aug2014$LocationLevel1Description), file =
# 'out/extractgovprogres.csv')

extractrefugee <- aggregate(cbind(Total) ~ LocationLevel1Description, data = refugee_data_irq_26Aug2014, FUN = sum, na.rm = TRUE)

rm(govnames)
mapgovnames <- read.csv("data/mapgovnames.csv")
# names(mapgovnames)

govnames <- merge(mapgovnames, extractrefugee, by.x = "Governorate.progres", by.y = "LocationLevel1Description", all.x = "TRUE")
govnames <- merge(govnames, irq_census_admin1, by.x = "governorate.census", by.y = "governorate", all.x = "TRUE")

govnames <- merge(govnames, extractgov, by.x = "Governorate.dtm", by.y = "Governorate", all.x = "TRUE")

# names(govnames)
govnames$idp.ref <- govnames$total/govnames$Total
govnames$idp.ref[is.na(govnames$idp.ref)] <- 0
govnames$idp.ref.rank <- findCols(classIntervals(govnames$idp.ref, n = 5, style = "jenks"))
govnames$idp.ref.rank <- as.factor(govnames$idp.ref.rank)

govnames$idp.pop <- govnames$total/govnames$population2011estimate
govnames$idp.pop[is.na(govnames$idp.pop)] <- 0
govnames$idp.pop.rank <- findCols(classIntervals(govnames$idp.pop, n = 5, style = "jenks"))


govnames$ref.pop <- govnames$Total/govnames$population2011estimate
govnames$ref.pop[is.na(govnames$ref.pop)] <- 0
govnames$ref.pop.rank <- findCols(classIntervals(govnames$ref.pop, n = 5, style = "jenks"))
govnames$ref.pop.rank <- as.factor(govnames$ref.pop.rank)

## rank total
govnames$total.rank <- findCols(classIntervals(govnames$total, n = 5, style = "jenks"))
govnames$total.rank <- as.factor(govnames$total.rank)

govnames$A1NameAlt1 <- govnames$Governorate.shp


## Rank per communal setting
govnames$class.communalsetting <- findCols(classIntervals(govnames$communal.setting, n = 5, style = "jenks"))
govnames$class.communalsetting[is.na(govnames$class.communalsetting)] <- 0

# Rank ratio communal private setting
govnames$comm.private <- govnames$communal.setting/govnames$private.setting
govnames$comm.private[is.na(govnames$comm.private)] <- 0
govnames$comm.private.rank <- findCols(classIntervals(govnames$comm.private, n = 5, style = "jenks"))

# Rank schools
govnames$class.school <- findCols(classIntervals(govnames$Squatted.schools, n = 5, style = "jenks"))
govnames$class.school[is.na(govnames$class.school)] <- 0


### Apply a caluclation to get a composite view: 1.  # of IDPs hosted in community settings – 20% 2.  Ratio of IDPs hosted in community settings / private settings – 30%
### 3.  # of IDPs hosted in schools – 25% 4.  Ratio (per governorate) of # of IDPs / # of local inhabitants – 25%

govnames$composite <- ((govnames$class.communalsetting * 0.2) + (govnames$comm.private.rank * 0.3) + (govnames$class.school * 0.25) + (govnames$idp.pop.rank * 0.25)) * 100
govnames$composite[is.na(govnames$composite)] <- 0
govnames$class.composite <- findCols(classIntervals(govnames$composite, n = 5, style = "jenks"))

## Convert rank to factor for further mapping!
govnames$class.school <- as.factor(govnames$class.school)
govnames$comm.private.rank <- as.factor(govnames$comm.private.rank)
govnames$class.communalsetting <- as.factor(govnames$class.communalsetting)
govnames$idp.pop.rank <- as.factor(govnames$idp.pop.rank)
govnames$class.composite <- as.factor(govnames$class.composite)

## Removeov district without IDPS
govnames <- govnames[!rowSums(is.na(govnames["total"])), ]

## Adding planning figures
total <- sum(govnames$total)

govnames$pc <- as.numeric(govnames$total/sum(govnames$total))
govnames$planning <- round(((govnames$total/sum(govnames$total)) * 1800000), digits = -1)
govnames$planningcommunal <- as.numeric(govnames$communal.setting/(govnames$private.setting + govnames$communal.setting))
govnames$planningcccm <- ceiling((govnames$communal.setting/(govnames$private.setting + govnames$communal.setting)) * govnames$planning)
affectedcccm <- sum(govnames$planningcccm)


govnames$class.planningcommunal <- as.factor(findCols(classIntervals(govnames$planningcommunal, n = 5, style = "fixed", fixedBreaks = c(0, 0.05, 0.1, 0.25, 0.5, 1))))
govnames$class.planningcommunal <- revalue(govnames$class.planningcommunal, c(`1` = "< 5%", `2` = "5% to 10%", `3` = "10% to 25%", `4` = "25% to 50%", `5` = "> 50%"))
# str(govnames)


govnames$class.planningcccm <- as.factor(findCols(classIntervals(govnames$planningcccm, n = 5, style = "jenks")))

govnames$class.composite <- findCols(classIntervals(govnames$composite, n = 5, style = "jenks"))

write.csv(govnames, file = "out/govnames.csv")


# - # of IDP in a district compared to # of refugees (not sure if you would like to include that - ?)  - # of IDP in a district compared to # of local inhabitants

# names(govnames) rm(govnamesmerge) govnamesmerge <- govnames[,c(1,35,37)] master1 <- merge(master,govnamesmerge, by.x = 'Governorate', by.y = 'Governorate.dtm')


# names(IraqEstimatedPopulation2014)
extractrefugeelevel2 <- aggregate(cbind(Total) ~ LocationLevel2Description, data = refugee_data_irq_26Aug2014, FUN = sum, na.rm = TRUE)
extractdistrict <- aggregate(cbind(total, IDPs.in.Camps.or.transit.camps, School.Building, Mosques.Holly.Shrines, Abandoned.public.buildings.under.construction, Collective.centres, 
    Informal.settlements, Military.Camps, Unknown.or.other) ~ District, data = master, FUN = sum, na.rm = TRUE)
pop2014dis <- aggregate(cbind(male.urban, female.urban, total.urban, male.rural, female_rural, total.rural, male.pop, female.pop, total.pop) ~ dis_en, data = IraqEstimatedPopulation2014, 
    FUN = sum, na.rm = TRUE)
labeladmin2 <- read.csv("data/label-admin2-iraq.csv")

# write.csv(extractdistrict, file = 'out/extractdis.csv')

disnames <- pop2014dis

rm(mapgovnames)
rm(refugee_data_irq_26Aug2014)
rm(irq_census_admin1)
rm(extractgov)
rm(extractdistrict)
rm(pop2014dis)
rm(pop2014gov)
rm(labeladmin2)
rm(extractrefugee)
rm(extractrefugeelevel2)
rm(IraqEstimatedPopulation2014)
rm(disnames) 
