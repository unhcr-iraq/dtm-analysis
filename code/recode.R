## Labels have been manually reviewed
label <- read.csv("data/label.csv")

## let's recode the variable of the dataset using short label
names(master) <- label[, 3]
# names(master)
rm(label)

## Format date correctly
master$Date.Displacement <- as.Date(master$Date.Displacement, "%d-%m-%Y")
master$Current.Round.Date <- as.Date(master$Current.Round.Date, "%d %b %Y")

# master$Master.Families <- as.numeric(master$Master.Families)

master$Master.Families <- as.numeric(as.character(master$Master.Families))

master$PlaceID <- as.factor(master$PlaceID)
str(master)
# names(master)



# quick check on the total
master$total <- psum(master$IDPs.in.Camps.or.transit.camps, master$Rented.Hotel, master$Rented.House, master$IDP.Owned.House, master$With.Relative, master$With.HC.non.Relative, 
    master$School.Building, master$Mosques.Holly.Shrines, master$Abandoned.public.buildings.under.construction, master$Collective.centres, master$Informal.settlements, master$Military.Camps, 
    master$Unknown.or.other, na.rm = TRUE) * 6

# adding month.displacement for further graph
master$Month.Displacement <- format(master$Date.Displacement, "%b-%y")
# levels(master$Month.Displacement)
master$Month.Displacement <- factor(master$Month.Displacement, levels = c("Dec-13", "Jan-14", "Feb-14", "Mar-14", "Apr-14", "May-14", "Jun-14", "Jul-14", "Aug-14", "Sep-14"))

## Adding some aggregation
master$communal.setting <- psum(master$IDPs.in.Camps.or.transit.camps, master$School.Building, master$Mosques.Holly.Shrines, master$Abandoned.public.buildings.under.construction, 
    master$Collective.centres, master$Informal.settlements, master$Military.Camps, na.rm = TRUE) * 6

master$private.setting <- psum(master$Rented.Hotel, master$Rented.House, master$IDP.Owned.House, master$With.Relative, master$With.HC.non.Relative, na.rm = TRUE) * 6

master$pc.communal <- master$communal.setting/master$total
master$pc.camp <- (master$IDPs.in.Camps.or.transit.camps * 6)/master$total

# Recoding priority needs levels(master$priority.need.of.the.displaced.population.in.the.location)

## Parsing the variable and multiplying by total population
master$priority.Food <- as.numeric(with(master, ifelse(grepl("Food|Fis|FIand", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, master$priority.need.of.the.displaced.population.in.the.location), 
    paste0(1), 0))) * master$total

master$priority.Shelter <- as.numeric(with(master, ifelse(grepl("Shelter|Housing", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, master$priority.need.of.the.displaced.population.in.the.location), 
    paste0(1), 0))) * master$total

master$priority.NFI <- as.numeric(with(master, ifelse(grepl("NFI|NFIs|non-food|CRIs", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, master$priority.need.of.the.displaced.population.in.the.location), 
    paste0(1), 0))) * master$total

master$priority.Water <- as.numeric(with(master, ifelse(grepl("water", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, master$priority.need.of.the.displaced.population.in.the.location), 
    paste0(1), 0))) * master$total

master$priority.Cash <- as.numeric(with(master, ifelse(grepl("financial|Cash", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, master$priority.need.of.the.displaced.population.in.the.location), 
    paste0(1), 0))) * master$total

master$priority.Health <- as.numeric(with(master, ifelse(grepl("health|Healthy|Medical|Medicine|healthcare", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, master$priority.need.of.the.displaced.population.in.the.location), 
    paste0(1), 0))) * master$total

master$priority.Child <- as.numeric(with(master, ifelse(grepl("children|Child|", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, master$priority.need.of.the.displaced.population.in.the.location), 
    paste0(1), 0))) * master$total

# head(master) levels(master$Note) levels(master$assistance.previously.received) levels(master$sufficient.access.to.food)

# str(master)

# names(master)

################################### Size and type of displacement

# - # of IDP hosted in communal setting -- a score can be given based on the size on the population a classification is computed on 5 classes using different algorythm

master$class.total <- findCols(classIntervals(master$total, n = 5, style = "jenks"))
# master$class.total <-findCols(classIntervals(master$total, n = 5, style = 'quantile'))


# print(classIntervals(master$total, n = 5, style = 'jenks'), unique=FALSE) pal1 = brewer.pal(5,'Greens') plot(classIntervals(master$total, n = 5, style = 'jenks'),
# pal=pal1, main='Classification per total population')


# - % of IDP hosted in communal settings / private settings -- a score will be given based on this %

master$class.pccommunal <- findCols(classIntervals(master$pc.communal, n = 5, style = "jenks"))
# print(classIntervals(master$pc.communal, n = 5, style = 'jenks'), unique=FALSE)


################################### Impact on local population

# - a specific score could be given if idps are hosted in school building as this has a disruptive impact on local population
master$class.school <- findCols(classIntervals(master$School.Building, n = 5, style = "jenks"))


################################### Exclusion criteria

# - a specific score could be given if idps are already in camp (compared to other communal setting)
master$class.camp <- findCols(classIntervals(master$IDPs.in.Camps.or.transit.camps, n = 5, style = "jenks"))

# - non access to services (in priority FOOD, WATER, NFI) -- a score can be given if no access is reported


# - specific score depending on the date of displacement (pre-june /post-june). master$class.date <-findCols(classIntervals(as.numeric(master$Date.Displacement), n = 5,
# style = 'jenks'))

master$class.date <- as.numeric(master$Date.Displacement)

master$class.date <- as.numeric(with(master, ifelse(as.numeric(master$Date.Displacement) > as.numeric(as.Date("01-06-2014", "%d-%m-%Y")), paste0(0), 1)))
## New label to define wether the
master$datecut <- with(master, ifelse(as.numeric(master$Date.Displacement) > as.numeric(as.Date("01-06-2014", "%d-%m-%Y")), paste0("Post-June"), "Pre-June"))
## New label to define wether the
master$datecut <- factor(master$datecut, levels = c("Pre-June", "Post-June"))


# - specific flag if the location has previously received assistance levels(master$has.received.assistance)
master$Assistance.received <- as.numeric(with(master, ifelse(grepl("Yes", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, master$has.received.assistance), paste0(0), 
    1)))

# - specific flag when shelter has been defined as one of the priority need.

master$Shelterpriority <- as.numeric(with(master, ifelse(grepl("Shelter|Housing", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, master$priority.need.of.the.displaced.population.in.the.location), 
    paste0(1), 0)))


## identify locations where refugee camp already exist in the next 20kms!!!!


## Try to find place with same name but different master id extractplacecountplace <- aggregate(cbind(total) ~ Place , data = master, FUN = sum, na.rm = TRUE)
## extractplacecountmasterid <- aggregate(cbind(total) ~ Master.ID , data = master, FUN = sum, na.rm = TRUE)


#### 

## Multiplying the family number by the average
master$Unknown.or.other <- master$Unknown.or.other * 6
master$IDPs.in.Camps.or.transit.camps <- master$IDPs.in.Camps.or.transit.camps * 6
master$Military.Camps <- master$Military.Camps * 6
master$Informal.settlements <- master$Informal.settlements * 6
master$Collective.centres <- master$Collective.centres * 6
master$Abandoned.public.buildings.under.construction <- master$Abandoned.public.buildings.under.construction * 6
master$Mosques.Holly.Shrines <- master$Mosques.Holly.Shrines * 6
master$School.Building <- master$School.Building * 6
master$With.HC.non.Relative <- master$With.HC.non.Relative * 6
master$With.Relative <- master$With.Relative * 6
master$IDP.Owned.House <- master$IDP.Owned.House * 6
master$Rented.House <- master$Rented.House * 6
master$Rented.Hotel <- master$Rented.Hotel * 6

# Using the following reclassification

# IDPs.in.Camps.or.transit.camps ; IDPs.in.Camps.or.transit.camps Rented.Hotel ; Rented.or.Owned.Accomodation Rented.House ; Rented.or.Owned.Accomodation IDP.Owned.House
# ; Rented.or.Owned.Accomodation With.Relative ; Hosted.Accomodation With.HC.non.Relative ; Hosted.Accomodation School.Building ; Self-settled.in.public.building
# Mosques.Holly.Shrines ; Self-settled.in.public.building Abandoned.public.buildings.under.construction ; Self-settled.in.public.building Collective.centres ;
# Collective.centres Informal.settlements ; Informal.settlements Military.Camps ; Informal.settlements Unknown.or.other ; Unknown.or.other

master$Hosted.or.Owned.Accomodation <- psum(master$IDP.Owned.House, master$With.Relative, master$With.HC.non.Relative, na.rm = TRUE)

master$Rented.Accomodation <- psum(master$Rented.House, master$Rented.Hotel, na.rm = TRUE)

master$Organised.site <- psum(master$Collective.centres, master$Mosques.Holly.Shrines, master$Military.Camps, na.rm = TRUE)

master$Improvised.site <- psum(master$IDPs.in.Camps.or.transit.camps, master$Abandoned.public.buildings.under.construction, na.rm = TRUE)


master$Squatted.schools <- psum(master$School.Building, na.rm = TRUE)

master$Open.air <- psum(master$Informal.settlements, na.rm = TRUE)

master$base2 <- "Rest of Iraq"
master$base2[master$Governorate == "Dahuk"] <- "KRI"
master$base2[master$Governorate == "Erbil"] <- "KRI"
master$base2[master$Governorate == "Sulaymaniyah"] <- "KRI"

master$base2[master$District == "Khanaqin"] <- "Disputed Territories"
master$base2[master$District == "Akre"] <- "Disputed Territories"
master$base2[master$District == "Al-Shikhan"] <- "Disputed Territories"
master$base2[master$District == "Kifri"] <- "Disputed Territories"
# master$base2[grepl('Balance MoP', master$Place) ] <-'Balance MoP' master$base2[is.na(master$base) ] <-'-'
master$base2 <- as.factor(master$base2)




## Adding some aggregation

master$IDPs.in.Camps.or.transit.camps10 <- as.numeric(with(master, ifelse((master$IDPs.in.Camps.or.transit.camps > 60), paste0(master$IDPs.in.Camps.or.transit.camps), 0)))
master$School.Building10 <- as.numeric(with(master, ifelse((master$School.Building > 60), paste0(master$School.Building), 0)))
master$Mosques.Holly.Shrines10 <- as.numeric(with(master, ifelse((master$Mosques.Holly.Shrines > 60), paste0(master$Mosques.Holly.Shrines), 0)))
master$Abandoned10 <- as.numeric(with(master, ifelse((master$Abandoned.public.buildings.under.construction > 60), paste0(master$Abandoned.public.buildings.under.construction), 
    0)))
master$Collective.centres10 <- as.numeric(with(master, ifelse((master$Collective.centres > 60), paste0(master$Collective.centres), 0)))
master$Informal.settlements10 <- as.numeric(with(master, ifelse((master$Informal.settlements > 60), paste0(master$Informal.settlements), 0)))
master$Military.Camps10 <- as.numeric(with(master, ifelse((master$Military.Camps > 60), paste0(master$Military.Camps), 0)))


master$communal.setting10 <- psum(master$IDPs.in.Camps.or.transit.camps10, master$School.Building10, master$Mosques.Holly.Shrines10, master$Abandoned10, master$Collective.centres10, 
    master$Informal.settlements10, master$Military.Camps10, na.rm = TRUE)




write.csv(master, "out/master.csv", row.names = TRUE)


 
