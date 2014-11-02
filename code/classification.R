############ To analyse location, we need to sum them up as we have more than one record per location

rm(accomodation)
accomodation <- aggregate(cbind(total, Master.Families, IDPs.in.Camps.or.transit.camps, School.Building, Mosques.Holly.Shrines, Abandoned.public.buildings.under.construction, 
    Collective.centres, Informal.settlements, Military.Camps, Unknown.or.other, Hosted.or.Owned.Accomodation, Rented.Accomodation, Organised.site, Improvised.site, Squatted.schools, 
    Open.air, communal.setting, private.setting) ~ Governorate + base2, data = master, FUN = sum, na.rm = TRUE)



Open.air <- c("Open.Air", sum(accomodation$Open.air)/sum(accomodation$total))
Squatted.schools <- c("Squatted.schools", sum(accomodation$Squatted.schools)/sum(accomodation$total))
Improvised.site <- c("Improvised.site", sum(accomodation$Improvised.site)/sum(accomodation$total))
Organised.site <- c("Organised.site", sum(accomodation$Organised.site)/sum(accomodation$total))
Rented.Accomodation <- c("Rented.Accomodation", sum(accomodation$Rented.Accomodation)/sum(accomodation$total))
Hosted.or.Owned.Accomodation <- c("Hosted.or.Owned.Accomodation", sum(accomodation$Hosted.or.Owned.Accomodation)/sum(accomodation$total))
Unknown.or.other <- c("Unknown.or.other", sum(accomodation$Unknown.or.other)/sum(accomodation$total))

accomodation.sum <- rbind(Unknown.or.other, Hosted.or.Owned.Accomodation, Rented.Accomodation, Organised.site, Improvised.site, Squatted.schools, Open.air)
# accomodation.sum$value <- (as.numeric(accomodation.sum$V2))*1800000

# accomodation <- aggregate(cbind(total)~ Governorate, data = master, FUN = sum, na.rm = TRUE)





rm(masterloc)
masterloc <- aggregate(cbind(total, Master.Families, IDPs.in.Camps.or.transit.camps, School.Building, Mosques.Holly.Shrines, Abandoned.public.buildings.under.construction, 
    Collective.centres, Informal.settlements, Military.Camps, Unknown.or.other, private.setting, communal.setting, communal.setting10, Hosted.or.Owned.Accomodation, Rented.Accomodation, 
    Organised.site, Improvised.site, Squatted.schools, Open.air) ~ Governorate + District + Place + Longitude + Latitude + PlaceID + base2, data = master, FUN = sum, na.rm = TRUE)
write.csv(masterloc, "out/masterloc.csv", row.names = TRUE)

poptot <- sum(masterloc$total)
poptotfam <- sum(masterloc$total)/6
famtot <- sum(masterloc$Master.Families)



extractplacecounttotal <- aggregate(cbind(rep(1, length(total))) ~ Governorate, data = masterloc, FUN = sum, na.rm = TRUE)

masterlocschool <- subset(masterloc, School.Building > 0)
extractplacecountschool <- aggregate(cbind(rep(1, length(School.Building))) ~ Governorate, data = masterlocschool, FUN = sum, na.rm = TRUE)

extractplacecount <- aggregate(cbind(rep(1, length(total)), rep(1, length(IDPs.in.Camps.or.transit.camps)), rep(1, length(School.Building)), rep(1, length(Mosques.Holly.Shrines)), 
    rep(1, length(Abandoned.public.buildings.under.construction)), rep(1, length(Collective.centres)), rep(1, length(Informal.settlements)), rep(1, length(Military.Camps)), 
    rep(1, length(Unknown.or.other))) ~ Place, data = master, FUN = sum, na.rm = TRUE)
rm(extractplacecount)

masterloc$class.family.fixed <- as.factor(findCols(classIntervals(masterloc$Master.Families, n = 6, style = "fixed", fixedBreaks = c(0, 50, 100, 250, 500, 2000, 1e+05))))

masterloc$class.family.fixed <- revalue(masterloc$class.family.fixed, c(`1` = "0-49", `2` = "50-99", `3` = "100-249", `4` = "250-499", `5` = "500-2000", `6` = ">2000"))

masterloc$class.quartile <- cut(masterloc$total, breaks = quantile(masterloc$total, c(0, 0.25, 0.5, 0.75, 1)), include.lowest = TRUE)
extractquartile <- aggregate(cbind(rep(1, length(total))) ~ class.quartile, data = masterloc, FUN = sum, na.rm = TRUE)





rm(histo.totalclassfamily)
histo.totalclassfamily <- ggplot(masterloc, aes(x = as.factor(masterloc$class.family.fixed))) + geom_histogram(binwidth = 0.5) + labs(x = "Population class size") + ggtitle("Number of sites per population size class") + 
    theme_bw()
ggsave("plot/class/histogramm-totalclassfamily.png", histo.totalclassfamily, width = 8, height = 6, units = "in", dpi = 300)

rm(histo.totalclassfamilygov)
histo.totalclassfamilygov <- ggplot(masterloc, aes(x = as.factor(masterloc$class.family.fixed))) + geom_histogram(binwidth = 0.5) + labs(x = "Population class size") + ggtitle("Number of sites per population size class") + 
    facet_wrap(~Governorate, ncol = 3) + theme_bw()
ggsave("plot/class/histogramm-totalclassfamilygov.png", histo.totalclassfamilygov, width = 8, height = 6, units = "in", dpi = 300)



## Plot per month of Arrival
rm(masterloc.pop)
masterloc.pop <- melt(masterloc, id = c(1, 27), measure = c(8))
masterloc.pop <- dcast(masterloc.pop, Governorate + class.family.fixed ~ variable, sum)


rm(plotlocpop)
plotlocpop <- ggplot(data = masterloc.pop, aes(x = class.family.fixed, y = total)) + geom_bar(stat = "identity") + labs(x = "Population in the site", y = "Total IDP Ind.") + 
    scale_y_continuous(labels = format_si()) + ggtitle("Total IDPs per population size class in site") + theme_bw()
ggsave("plot/class/plotlocpop.png", plotlocpop, width = 8, height = 6, units = "in", dpi = 300)

############################################################# 

rm(plotlocpopgov)
plotlocpopgov <- ggplot(data = masterloc.pop, aes(x = class.family.fixed, y = total)) + geom_bar(stat = "identity") + labs(x = "Population in the site", y = "Total IDP Ind.") + 
    scale_y_continuous(labels = format_si()) + ggtitle("Total IDPs per population size class in site") + facet_wrap(~Governorate, ncol = 3) + theme_bw()
ggsave("plot/class/plotlocpopgov.png", plotlocpopgov, width = 8, height = 6, units = "in", dpi = 300)

rm(Hosted.or.Owned.Accomodation)
rm(Improvised.site)
rm(Open.air)
rm(Organised.site)
rm(Rented.Accomodation)
rm(Squatted.schools)
rm(Unknown.or.other)


rm(extractplacecount)
rm(extractplacecountschool)
rm(extractplacecounttotal)
rm(extractquartile)
rm(masterlocschool)
rm(masterloc.pop)

rm(plotlocpopgov)
rm(histo.totalclassfamilygov)
rm(histo.totalclassfamily)
rm(histo.classfamilygov)
rm(histo.classfamily)

rm(histo.totalclassfamily)
rm(histo.totalclassfamilygov)
rm(plotlocpop)


rm(poptotfam)
rm(famtot)
rm(poptot)
rm(accomodation.sum) 
