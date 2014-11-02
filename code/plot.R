


## Plot per month of Arrival
rm(master.month)
master.month <- melt(master, id=c(43), measure=c(42))
master.month <- dcast(master.month,  Month.Displacement ~ variable, sum)

#master.month <- master.month[order(-master.month$Month.Displacement),]

rm(plotmonth1)
plotmonth1 <- ggplot(data=master.month, aes(x=Month.Displacement , y=total))+
  geom_bar(stat="identity",fill="#2a87c8",colour="#2a87c8")+
  labs(x = "Displacement Month", y = "Total IDP Ind.")+
  scale_y_continuous(labels=format_si())+
  ggtitle("Total IDPs per month of reported displacement")+
  theme_tufte(base_family="Helvetica")+
  theme(plot.title=element_text(face="bold", size=14),
        axis.title.x=element_text(face="plain", size=9),
        axis.title.y=element_text(face="plain", size=9),
        axis.text.x=element_text(face="italic", size=7),
        axis.text.y=element_text(face="bold", size=7))

# Save this!
ggsave("plot/bar/test.png", plotmonth1, width=8, height=6,units="in", dpi=300, bg = "transparent")

###############################################################################
## Plot per month of Arrival
rm(master.monthcom)
master.monthcom <- melt(master, id=c(43), measure=c(44,45))
#master.monthcom <- dcast(master.monthcom,  Month.Displacement ~ variable, sum)

master.monthcom <- aggregate(cbind(value) ~ Month.Displacement+variable,
                          data = master.monthcom, FUN = sum, na.rm = TRUE)

#master.month <- master.month[order(-master.month$Month.Displacement),]

# The palette with grey:
Palette <- c("#2a87c8", "#ccccff")


rm(plotmonth)
plotmonth <- ggplot(data=master.monthcom, aes(x=Month.Displacement , y=value, fill=variable))+
  geom_bar(stat="identity")+
  labs(x = "", y = "")+
  scale_fill_manual(values=Palette)+
  scale_y_continuous(labels=format_si())+
 # ggtitle("")+
  theme_tufte(base_family="Helvetica")+
  theme(plot.title=element_text(face="bold", size=14),
        axis.title.x=element_text(face="plain", size=9),
        axis.title.y=element_text(face="plain", size=9),
        axis.text.x=element_text(face="italic", size=7),
        axis.text.y=element_text(face="bold", size=7),
        legend.position = c(0.15, 0.8), legend.title=element_blank() 
        )

# Save this!
ggsave("plot/bar/plot-month.png", plotmonth, width=8, height=2,units="in", dpi=300, bg = "transparent")
ggsave("plot/bar/plot-month.svg", plotmonth, width=8, height=2,units="in", dpi=300, bg = "transparent")

#########################################################################
#########################################################################



## Plot per pre-june /postjune
rm(master.monthjune)
master.monthjune <- melt(master, id=c(60), measure=c(42))
master.monthjune <- dcast(master.monthjune, datecut ~ variable, sum)
rm(plotmonthjune)
plotmonthjune <- ggplot(data=master.monthjune, aes(x=datecut , y=total))+
  geom_bar(stat="identity",fill="#2a87c8",colour="#2a87c8")+
  labs(x = "Displacement Month", y = "Total IDP Ind.")+
  scale_y_continuous(labels=format_si())+
  ggtitle("Total IDPs Jan-June 2014 / post-June 2014")+
  theme_tufte(base_family="Helvetica")+
  theme(plot.title=element_text(face="bold", size=14),
        axis.title.x=element_text(face="plain", size=9),
        axis.title.y=element_text(face="plain", size=9),
        axis.text.x=element_text(face="italic", size=7),
        axis.text.y=element_text(face="bold", size=7))
ggsave("plot/bar/plot-monthjune.png", plotmonthjune, width=8, height=6,units="in", dpi=300, bg = "transparent")
rm(master.monthjune)
rm(plotmonthjune)

#########################################################################
#########################################################################

## Plot per information status
rm(master.status)
master.status <- melt(master, id=c(60,6), measure=c(42))
master.status <- dcast(master.status, datecut+Displacement.Info.Status ~ variable, sum)
rm(plotstatus)
plotstatus <- ggplot(data=master.status, aes(x=Displacement.Info.Status , y=total))+
  geom_bar(stat="identity",fill="#2a87c8",colour="#2a87c8")+
  labs(x = "Information status", y = "Total IDP Ind.")+  
  facet_wrap(  ~ datecut, ncol=3)+
  scale_y_continuous(labels=format_si())+
  ggtitle("Total IDPs  per information status")+
  theme_tufte(base_family="Helvetica")+
  theme(plot.title=element_text(face="bold", size=14),
        axis.title.x=element_text(face="plain", size=9),
        axis.title.y=element_text(face="plain", size=9),
        axis.text.x=element_text(face="italic", size=7),
        axis.text.y=element_text(face="bold", size=7))
ggsave("plot/bar/plot-status.png", plotstatus, width=8, height=6,units="in", dpi=300, bg = "transparent")
rm(master.status)
rm(plotstatus)

#########################################################################
#########################################################################


## Plot accomodation
#names(master)
rm(master.accomodationall)
master.accomodationall <- melt(master, id=c(7), measure=c(19:31))
#master.accomodationall <- dcast(master.accomodation, Place + Governorate +  District ~ variable, sum)
#names(master.accomodationall)
master.accomodationall$variable <- factor(master.accomodationall$variable, levels = c(
  "Unknown.or.other",
  "Informal.settlements",
  "School.Building",
  "Abandoned.public.buildings.under.construction",
  "IDPs.in.Camps.or.transit.camps",
  "Military.Camps",
  "Mosques.Holly.Shrines",
  "Collective.centres",
  "Rented.Hotel",
  "Rented.House",
  "With.HC.non.Relative",
  "With.Relative",
  "IDP.Owned.House"
))

rm(plotaccomodationall)
plotaccomodationall <- ggplot(data=master.accomodationall, aes(x=variable , y=value))+
  geom_bar(stat="identity",fill="#2a87c8",colour="#2a87c8")+
  labs(x = "Accomodation type", y = "Total IDP Ind.")+
  scale_y_continuous(labels=format_si())+
  ggtitle("Total IDPs per Accomodation type")+ 
  coord_flip()+
  theme_tufte(base_family="Helvetica")+
  theme(plot.title=element_text(face="bold", size=14),
        axis.title.x=element_text(face="plain", size=9),
        axis.title.y=element_text(face="plain", size=9),
        axis.text.x=element_text(face="italic", size=7),
        axis.text.y=element_text(face="bold", size=7))

ggsave("plot/bar/plot-accomodationall.png", plotaccomodationall, width=8, height=6,units="in", dpi=300, bg = "transparent")



#########################################################################
#########################################################################

## Let's focus on the main governorate for better leigibility of the facetted graph
master.accomodationallnorth <- subset(master.accomodationall, Governorate=="Anbar" |
                                   Governorate=="Kirkuk" |
                                   Governorate=="Ninewa"|
                                   Governorate=="Baghdad"|
                                   Governorate=="Najaf"|
                                   Governorate=="Diyalah")

rm(plotaccomodationallgov)
plotaccomodationallgov <- ggplot(data=master.accomodationallnorth, aes(x=variable , y=value))+
  facet_wrap(  ~ Governorate, ncol=3)+
  geom_bar(stat="identity",fill="#2a87c8",colour="#2a87c8")+
  labs(x = "Accomodation type", y = "Total IDP Ind.")+
  scale_y_continuous(labels=format_si())+
  theme(strip.text.x = element_text(size = 6, angle = 90),strip.text.y = element_text(size = 4, angle = 90))+
  ggtitle("Total IDPs per Accomodation type and Governorate")+
  coord_flip()+
  theme_tufte(base_family="Helvetica")+
  theme(plot.title=element_text(face="bold", size=14),
        axis.title.x=element_text(face="plain", size=9),
        axis.title.y=element_text(face="plain", size=9),
        axis.text.x=element_text(face="italic", size=7),
        axis.text.y=element_text(face="bold", size=7))

# Save this!
ggsave("plot/bar/plot-accomodationallgov.png", plotaccomodationallgov, width=8, height=6,units="in", dpi=300, bg = "transparent")


#########################################################################
#########################################################################

## Let's focus on the kri governorate for better leigibility of the facetted graph
master.accomodationallk <- subset(master.accomodationall, Governorate=="Dahuk" |
                                   Governorate=="Erbil" |
                                   Governorate=="Sulaymaniyah")
master.accomodationallkr <- dcast(master.accomodationallk, Governorate ~ variable, sum)
rm(plotaccomodationallgovk)
plotaccomodationallgovk <- ggplot(data=master.accomodationallk, aes(x=variable , y=value))+
  facet_wrap(  ~ Governorate, ncol=3)+
  geom_bar(stat="identity",fill="#2a87c8",colour="#2a87c8")+
  labs(x = "Accomodation type", y = "Total IDP Ind.")+
  scale_y_continuous(labels=format_si())+
  theme(strip.text.x = element_text(size = 6, angle = 90),strip.text.y = element_text(size = 4, angle = 90))+
  ggtitle("Total IDPs per Accomodation type and Governorate")+
  coord_flip()+
  theme_tufte(base_family="Helvetica")+
  theme(plot.title=element_text(face="bold", size=14),
        axis.title.x=element_text(face="plain", size=9),
        axis.title.y=element_text(face="plain", size=9),
        axis.text.x=element_text(face="italic", size=7),
        axis.text.y=element_text(face="bold", size=7))
#plotaccomodationallgovk <-plotaccomodationallgovk +   geom_text(aes(label = value))  #add labels at centroids

# Save this!
ggsave("plot/bar/plot-accomodationallgovk.png", plotaccomodationallgovk, width=8, height=6,units="in", dpi=300, bg = "transparent")
#########################################################################
#########################################################################


rm(master.accomodationallmonthk)
master.accomodationallmonthk <- melt(master, id=c(7,43), measure=c(19:31))
master.accomodationallmonthk <- subset(master.accomodationallmonthk, Governorate=="Dahuk" |
                                    Governorate=="Erbil" |
                                    Governorate=="Sulaymaniyah")

#levels(master.accomodation$variable)
## Change order
master.accomodationallmonthk$variable <- factor(master.accomodationallmonthk$variable, levels = c(
  "Unknown.or.other",
  "Informal.settlements",
  "School.Building",
  "Abandoned.public.buildings.under.construction",
  "IDPs.in.Camps.or.transit.camps",
  "Military.Camps",
  "Mosques.Holly.Shrines",
  "Collective.centres",
  "Rented.Hotel",
  "Rented.House",
  "With.HC.non.Relative",
  "With.Relative",
  "IDP.Owned.House"
))


rm(plotaccomodationallmonthk)
plotaccomodationallmonthk <- ggplot(data=master.accomodationallmonthk, aes(x=variable , y=value))+
  facet_grid( Governorate ~ Month.Displacement)+
  geom_bar(stat="identity",fill="#2a87c8",colour="#2a87c8")+
  labs(x = "Arrival Month", y = "Total IDP Ind.")+
  scale_y_continuous(labels=format_si())+
  #theme(element_text(size=8)) +
  ggtitle("Total IDPs per accomodation type and reported displacement date")+
  coord_flip()+
  theme_tufte(base_family="Helvetica")+
  theme(plot.title=element_text(face="bold", size=14),
        axis.title.x=element_text(face="plain", size=9),
        axis.title.y=element_text(face="plain", size=9),
        axis.text.x=element_text(face="italic", size=7),
        axis.text.y=element_text(face="bold", size=7))


ggsave("plot/bar/plot-accomodationallmonthk.png", plotaccomodationallmonthk, width=8, height=6,units="in", dpi=300, bg = "transparent")





rm(master.accomodationallmonth)
master.accomodationallmonth <- melt(master, id=c(43), measure=c(19:31))

#levels(master.accomodation$variable)
## Change order
master.accomodationallmonth$variable <- factor(master.accomodationallmonth$variable, levels = c(
  "Unknown.or.other",
  "Informal.settlements",
  "School.Building",
  "Abandoned.public.buildings.under.construction",
  "IDPs.in.Camps.or.transit.camps",
  "Military.Camps",
  "Mosques.Holly.Shrines",
  "Collective.centres",
  "Rented.Hotel",
  "Rented.House",
  "With.HC.non.Relative",
  "With.Relative",
  "IDP.Owned.House"
))


rm(plotaccomodationallmonth)
plotaccomodationallmonth <- ggplot(data=master.accomodationallmonth, aes(x=variable , y=value))+
  facet_wrap(  ~ Month.Displacement, ncol=3)+
  geom_bar(stat="identity",fill="#2a87c8",colour="#2a87c8")+
  labs(x = "Arrival Month", y = "Total IDP Ind.")+
  scale_y_continuous(labels=format_si())+
  #theme(element_text(size=8)) +
  ggtitle("Total IDPs per accomodation type and reported displacement date")+
  coord_flip()+
  theme_tufte(base_family="Helvetica")+
  theme(plot.title=element_text(face="bold", size=14),
        axis.title.x=element_text(face="plain", size=9),
        axis.title.y=element_text(face="plain", size=9),
        axis.text.x=element_text(face="italic", size=7),
        axis.text.y=element_text(face="bold", size=7))


ggsave("plot/bar/plot-accomodationallmonth.png", plotaccomodationallmonth, width=8, height=6,units="in", dpi=300, bg = "transparent")
#########################################################################
#########################################################################


rm(master.accomodation)
master.accomodation <- melt(master, id=c(7), measure=c(63:68))
#master.accomodation <- melt(master, id=c(1), measure=c(19,63,64,65,66,28,31))

#levels(master.accomodation$variable)
## Change order
master.accomodation$variable <- factor(master.accomodation$variable, levels = c(
  "Hosted.or.Owned.Accomodation",
  "Rented.Accomodation",
  "Organised.site",
  "Improvised.site",
  "Squatted.schools",
  "Open.air"
  ))


#master.accomodation <- aggregate(value  ~ Governorate, data = master.accomodation, FUN = sum, na.rm = TRUE)
#master.accomodation <- dcast(master.accomodation, Governorate ~ variable, sum)

#master.accomodationall <- melt(master, id=c(11,7,8), measure=c(19,63,64,65,28,31))
#master.accomodationall <- dcast(master.accomodation, Place + Governorate +	District ~ variable, sum)
#names(master.accomodationall)

# 

rm(plotaccomodation)
plotaccomodation <- ggplot(data=master.accomodation, aes(x=variable , y=value))+
  geom_bar(stat="identity",fill="#2a87c8",colour="#2a87c8")+
  labs(x = "Accomodation type", y = "Total IDP Ind.")+
  scale_y_continuous(labels=format_si())+
  ggtitle("Total IDPs per Accomodation type")+ 
  coord_flip()+
  theme_tufte(base_family="Helvetica")+
  theme(plot.title=element_text(face="bold", size=14),
        axis.title.x=element_text(face="plain", size=9),
        axis.title.y=element_text(face="plain", size=9),
        axis.text.x=element_text(face="italic", size=7),
        axis.text.y=element_text(face="bold", size=7))


# Save this!
ggsave("plot/bar/plot-accomodation.png", plotaccomodation, width=8, height=6,units="in", dpi=300, bg = "transparent")
#########################################################################
#########################################################################

rm(plotaccomodationgov)
plotaccomodationgov <- ggplot(data=master.accomodation, aes(x=variable , y=value))+
  facet_wrap(  ~ Governorate, ncol=3)+
  geom_bar(stat="identity",fill="#2a87c8",colour="#2a87c8")+
  labs(x = "Accomodation type", y = "Total IDP Ind.")+
  scale_y_continuous(labels=format_si())+
  theme(axis.text.y = element_text(size = 8))+
  ggtitle("Total IDPs per Accomodation type and Governorate")+
  coord_flip()+
  theme_tufte(base_family="Helvetica")+
  theme(plot.title=element_text(face="bold", size=14),
        axis.title.x=element_text(face="plain", size=9),
        axis.title.y=element_text(face="plain", size=9),
        axis.text.x=element_text(face="italic", size=7),
        axis.text.y=element_text(face="bold", size=7))

#plotaccomodationgov
# Save this!
ggsave("plot/bar/plot-accomodationgov.png", plotaccomodationgov, width=8, height=6,units="in", dpi=300, bg = "transparent")
#########################################################################
#########################################################################
master.accomodationm <- subset(master.accomodation,
                      Governorate=="Dahuk" |
                      Governorate=="Erbil" |
                      Governorate=="Anbar"|
                      Governorate=="Ninewa"|
                      Governorate=="Najaf"|
                      Governorate=="Kerbala")
master.accomodationm$Governorate <- factor(master.accomodationm$Governorate, levels = c(
  "Dahuk","Erbil","Anbar","Ninewa","Najaf","Kerbala"
))

master.accomodationm <- aggregate(cbind(value) ~ Governorate+variable,
                             data = master.accomodationm, FUN = sum, na.rm = TRUE)

master.accomodationm[["accom"]]<- ifelse(master.accomodationm[["variable"]] == "Hosted.or.Owned.Accomodation"|
                                           master.accomodationm[["variable"]] == "Rented.Accomodation",
                                         "priv", "com")


rm(plotaccomodationgovm)
plotaccomodationgovm <- ggplot(data=master.accomodationm, aes(x=variable , y=value, fill = accom))+
  facet_wrap(  ~ Governorate, ncol=3)+
  geom_bar(stat="identity")+
  scale_fill_manual(values = c("priv" = "#ccccff", "com" = "#2a87c8"))+
  labs(x = "", y = "")+
  scale_y_continuous(labels=format_si())+
  theme(axis.text.y = element_text(size = 8))+
  ggtitle("")+
  #geom_text(aes(label=value),hjust=1,colour="grey",size=4.5,face="bold",lineheight=.4, family="Helvetica")+
  coord_flip()+
  theme_tufte(base_family="Helvetica")+
  theme(plot.title=element_text(face="bold", size=14),
        axis.title.x=element_text(face="plain", size=9),
        axis.title.y=element_text(face="plain", size=9),        
        axis.text.x = element_text(face="bold", size=8),
        axis.text.y=element_text(face="plain", size=8),
        legend.title=element_blank(),legend.position="none"
        )


# Save this!
ggsave("plot/bar/plot-accomodation-main-gov.png", plotaccomodationgovm, width=8, height=6,units="in", dpi=300, bg = "transparent")
ggsave("plot/bar/plot-accomodation-main-gov.svg", plotaccomodationgovm, width=8, height=6,units="in", dpi=300, bg = "transparent")


#########################################################################
#########################################################################



rm(master.accomodationmonth)
master.accomodationmonth <- melt(master, id=c(43), measure=c(63:68))

#levels(master.accomodation$variable)
## Change order
master.accomodationmonth$variable <- factor(master.accomodation$variable, levels = c(
  "Hosted.or.Owned.Accomodation",
  "Rented.Accomodation",
  "Organised.site",
  "Improvised.site",
  "Squatted.schools",
  "Open.air"
))


rm(plotaccomodationmonth)
plotaccomodationmonth <- ggplot(data=master.accomodationmonth, aes(x=variable , y=value))+
  facet_wrap(  ~ Month.Displacement, ncol=3)+
  geom_bar(stat="identity",fill="#2a87c8",colour="#2a87c8")+
  labs(x = "Arrival Month", y = "Total IDP Ind.")+
  scale_y_continuous(labels=format_si())+
  #theme(element_text(size=8)) +
  ggtitle("Total IDPs per accomodation type and reported displacement date")+
  coord_flip()+
  theme_tufte(base_family="Helvetica")+
  theme(plot.title=element_text(face="bold", size=14),
        axis.title.x=element_text(face="plain", size=9),
        axis.title.y=element_text(face="plain", size=9),
        axis.text.x=element_text(face="italic", size=7),
        axis.text.y=element_text(face="bold", size=7))
# Save this!
ggsave("plot/bar/plot-accomodationmonth.png", plotaccomodationmonth, width=8, height=6,units="in", dpi=300, bg = "transparent")
#########################################################################
#########################################################################



rm(master.gov)
master.gov <- melt(master, id=c(7), measure=c(42))
#master.gov <- melt(master, id=c(1), measure=c(42))
master.gov <- dcast(master.gov, Governorate ~ variable, sum)
master.gov <- master.gov[order(-master.gov$total),]
master.gov$Governorate <- factor(master.gov$Governorate, levels = master.gov[order(master.gov$total), 1])

rm(plotgov)
plotgov <- ggplot(data=master.gov, aes(x=Governorate , y=total))+
  geom_bar(stat="identity",fill="#2a87c8",colour="#2a87c8")+
  labs(x = "Governorate of Asylum", y = "Total IDP Ind.")+
  scale_y_continuous(labels=format_si())+
  ggtitle("Total IDPs by Governorates")+
  coord_flip()+
  theme_tufte(base_family="Helvetica")+
  theme(plot.title=element_text(face="bold", size=14),
        axis.title.x=element_text(face="plain", size=9),
        axis.title.y=element_text(face="plain", size=9),
        axis.text.x=element_text(face="italic", size=7),
        axis.text.y=element_text(face="bold", size=7))
# Save this!
ggsave("plot/bar/plot-gov.png", plotgov, width=8, height=6,units="in", dpi=300, bg = "transparent")


## Stacked chart arrival per month
rm(master.govmonth)
master.govmonth <- melt(master, id=c(7,43), measure=c(42))
#master.govmonth <- melt(master, id=c(1,43), measure=c(42))
master.govmonth <- dcast(master.govmonth, Governorate + Month.Displacement ~ variable, sum)
## Reordering factor for the month
#str(master.govmonth)
#as.factor(master.govmonth$Month.Displacement)
#levels(master.govmonth$Month.Displacement)
#master.govmonth$Month.Displacement <- factor(master.govmonth$Month.Displacement, levels = c("Dec-13","Jan-14","Mar-14", "Apr-14","May-14","Jun-14","Jul-14","Aug-14"))
#levels(master.govmonth$Month.Displacement)
master.govmonth$Governorate <- factor(master.govmonth$Governorate, levels = master.gov[order(master.gov$total), 1])
#master.govmonth <- master.govmonth[order(master.govmonth$Month.Displacement),]

rm(plotgovmonth)
plotgovmonth <- ggplot(data=master.govmonth, aes(x=Governorate , y=total))+
  geom_bar(stat="identity",fill="#2a87c8",colour="#2a87c8")+
  facet_wrap(  ~ Month.Displacement, ncol=3)+
  labs(x = "Governorate of Asylum", y = "Total IDP Ind.")+
  scale_y_continuous(labels=format_si())+
  ggtitle("Total IDPs by Governorates and per Month of arrival")+
  scale_fill_stata()+
  coord_flip()+
  theme_tufte(base_family="Helvetica")+
  theme(plot.title=element_text(face="bold", size=14),
        axis.title.x=element_text(face="plain", size=9),
        axis.title.y=element_text(face="plain", size=9),
        axis.text.x=element_text(face="italic", size=7),
        axis.text.y=element_text(face="bold", size=7))

ggsave("plot/bar/plot-gov-month.png", plotgovmonth, width=8, height=6,units="in", dpi=300, bg = "transparent")

####################
## Plot per origin !

# Reorgnise the matrix in order to compute pivot tables
# Tuto: http://marcoghislanzoni.com/blog/2013/10/11/pivot-tables-in-r-with-melt-and-cast/
rm(master.origin)
# Melt and cast!
master.origin <- melt(master, id=c(5), measure=c(42))
#master.origin <- melt(master, id=c(6), measure=c(42))
master.origin <- dcast(master.origin, Origin.Governorate ~ variable, sum)
# Reorder the dataframe based on the total
master.origin <- master.origin[order(-master.origin$total),]

## Reorder the level of the factor based on the total -- order of the level are used in plot!
master.origin$Origin.Governorate <- factor(master.origin$Origin.Governorate, levels = master.origin[order(master.origin$total), 1])
## Reverse the order of those level -- might not be usefull if coord_flip
#master.origin$Origin.Governorate <- factor(master.origin$Origin.Governorate, levels=rev(levels(master.origin$Origin.Governorate)) )
## Check level
#levels(master.origin$Origin.Governorate)

rm(plotorigin)
plotorigin <- ggplot(data=master.origin, aes(x=Origin.Governorate , y=total))+
  geom_bar(stat="identity",fill="#2a87c8",colour="#2a87c8")+
  labs(x = "Governorate of Origin", y = "Total IDP Ind.")+
  scale_y_continuous(labels=format_si())+
  ggtitle("Total IDPs by Governorates of Origin")+
  coord_flip()+
  theme_tufte(base_family="Helvetica")+
  theme(plot.title=element_text(face="bold", size=14),
        axis.title.x=element_text(face="plain", size=9),
        axis.title.y=element_text(face="plain", size=9),
        axis.text.x=element_text(face="italic", size=7),
        axis.text.y=element_text(face="bold", size=7))

# Save this!
ggsave("plot/bar/plot-origin.png", plotorigin, width=8, height=6,units="in", dpi=300, bg = "transparent")
#########################################################################
#########################################################################


## Stacked chart arrival per month
rm(master.govorigin)
master.govorigin <- melt(master, id=c(7,5), measure=c(42))
#master.govorigin <- melt(master, id=c(1,6), measure=c(42))
master.govorigin <- dcast(master.govorigin, Governorate + Origin.Governorate ~ variable, sum)
## Reordering factor for the month
#str(master.govmonth)
#as.factor(master.govmonth$Month.Displacement)
#levels(master.govmonth$Month.Displacement)
#master.govmonth$Month.Displacement <- factor(master.govmonth$Month.Displacement, levels = c("Dec-13","Jan-14","Mar-14", "Apr-14","May-14","Jun-14","Jul-14","Aug-14"))
#levels(master.govmonth$Month.Displacement)
master.govorigin$Governorate <- factor(master.govorigin$Governorate, levels = master.gov[order(master.gov$total), 1])
#master.govmonth <- master.govmonth[order(master.govmonth$Month.Displacement),]

rm(plotgovorigin)
plotgovorigin <- ggplot(data=master.govorigin, aes(x=Governorate , y=total))+
  geom_bar(stat="identity",fill="#2a87c8",colour="#2a87c8")+
  facet_wrap(  ~ Origin.Governorate, ncol=3)+
  labs(x = "Governorate of Asylum", y = "Total IDP Ind.")+
  scale_y_continuous(labels=format_si())+
  ggtitle("Total IDPs by governorates and per governorate of origin")+
  coord_flip()+
  theme_tufte(base_family="Helvetica")+
  theme(plot.title=element_text(face="bold", size=14),
        axis.title.x=element_text(face="plain", size=9),
        axis.title.y=element_text(face="plain", size=9),
        axis.text.x=element_text(face="italic", size=7),
        axis.text.y=element_text(face="bold", size=7))
# Save this!
ggsave("plot/bar/plot-gov-origin.png", plotgovorigin, width=8, height=6,units="in", dpi=300, bg = "transparent")


## Stacked chart arrival per month from Origin
rm(master.originmonth)
master.originmonth <- melt(master, id=c(5,43), measure=c(42))
#master.originmonth <- melt(master, id=c(6,43), measure=c(42))
master.originmonth <- dcast(master.originmonth, Origin.Governorate + Month.Displacement ~ variable, sum)
#as.factor(master.originmonth$Month.Displacement)
#master.originmonth$Month.Displacement <- factor(master.originmonth$Month.Displacement, levels = c("Dec-13","Jan-14","Mar-14", "Apr-14","May-14","Jun-14","Jul-14","Aug-14"))

#master.originmonth <- master.originmonth[order(-master.originmonth$Month.Displacement),]
master.originmonth$Origin.Governorate <- factor(master.originmonth$Origin.Governorate,levels = master.origin[order(master.origin$total), 1])

rm(plotoriginmonth)
plotoriginmonth <- ggplot(data=master.originmonth, aes(x=Origin.Governorate , y=total))+
  geom_bar(stat="identity",fill="#2a87c8",colour="#2a87c8")+
  facet_wrap(  ~ Month.Displacement, ncol=3)+
  labs(x = "Governorate of Origin", y = "Total IDP Ind.")+
  scale_y_continuous(labels=format_si())+
  ggtitle("Total IDPs by Governorate of Origin")+
  coord_flip()+
  theme_tufte(base_family="Helvetica")+
  theme(plot.title=element_text(face="bold", size=14),
        axis.title.x=element_text(face="plain", size=9),
        axis.title.y=element_text(face="plain", size=9),
        axis.text.x=element_text(face="italic", size=7),
        axis.text.y=element_text(face="bold", size=7))

plotoriginmonth
# Save this!
ggsave("plot/bar/plot-origin-month.png", plotoriginmonth, width=8, height=6,units="in", dpi=300, bg = "transparent")
#########################################################################
#########################################################################

ggsave("plot/bar/plot-month1.png", plotmonth1, width=8, height=6,units="in", dpi=300, bg = "transparent")


### Remove everything!!
rm(master.gov)
rm(master.govorigin)
rm(master.origin)
rm(master.govmonth)
rm(master.accomodationall)
rm(master.accomodation)
rm(master.accomodationmonth)
rm(master.accomodationallmonth)
rm(master.originmonth)
rm(master.originmonth)
rm(master.month)
rm(master.accomodationall)
rm(master.accomodationallk)
rm(master.accomodationallkr)
rm(master.accomodationallmonthk)


rm(plotaccomodation)
rm(plotaccomodationgov)
rm(plotaccomodationallgov)
rm(plotaccomodationallgovk)
rm(plotaccomodationallmonth)
rm(plotaccomodationallmonthk)
rm(plotaccomodationmonth)
rm(plotaccomodationmonthgov)
rm(master.govorigin)
rm(plotorigin)
rm(plotoriginmonth)
rm(plotgov)
rm(plotmonth)
rm(plotgovmonth)
rm(plotgovorigin)
rm(plotlocpop)
rm(plotlocpopgov)
rm(plotaccomodationall)
rm(master.accomodationallnorth)
rm(master.accomodationm)
rm(master.monthcom)
rm(Palette)
rm(plotaccomodationgovm)
rm(plotmonth1)

