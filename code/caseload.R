## planning figures

gvt <- read.csv("data/gvt.csv")

humanitariancaseload <- read.csv("data/HumanitarianCaseload.csv")

## Find locations in AOG

rm(masterloc)
masterloc <- aggregate(cbind( total,
                              #, Master.Families, IDPs.in.Camps.or.transit.camps , School.Building ,Mosques.Holly.Shrines, Abandoned.public.buildings.under.construction , Collective.centres ,
                              #Informal.settlements , Military.Camps , Unknown.or.other,
                              #Hosted.or.Owned.Accomodation,
                              #Rented.Accomodation,
                              #Organised.site,
                              #Improvised.site,
                              #Squatted.schools,
                              #Open.air,
                              communal.setting,
                              private.setting
                              ) ~ 
                         Governorate+District+Place+
                        base2+Longitude+Latitude,
                       data = master, FUN = sum #, na.rm = TRUE
                       )

checksum1 <- sum(master$total)
checksum2 <- sum(masterloc$total)

xy <- cbind(masterloc$Longitude, masterloc$Latitude)
masterlocp <- SpatialPointsDataFrame(xy , data= masterloc,proj4string=CRS("+proj=longlat"))
#proj4string(masterloc) <- CRS("+proj=longlat")

#coordinates(masterloc) <- c("Longitude", "Latitude")
#masterloc <- SpatialPointsDataFrame(coords=coordinates,proj4string=CRS("+proj=longlat"))

aog <- readShapePoly('data/shp/AG_Controlled.shp', proj4string=CRS("+proj=longlat"))


masterlocaog <- t(gIntersects(masterlocp, aog, byid = TRUE, prepared=TRUE, returnDense=TRUE))

masterlocaog <- as.data.frame(masterlocaog)
colnames(masterlocaog)[1] <- "in.aog"

masterlocaog$id = rownames(masterlocaog)
masterloc$id = rownames(masterloc)
masterloc <- join(masterloc, masterlocaog, by="id")

masterloc$within.aog <- as.numeric(ifelse(masterloc[["in.aog"]] == "TRUE",
                                  masterloc$total, 0))
masterloc$outside.aog <- as.numeric(ifelse(masterloc[["in.aog"]] == "FALSE",
                                             masterloc$total, 0))

masterloc$outside.aog.communal.setting <- as.numeric(ifelse(masterloc[["in.aog"]] == "FALSE",
                                           masterloc$communal.setting, 0))

outside.aog.communal.setting <- sum(masterloc$outside.aog.communal.setting)
total.cccm <- sum(masterloc$communal.setting)


rm(maploc)
maploc <-  ggplot(masterloc, aes(x = Longitude, y = Latitude)) + 
  coord_equal()+  
  stat_summary_hex(aes( x= masterloc$Longitude, y= masterloc$Latitude,
                        z = masterloc$communal.setting), data=masterloc, alpha = 7/10)+
  #coord_equal() +
  theme_tufte(base_family="Helvetica")+
  theme(plot.title=element_text(face="bold", size=14),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank(),axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        legend.position = c(0.15, 0.8))

ggsave("plot/caseload/maploc.png", maploc, width=6, height=4,units="in", dpi=300)










#########################################################


dtm <- aggregate(cbind( total, communal.setting,
                                 private.setting,
                                 within.aog,outside.aog
                          ) ~ Governorate+base2+District,data = masterloc, FUN = sum, na.rm = TRUE)

testsum1 <- sum(dtm$total)

dtmgvt <- merge(x=dtm, y= gvt, by="District", all.x=TRUE)



dtmgvtgov <- merge(x=dtmgvt, y= govnames, by.x="Governorate.x", by.y="Governorate.dtm", all.x=TRUE)

dtmgvtgovcase <- merge(x=dtmgvtgov, y= humanitariancaseload, by.x="Governorate.x", by.y="Governorate", all.x=TRUE)

testsum2 <- sum(dtmgvtgovcase$total.x)

dtmgvtgovcase$planning2 <-ceiling((dtmgvtgovcase$total.x/dtmgvtgovcase$total.y)*dtmgvtgovcase$planning)

testsum3 <- sum(dtmgvtgovcase$planning2)

dtmgvtgovcase$diffgvt <-(dtmgvtgovcase$total.x-dtmgvtgovcase$Individuals)/dtmgvtgovcase$total.x

dtmgvtgovcase$planningcccm2 <-ceiling((dtmgvtgovcase$communal.setting.x/(dtmgvtgovcase$private.setting.x+dtmgvtgovcase$communal.setting.x))*dtmgvtgovcase$planning2)

testsum4 <- sum(dtmgvtgovcase$planningcccm2)

dtmgvtgovcase$planningcommunal2 <-as.numeric(dtmgvtgovcase$communal.setting.x/(dtmgvtgovcase$private.setting.x+dtmgvtgovcase$communal.setting.x))

dtmgvtgovcase$communalplan.within.aog <- ceiling((dtmgvtgovcase$within.aog/dtmgvtgovcase$total.x)*dtmgvtgovcase$planning2*dtmgvtgovcase$planningcommunal2)
dtmgvtgovcase$communalplan.outside.aog <- ceiling((dtmgvtgovcase$outside.aog/dtmgvtgovcase$total.x)*dtmgvtgovcase$planning2*dtmgvtgovcase$planningcommunal2)

#ggplot(dtmgvtgovcase, aes(x=planningcommunal))  + geom_histogram(binwidth=.5)


#dtmgvtgovcase$class.planningcommunal <-as.factor(findCols(classIntervals(dtmgvtgovcase$planningcommunal, n = 5, style = "jenks")))


dtmgvtgovcase$class.planningcommunal2 <-as.factor(findCols(classIntervals(dtmgvtgovcase$planningcommunal2, n=5, style="fixed",
                                                               fixedBreaks=c(0,0.05,0.10,0.25,0.50,1))))
dtmgvtgovcase$class.planningcommunal2 <-revalue(dtmgvtgovcase$class.planningcommunal2, c("1"="< 5%","2"="5% to 10%",
                                                                   "3"="10% to 25%", "4"="25% to 50%",
                                                                   "5"="> 50%"))

rm(planning)
planning <- dtmgvtgovcase
names(planning)
planning <- planning[,-(10:42),drop=FALSE]
names(planning)
planning <- planning[,-(26:41),drop=FALSE]
names(planning)
planning <- planning[,-(32:40),drop=FALSE]
names(planning)

testsum5 <- sum(planning$total.x)
testsum6 <- sum(planning$planning2)
testsum7 <- sum(planning$planningcccm2)



write.csv(planning, 'out/planning.csv', row.names=TRUE)
################################################################

rm(planning.govtotal)
planning.govtotal <- aggregate(cbind(planningcccm2) ~ Governorate.x,data = planning, FUN = sum, na.rm = TRUE)


levels(planning.govtotal$Governorate.x) <- c(levels(planning.govtotal$Governorate.x), "Rest of Iraq")
planning.govtotal$Governorate.x[planning.govtotal$planningcccm2 < 2500 ]  <- "Rest of Iraq"
planning.govtotal$Governorate.x <- factor(planning.govtotal$Governorate.x, levels = planning.govtotal[order(planning.govtotal$planningcccm2), 1])
planning.govtotal <- aggregate(cbind(planningcccm2) ~ Governorate.x ,data = planning.govtotal, FUN = sum, na.rm = TRUE)

planning.govtotal$planningcccmround <- round(as.numeric(planning.govtotal$planningcccm2),digits=-1)

rm(planning.gov)
planning.gov <- melt(planning, id=c(1,2), measure=c(36,37))

## Remove district without IDPS
planning.gov <- planning.gov[complete.cases(planning.gov),]


#planning.gov <- dcast(planning.gov, Governorate.x ~ variable, sum)
## Sum up small values
planning.gov <- aggregate(cbind(value ) ~ Governorate.x+variable ,data = planning.gov, FUN = sum, na.rm = TRUE)

levels(planning.gov$Governorate.x) <- c(levels(planning.gov$Governorate.x), "Rest of Iraq")
planning.gov$Governorate.x[planning.gov$value < 2500 ]  <- "Rest of Iraq"


planning.gov <- aggregate(cbind(value ) ~ Governorate.x+variable ,data = planning.gov, FUN = sum, na.rm = TRUE)


totalccccm <- sum(planning.gov$value)

planning.gov$planningcccmround <- round(as.numeric(planning.gov$value),digits=-1)

## Remove district without IDPS
planning.gov <- planning.gov[complete.cases(planning.gov),]

planning.gov$Governorate.x <- factor(planning.gov$Governorate.x, levels = planning.gov[order(planning.gov$value), 1])


testsum <- sum(planning.gov$planningcccmround)



##########################################################################

rm(plotcccm)
plotcccm <- ggplot(data=planning.govtotal, aes(x=Governorate.x , y=planningcccm2#, fill=variable
                                          ))+
  geom_bar(stat="identity",fill="#2a87c8",colour="#2a87c8")+
  #facet_wrap(  ~ Month.Displacement, ncol=3)+
  labs(x = "", y = "")+
  scale_y_continuous(labels=format_si())+
  #scale_fill_manual(values = c("communalplan.within.aog" = "#ccccff", "communalplan.outside.aog" = "#2a87c8"))+
  #geom_hline(aes(yintercept=seq(50000,200000,300000)), colour="white")+
  geom_text(aes(label=planningcccmround),hjust=1,colour="grey",size=3.5,face="bold",lineheight=.4, family="Helvetica")+
  ggtitle("")+
  coord_flip()+
  theme_tufte(base_family="Helvetica")+
  theme(plot.title=element_text(face="bold", size=14),
        axis.title.x=element_text(face="plain", size=9),
        axis.title.y=element_text(face="plain", size=9),
        axis.text.x = element_blank(),axis.ticks = element_blank(),
        axis.text.y=element_text(face="bold", size=10))

ggsave("plot/caseload/plot-cccm.svg", plotcccm, width=6, height=4,units="in", dpi=300)

            
############################################################################################################################
############################################################################################################################
rm(irq_adm1)
irq_adm1 <- readShapePoly('data/shp/irq_admbnda_adm1_ocha_20140717.shp', proj4string=CRS("+proj=longlat"))
irq_adm1@data$id = rownames(irq_adm1@data)

rm(irq_adm1_f)
irq_adm1_f <- fortify(irq_adm1, region="id")
irq_adm1_f <-join(irq_adm1_f, irq_adm1@data, by="id")
irq_adm1_f <-join(x=irq_adm1_f, y=govnames, by="A1NameAlt1")

aog@data$id = rownames(aog@data)
aog_f <- fortify(aog, region="id")
aog_f <-join(aog_f, aog@data, by="id")

rm(mapgov)
mapgov<-  ggplot(irq_adm1_f, aes(x = long, y = lat, group = group
                                 #, fill=class.planningcommunal
                                 )) + 
  coord_equal()+
  #geom_polygon(data = irq_adm1_f, aes(fill=class.planningcommunal, group = group), alpha = 0.7) +
  #scale_fill_brewer(palette="Blues", name="% in Communal settings") + 
  geom_path(data = irq_adm1_f, aes(x = long, y = lat, group = group), color="black", size=0.1)+
  theme_tufte(base_family="Helvetica")+
  theme(plot.title=element_text(face="bold", size=14),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank(),axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        legend.position = c(0.15, 0.8))

#add proportional symbol at centroids
mapgov <- mapgov +
  geom_point(data = irq_adm1_f, aes(size = planningcccm, x = Longitude_c, y = Latitude_c, group = group),
             alpha = 0.7, color="#2a87c8")+  
  scale_size(name="IDPs in communal settings",labels=format_si())+
  ggtitle("")

## add layer for AOG
mapgov <- mapgov + geom_polygon(data = aog_f, aes(x = long, y = lat, group = group), alpha = 0.3, color ="red", fill="LightSeaGreen")

#add labels at centroids
mapgov <- mapgov + geom_text(data = irq_adm1_f, aes(label = short, x = Longitude_c, y = Latitude_c, group = group,
                                                    hjust=0, vjust=-0.5), size = 2.5)
mapgov <- mapgov + geom_text(data = irq_adm1_f, aes(label = planningcccm, x = Longitude_c, y = Latitude_c, group = group,
                                                    hjust=1, vjust=1), size = 2)

ggsave("plot/caseload/mapgov.png", mapgov, width=8, height=6,units="in", dpi=300)
#ggsave("plot/caseload/mapgov.svg", mapgov, width=8, height=6,units="in", dpi=300)


############################################################################################################################
############################################################################################################################


## Summary by district
rm(planning.dis)
planning.dis <- melt(planning, id=c(1,2), measure=c(38))


planning.dis.sub <- subset(planning.dis, value>=5000) 

planning.dis.sub$dis <- planning.dis.sub$District
planning.dis.sub$valueround <- as.integer(round(as.numeric(planning.dis.sub$value),digits=-2))

planning.dis.sub <- planning.dis.sub[order(-planning.dis.sub$valueround),]

#planning.dis$District <- factor(planning.dis$District, levels = planning.dis[order(planning.dis$value), 1])
#planning.dis$planningcccmround <- round(planning.dis$communalplan.outside.aog,digits=-1)
str(planning.dis.sub)

printTable <- planning.dis.sub[,-(3:5),drop=FALSE]
rownames(printTable) <- NULL
colnames(printTable) <- c("Governorates", "District","Planning figures")


#add table as an overlay
planning.dis.over <- qplot(1:10, 1:10, geom = "blank") +
  theme_tufte(base_family="Helvetica")+
  theme(line = element_blank(),
        text = element_blank()) +
  annotation_custom(grob = tableGrob(printTable,
                                     show.colnames = FALSE,
                                     #show.box = FALSE,
                                     # change font sizes:
                                     gpar.coltext = gpar(cex = 1.2),
                                     gpar.rowtext = gpar(cex = 1.2)),
                                     xmin = -Inf, xmax = Inf, 
                                     ymin = -Inf, ymax = Inf)

ggsave("plot/caseload/listdistrict1.png", 
       planning.dis.over, width=9, height=7,units="in", dpi=300, bg = "transparent")

#add table as an overlay
#planning.dis.over <- ggplot(planning.dis.sub) + 
#  annotation_custom(grob = tableGrob(planning.dis.sub[ ,5:6]),xmin = 42, xmax = 44, ymin = 37.5, ymax = 40)

########################################################
## Getting breakdown % in communal setting
rm(irq_adm2)
irq_adm2 <- readShapePoly('data/shp/irq_admbnda_adm2_ocha_20140717.shp', proj4string=CRS("+proj=longlat"))
irq_adm2@data$id = rownames(irq_adm2@data)

## Join with planning figures
irq_adm2@data <-merge(x=irq_adm2@data, y=planning.dis.sub, by.x="A2NameAlt1", by.y="District", all.x=TRUE)

#Createcentroid
irq_adm2.centroids <- as.data.frame(coordinates(irq_adm2))
irq_adm2.centroids$id = rownames(irq_adm2.centroids)
irq_adm2@data <-join(irq_adm2@data, irq_adm2.centroids, by="id")

irq_adm2_f <- fortify(irq_adm2, region="id")
irq_adm2_f <-join(irq_adm2_f, irq_adm2@data, by="id")

#str(irq_adm2_f)

rm(mapdistrict1)
mapdistrict1 <-  ggplot(irq_adm2_f, aes(x = long, y = lat, group = group)) + 
  coord_equal()+
  #geom_polygon(data = irq_adm2_f, aes(fill=class.planningcommunal), alpha = 0.7) +
  #scale_fill_brewer(palette="Blues", name="% in Communal settings") + 
  geom_path(data = irq_adm2_f, aes(x = long, y = lat, group = group), color="black", size=0.1)+
  theme_tufte(base_family="Helvetica")+
  theme(plot.title=element_text(face="bold", size=14),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank(),axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        legend.position = c(0.15, 0.8))

#add proportional symbol at centroids
mapdistrict1 <- mapdistrict1 +
  geom_point(aes(size = value, x = V1, y = V2, group = group), alpha = 0.6, color="coral1")+  
  scale_size(name="IDps in Communal setting",labels=format_si())+
  ggtitle("")

#add borders of admin1
mapdistrict1 <- mapdistrict1 +
  geom_path(data = irq_adm1_f, aes(x = long, y = lat, group = group), color="#2a87c8", size=0.5)

mapdistrict1 <- mapdistrict1 +
  geom_polygon(data = aog_f, aes(x = long, y = lat, group = group), alpha = 0.2, color ="red")

#mapdistrict1 <- mapdistrict1 +
#  geom_text(aes(label = valueround, x = V1, y = V2, group = group, hjust=0, vjust=0), size = 2.5) #add labels at centroids

mapdistrict1 <- mapdistrict1 +
  geom_text(aes(label = dis, x = V1, y = V2, group = group, hjust=1, vjust=-1), size = 2) #add labels at centroids

ggsave("plot/caseload/mapdistrict1.png", mapdistrict1, width=9, height=7,units="in", dpi=300)
#ggsave("plot/caseload/mapdistrict.svg", mapdistrict, width=9, height=7,units="in", dpi=300)


######################################################################


rm(mapdistrict)
mapdistrict <-  ggplot(irq_adm2_f, aes(x = long, y = lat)) + 
  coord_equal()+ 
  geom_path(data = irq_adm2_f, aes(x = long, y = lat, group = group), color="black", size=0.1)+
  theme_tufte(base_family="Helvetica")+
  theme(plot.title=element_text(face="bold", size=14),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank(),axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        #legend.position = c(0.15, 0.8)
        legend.position = "none")

## Add hexagrid for locations
mapdistrict <- mapdistrict +
  stat_summary_hex(data=masterloc, 
                   aes( x= masterloc$Longitude, y= masterloc$Latitude, z = masterloc$communal.setting),
                   alpha = 0.6)+
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444")

#add proportional symbol at centroids
mapdistrict <- mapdistrict +
  geom_point(data = irq_adm2_f, 
             aes(size = value, x = V1, y = V2, group = group), 
             alpha = 0.3, color="coral1")+  
  scale_size(name="IDps in Communal setting",labels=format_si())

#add borders of admin1
mapdistrict <- mapdistrict +
  geom_path(data = irq_adm1_f, 
            aes(x = long, y = lat, group = group), color="#2a87c8", size=0.5)

#Add AOG Area
mapdistrict <- mapdistrict +
  geom_polygon(data = aog_f, 
               aes(x = long, y = lat, group = group), alpha = 0.2, color ="red")
#+
#                scale_fill_gradient(low = "#ffffcc", high = "#ff4444")

#add table as an overlay
#mapdistrict <- mapdistrict +
#  annotation_custom(grob = tableGrob(planning.dis.sub[ ,5:6]),xmin = 42, xmax = 44, ymin = 37.5, ymax = 40)
 # annotate("text",  label = irq_adm2_f$dis, x = 42, y = 37.5,  size = 3, hjust = 0)
  #geom_text(data = irq_adm2_f, 
  #          aes(label = valueround, x = V1, y = V2, group = group, hjust=0, vjust=0), size = 2.5) 

#add labels at centroids
mapdistrict <- mapdistrict +
  geom_text(data = irq_adm2_f, 
            aes(label = dis, x = V1, y = V2, group = group, hjust=1, vjust=-1), size = 2) 

ggsave("plot/caseload/mapdistrict.png", mapdistrict, width=9, height=7,units="in", dpi=400, bg = "transparent")
#ggsave("plot/caseload/mapdistrict.svg", mapdistrict, width=9, height=7,units="in", dpi=300)
######################################################################

## add layer for AOG
#mapdistrict <- mapdistrict +
#  geom_polygon(data = aog_f, aes(x = long, y = lat, group = group), alpha = 0.3, color ="red")
rm(mapaog)
mapaog <-  ggplot(aog_f, aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = aog_f, aes(x = long, y = lat, group = group), alpha = 0.3, color ="red")
ggsave("plot/caseload/mapaog.png", mapaog, width=8, height=6,units="in", dpi=300)
######################################################################

access <- readShapePoly('data/shp/Access_level.shp', proj4string=CRS("+proj=longlat"))
access@data$id = rownames(access@data)
access_f <- fortify(access, region="id")
access_f <-join(access_f, access@data, by="id")

hub <- readShapePoly('data/shp/hups_v1.shp', proj4string=CRS("+proj=longlat"))
hub@data$id = rownames(hub@data)
hub_f <- fortify(hub, region="id")
hub_f <-join(hub_f, hub@data, by="id")

#plot(hub)
#plot(access)

#dist <- over(irq_adm2, hub)
#dist2 <- gIntersects(irq_adm2, hub, byid = FALSE, prepared=TRUE, returnDense=TRUE)

###



rm(HumanitarianCaseload)
rm(access_f)
rm(aog_f)
rm(dtm)
rm(dtmgvt)
rm(dtmgvtgov)
rm(dtmgvtgovcase)
rm(gvt)
rm(hub_f)
rm(humanitariancaseload)
rm(irq_adm1_f)
rm(irq_adm2.centroids)
rm(irq_adm2_f)
rm(irq_adm2_fm)
rm(masterloc)
rm(masterlocaog)
rm(masterlocation)
rm(planning)
rm(planning.dis)
rm(planning.gov)
rm(xy)
rm(Values)
rm(Palette)
rm(access)
rm(affectedcccm)
rm(aog)
rm(dist)
rm(hub)
rm(irq_adm1)
rm(irq_adm2)
rm(mapdistrict)
rm(mapdistrict1)
rm(mapgov)
rm(maplevel1_composite)
rm(masterlocp)
rm(plotaccomodationgovm)
rm(plotcccm)
rm(plotmonth1)
rm(testsum)
rm(total)
rm(totalccccm)




rm(planning.dis.sub)
rm(planning.gov2)
rm(planning.govtotal)
rm(checksum)
rm(checksum1)
rm(checksum2)
rm(mapaog)
rm(testsum1)
rm(testsum2)
rm(testsum3)
rm(testsum4)
rm(testsum5)
rm(testsum6)
rm(testsum7)
rm(test)
rm(theta)
rm(xo)
rm(yo)
rm(maploc)
rm(planning.dis.over)
rm(printTable)
