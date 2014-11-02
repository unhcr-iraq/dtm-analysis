### Map camps


## code snippter to have a halo http://stackoverflow.com/questions/10686054/outlined-text-with-ggplot2

#theta <- seq(pi/8, 2*pi, length.out=16)
#xo <- diff(range(d$LONGITUDE))/200
#yo <- diff(range(d$LATITUDE))/200
#for(i in theta) {
#  p <- p + geom_text( 
#    bquote(aes(x=LONGITUDE+.(cos(i)*xo),y=LATITUDE+.(sin(i)*yo),label=LABEL)), 
#    size=12, colour='black' )
#}
# p <- p + geom_text( aes(label=LABEL), size=12, colour='white' )

rm(idpcamp)
#idpcamp <- read.csv("~/unhcr_r_project/displacement/data/Iraq-CCCM-Camp-CollectiveCenter-9October2014.csv")

idpcamp <- read.csv("~/unhcr_r_project/displacement/data/Official-camp-WeeklyReport-Data - to-geojson.csv")
idpcamp$Total.availability <- idpcamp$absorption

# coordinates(idpcamp) <- c("LONGITUDE", "LATITUDE")

idpcamp <- subset(idpcamp, (loct=="IDPs Camps Under Development" |
                              loct=="IDPs Camps Suggested Site Location" |
                              loct=="IDPs Camps Open"))

idpcamp$loct <- factor(idpcamp$loct, levels = c("IDPs Camps Open","IDPs Camps Under Development" ,"IDPs Camps Suggested Site Location"))


### need to merge Agricultural research and bersive for better legibility on the map - 
# Add the new location name in the factors
## Adding a line with sum and deleting previous lines
#Bersive I #Bersive II
levels(idpcamp$LOCATION) <- c(levels(idpcamp$LOCATION), "Bersive I & II")
idpcampbersive <- c("Bersive I & II", sum(idpcamp$Individual[idpcamp$LOCATION=="Bersive I"||
                                                               idpcamp$LOCATION=="Bersive II"]),
                    sum(idpcamp$Planned.capacity..number.of.persons.[idpcamp$LOCATION=="Bersive I"||
                                             idpcamp$LOCATION=="Bersive II"]))

sum(idpcamp$Planned.capacity..number.of.persons.)


#Agricultural Research I #Agricultural Research II
levels(idpcamp$LOCATION) <- c(levels(idpcamp$LOCATION), "Agricultural Research I & II")

#########################

idpcamp.sum <- aggregate(cbind( Planned.capacity..number.of.persons. , Individual) ~ 
                          Governorate+loct,
                        data = idpcamp, FUN = sum, na.rm = TRUE)

idpcamp.sum.ind <- sum(idpcamp$Individual, na.rm = TRUE)
idpcamp.sum.plan <- sum(idpcamp$Planned.capacity..number.of.persons., na.rm = TRUE)
## Get some background map from Google

#############################################################
# 36.852654,42.7677273,10
#36.9009894,42.9764676,10
googleterraindohuk <- get_map(location = c(lon =42.9764676, lat = 36.9009894),
                         color = "color", source = "google",maptype = "terrain", zoom = 10)
googleeterraindohuk <- ggmap(googleterraindohuk)

rm(dohuk)
dohuk <- ggmap(googleterraindohuk)+
  geom_point(aes(size = Planned.capacity..number.of.persons., shape= loct, x = LONGITUDE, y = LATITUDE),data=idpcamp, alpha = 0.9, color="#ccccff",fill="#2a87c8")+  #add proportional symbol at centroids
  scale_shape_manual(values=c(24,25,23),name="Site type") + 
  # scale_fill_manual(values=c("blue","blue", "blue")) + 
  # scale_colour_manual(values=c("white", "white","white"))+   
  scale_size(name="Capacity individual  \n Figures in label represents current population",labels=format_si())+
  ggtitle("IDP Camps in Dahuk")+
  theme(plot.title=element_text(face="bold", size=14),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank(),axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA))

dohuk <- dohuk +
  geom_text(aes(label = LOCATION, x = LONGITUDE, y = LATITUDE, hjust=1, vjust=-1), data=idpcamp, size = 2.1, colour='#2a87c8') #add labels at centroids

dohuk <- dohuk +
  geom_text(aes(label = Individual, x = LONGITUDE, y = LATITUDE, hjust=0, vjust=-1), data=idpcamp, size = 2,face="bold", colour='#00008b') #add labels at centroids


ggsave("plot/mapcamp/map-dohuk.png", dohuk, width=8, height=6,units="in", dpi=300, bg = "transparent")
#############################################################



#############################################################
## 35.4551862,45.3555107,9
googleterrainsuly <- get_map(location = c(lon =45.3555107, lat = 35.4551862),
                              color = "color", source = "google",maptype = "terrain", zoom = 9)
googleeterrainsuly <- ggmap(googleterrainsuly)

rm(suly)
suly <-ggmap(googleterrainsuly)+
  geom_point(aes(size = Planned.capacity..number.of.persons., shape= loct, x = LONGITUDE, y = LATITUDE),data=idpcamp, alpha = 0.9,face="bold", color="#ccccff",fill="#2a87c8")+  #add proportional symbol at centroids
  scale_shape_manual(values=c(24,25,23),name="Site type") +   
  # scale_fill_manual(values=c("blue","blue", "cyan4")) + 
  # scale_colour_manual(values=c("white", "white","white"))+   
  scale_size(name="Capacity individual \n Figures in label represents current population",labels=format_si())+
  ggtitle("IDP Camps in Sulaymaniyah")+
  theme(plot.title=element_text(face="bold", size=14),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank(),axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA))

suly <- suly +
  geom_text(aes(label = LOCATION, x = LONGITUDE, y = LATITUDE, hjust=1, vjust=-1), data=idpcamp, size = 2.1, colour='#2a87c8') #add labels at centroids

suly <- suly +
  geom_text(aes(label = Individual, x = LONGITUDE, y = LATITUDE, hjust=0, vjust=-1), data=idpcamp, size = 2,face="bold", colour='#00008b') #add labels at centroids


ggsave("plot/mapcamp/map-suly.png", suly, width=8, height=6,units="in", dpi=300, bg = "transparent")
#############################################################


#############################################################
# 36.2498443,44.022323,12
googleterrainerbil <- get_map(location = c(lon =44.022323, lat = 36.2498443),
                              color = "color", source = "google",maptype = "terrain", zoom = 12)
googleeterrainerbil <- ggmap(googleterrainerbil)
rm(suly)
erbil <-ggmap(googleterrainerbil)+
  geom_point(aes(size = Planned.capacity..number.of.persons., shape= loct, x = LONGITUDE, y = LATITUDE),data=idpcamp, alpha = 0.9, color="#ccccff",fill="#2a87c8")+  #add proportional symbol at centroids
  scale_shape_manual(values=c(24,25,23),name="Site type") + 
 # scale_fill_manual(values=c("blue","blue", "cyan4")) + 
 # scale_colour_manual(values=c("white", "white","white"))+   
  scale_size(name="Capacity individual \n Figures in label represents current population",labels=format_si())+
  ggtitle("IDP Camps in Erbil")+
  theme(plot.title=element_text(face="bold", size=14),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank(),axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA))

erbil <- erbil +
  geom_text(aes(label = LOCATION, x = LONGITUDE, y = LATITUDE, hjust=1, vjust=-1), data=idpcamp, size = 2.1, colour='#2a87c8') #add labels at centroids

erbil <- erbil +
  geom_text(aes(label = Individual, x = LONGITUDE, y = LATITUDE, hjust=0, vjust=-1), data=idpcamp, size = 2,face="bold", colour='#00008b') #add labels at centroids

ggsave("plot/mapcamp/map-erbil.png", erbil, width=8, height=6,units="in", dpi=300, bg = "transparent")
#############################################################



### get it with toner map




#############################################################
# 36.852654,42.7677273,10
#36.9009894,42.9764676,10
rm(googletonerdohuk)
googletonerdohuk <- get_stamenmap(bbox = c(left = 41.9952, bottom = 36.2974, right = 44.1953, top = 37.4727),
                      zoom = 10, source = "stamen", maptype = "terrain",  crop = TRUE, messaging = FALSE, urlonly = FALSE, color =  "bw")
                             

googleetonerdohuk <- ggmap(googletonerdohuk)
ggsave("plot/mapcamp/googleetonerdohuk.png", googleetonerdohuk, width=8, height=6,units="in", dpi=300, bg = "transparent")
#############################################################

rm(dohuk)
tdohuk <- ggmap(googletonerdohuk)+
  geom_point(aes(x = LONGITUDE, y = LATITUDE),
             data=idpcamp, alpha = 0.9, color="#ccccff",fill="#2a87c8", shape= 24)+  #add proportional symbol at centroids
  #scale_shape_manual(values=c(24,25,23),name="Site type") + 
  # scale_fill_manual(values=c("blue","blue", "blue")) + 
  # scale_colour_manual(values=c("white", "white","white"))+   
  #scale_size(name="",labels=format_si())+
  ggtitle("")+
  theme(plot.title=element_text(face="bold", size=14),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank(),axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA))

tdohuk <- tdohuk +
  geom_text(aes(label = paste(LOCATION,' '), x = LONGITUDE, y = LATITUDE, hjust=0.5, vjust=-0.5), data=idpcamp, size = 1.8, colour='#2a87c8') #add labels at centroids

tdohuk <- tdohuk +
  geom_text(aes(label = paste(Individual,' ind./ '), x = LONGITUDE, y = LATITUDE, hjust=1, vjust=1), data=idpcamp, size = 1.4,face="bold", colour='#2a87c8') #add labels at centroids

tdohuk <- tdohuk +
  geom_text(aes(label = paste(Planned.capacity..number.of.persons.,' cap. '), x = LONGITUDE, y = LATITUDE, hjust=0, vjust=1), data=idpcamp, size = 1.4,face="bold", colour='#2a87c8') #add labels at centroids

ggsave("plot/mapcamp/map-tdohuk.png", tdohuk, width=5, height=3,units="in", dpi=300, bg = "transparent")
#############################################################

#############################################################
# 36.852654,42.7677273,10
#36.9009894,42.9764676,10
rm(googletonerdohukc)
googletonerdohukc <- get_stamenmap(bbox = c(left = 42.6824, bottom = 36.6535, right = 43.2324, top = 36.9477),
                                  zoom = 10, source = "stamen", maptype = "terrain",
                                   crop = TRUE, messaging = FALSE, urlonly = FALSE, 
                                   color =  "color")

googleetonerdohukc <- ggmap(googletonerdohukc)

rm(dohukc)
tdohukc <- ggmap(googletonerdohukc)+
  geom_point(aes(x = LONGITUDE, y = LATITUDE),
             data=idpcamp, alpha = 0.9, color="#ccccff",fill="#2a87c8", shape= 24, size= 1)+  #add proportional symbol at centroids
  #scale_shape_manual(values=c(24,25,23),name="Site type") + 
  # scale_fill_manual(values=c("blue","blue", "blue")) + 
  # scale_colour_manual(values=c("white", "white","white"))+   
  #scale_size(name="",labels=format_si())+
  ggtitle("")+
  theme(plot.title=element_text(face="bold", size=14),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank(),axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA))

tdohukc <- tdohukc +
  geom_text(aes(label = paste(LOCATION,' '), x = LONGITUDE, y = LATITUDE, hjust=0.5, vjust=-0.5), data=idpcamp, size = 1.8, colour='#2a87c8') #add labels at centroids

tdohukc <- tdohukc +
  geom_text(aes(label = paste(Individual,' ind./ '), x = LONGITUDE, y = LATITUDE, hjust=1, vjust=1), data=idpcamp, size = 1.4,face="bold", colour='#2a87c8') #add labels at centroids

tdohukc <- tdohukc +
  geom_text(aes(label = paste(Planned.capacity..number.of.persons.,' cap. '), x = LONGITUDE, y = LATITUDE, hjust=0, vjust=1), data=idpcamp, size = 1.4,face="bold", colour='#2a87c8') #add labels at centroids

ggsave("plot/mapcamp/map-tdohukc.png", tdohukc, width=5, height=3,units="in", dpi=300, bg = "transparent")
#############################################################

#############################################################
# 36.852654,42.7677273,10
#36.9009894,42.9764676,10
rm(googletonerzako)
googletonerzako <- get_stamenmap(bbox = c(left = 42.3379, bottom = 36.9148, right = 42.8879, top = 37.2079),
                                  zoom = 11, source = "stamen", maptype = "terrain",  crop = TRUE, messaging = FALSE, urlonly = FALSE, color =  "color")

googleetonerzako <- ggmap(googletonerzako)

rm(zako)
tzako <- ggmap(googletonerzako)+
  geom_point(aes(x = LONGITUDE, y = LATITUDE),
             data=idpcamp, alpha = 0.9, color="#ccccff",fill="#2a87c8", shape= 24, size= 1)+  #add proportional symbol at centroids
  #scale_shape_manual(values=c(24,25,23),name="Site type") + 
  # scale_fill_manual(values=c("blue","blue", "blue")) + 
  # scale_colour_manual(values=c("white", "white","white"))+   
  #scale_size(name="",labels=format_si())+
  ggtitle("")+
  theme(plot.title=element_text(face="bold", size=14),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank(),axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA))

tzako <- tzako +
  geom_text(aes(label = paste(LOCATION,' '), x = LONGITUDE, y = LATITUDE, hjust=0.5, vjust=-0.5), data=idpcamp, size = 1.8, colour='#2a87c8') #add labels at centroids

tzako <- tzako +
  geom_text(aes(label = paste(Individual,' ind./ '), x = LONGITUDE, y = LATITUDE, hjust=1, vjust=1), data=idpcamp, size = 1.4,face="bold", colour='#2a87c8') #add labels at centroids

tzako <- tzako +
  geom_text(aes(label = paste(Planned.capacity..number.of.persons.,' cap. '), x = LONGITUDE, y = LATITUDE, hjust=0, vjust=1), data=idpcamp, size = 1.4,face="bold", colour='#2a87c8') #add labels at centroids

ggsave("plot/mapcamp/map-tzako.png", tzako, width=5, height=3, units="in", dpi=300, bg = "transparent")
#############################################################




#############################################################
## 35.4551862,45.3555107,9
googletonersuly <- get_stamenmap(bbox = c(left = 44.2667, bottom = 34.8994, right = 46.4667, top = 36.0957),
                                     zoom = 9, source = "stamen", maptype = "terrain",  crop = TRUE, messaging = FALSE, urlonly = FALSE, color =  "color")
googleetonersuly <- ggmap(googletonersuly)

rm(tsuly)
tsuly <-ggmap(googletonersuly)+
  geom_point(aes(x = LONGITUDE, y = LATITUDE),
             data=idpcamp, alpha = 0.9, color="#ccccff",fill="#2a87c8", shape= 24, size= 1)+
  #scale_shape_manual(values=c(24,25,23),name="Site type") +   
  # scale_fill_manual(values=c("blue","blue", "cyan4")) + 
  # scale_colour_manual(values=c("white", "white","white"))+   
  scale_size(name="",labels=format_si())+
  ggtitle("")+
  theme(plot.title=element_text(face="bold", size=14),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank(),axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA))

tsuly <- tsuly +
  geom_text(aes(label = LOCATION, x = LONGITUDE, y = LATITUDE, hjust=0.5, vjust=-0.5), data=idpcamp, size = 1.8, colour='#2a87c8') #add labels at centroids

tsuly <- tsuly +
  geom_text(aes(label = paste(Individual,' ind./ '), x = LONGITUDE, y = LATITUDE, hjust=1, vjust=1), data=idpcamp, size = 1.4,face="bold", colour='#2a87c8') #add labels at centroids

tsuly <- tsuly +
  geom_text(aes(label = paste(Planned.capacity..number.of.persons.,' cap. '), x = LONGITUDE, y = LATITUDE, hjust=0, vjust=1), data=idpcamp, size = 1.4,face="bold", colour='#2a87c8') #add labels at centroids


ggsave("plot/mapcamp/map-tsuly.png", tsuly, width=5, height=3, units="in", dpi=300, bg = "transparent")
#############################################################


#############################################################
# 36.2498443,44.022323,12
googletonererbil <- get_stamenmap(bbox = c(left = 43.8650, bottom = 36.1764, right = 44.1400, top = 36.3245),
                                  zoom = 11, source = "stamen", maptype = "terrain",  crop = TRUE, messaging = FALSE, urlonly = FALSE, color =  "color")
googleetonererbil <- ggmap(googletonererbil)
rm(suly)
terbil <-ggmap(googletonererbil)+
  geom_point(aes(x = LONGITUDE, y = LATITUDE),
             data=idpcamp, alpha = 0.9, color="#ccccff",fill="#2a87c8", shape= 24, size= 1)+
  #scale_shape_manual(values=c(24,25,23),name="Site type") + 
  # scale_fill_manual(values=c("blue","blue", "cyan4")) + 
  # scale_colour_manual(values=c("white", "white","white"))+   
  scale_size(name="Capacity individual \n Figures in label represents current population",labels=format_si())+
  ggtitle("")+
  theme(plot.title=element_text(face="bold", size=14),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank(),axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA))

terbil <- terbil +
  geom_text(aes(label = LOCATION, x = LONGITUDE, y = LATITUDE, hjust=0.5, vjust=-0.5), data=idpcamp, size = 1.6, colour='#2a87c8') #add labels at centroids

terbil <- terbil +
  geom_text(aes(label = paste(Individual,' ind./ '), x = LONGITUDE, y = LATITUDE, hjust=1, vjust=1), data=idpcamp, size = 1.4,face="bold", colour='#2a87c8') #add labels at centroids

terbil <- terbil +
  geom_text(aes(label = paste(Planned.capacity..number.of.persons.,' cap. '), x = LONGITUDE, y = LATITUDE, hjust=0, vjust=1), data=idpcamp, size = 1.4,face="bold", colour='#2a87c8') #add labels at centroids


ggsave("plot/mapcamp/map-terbil.png", terbil, width=5, height=3, units="in", dpi=300, bg = "transparent")
#############################################################












rm(dohuk)
   rm(erbil)
      rm(googleeterraindohuk)
         rm(googleeterrainerbil)
            rm(googleeterrainsuly)
               rm(googleterraindohuk)
                  rm(googleterrainerbil)
rm(googleterrainsuly)
                     rm(i)

