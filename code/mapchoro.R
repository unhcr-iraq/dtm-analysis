

# Load the shapefiles for admin 1 and admin 2

#irq_adm1 <- readOGR(dsn = "data/shp/irq_admbnda_adm1_ocha_20140717", "irq_admbnda_adm1_ocha_20140717")
irq_adm1 <- readShapePoly('data/shp/irq_admbnda_adm1_ocha_20140717.shp', proj4string=CRS("+proj=longlat"))
#
#plot(irq_adm1)

# Fortify them
irq_adm1@data$id = rownames(irq_adm1@data)

rm(irq_adm1_f)
irq_adm1_f <- fortify(irq_adm1, region="id")
#irq_adm1_f <- merge(irq_adm1_f, irq_adm1@data, by.x="id",by.y="row.names")
irq_adm1_f <-join(irq_adm1_f, irq_adm1@data, by="id")
irq_adm1_f <-join(x=irq_adm1_f, y=govnames, by="A1NameAlt1")

rm(maplevel1)
maplevel1 <-  ggplot(irq_adm1_f, aes(long, lat)) + coord_equal()+
  geom_polygon(data = irq_adm1_f, aes(x = long, y = lat, group = group), alpha = 0.5) +
  geom_text(aes(label = short, x = Longitude_c, y = Latitude_c, group = group)) + #add labels at centroids
  geom_path(data = irq_adm1_f, aes(x = long, y = lat, group = group), color="white")+
  ggtitle("Governorates of Iraq")

## Adding anootation for Governorates short code
maplevel1 <-maplevel1 + annotate("text", x = 39, y = 37.5,  size = 3, label = "Anbar", hjust = 0)
maplevel1 <-maplevel1 + annotate("text", x = 39, y = 37.25, size = 3, label = "Babylon", hjust = 0)
maplevel1 <-maplevel1 + annotate("text", x = 39, y = 37,    size = 3, label = "Baghdad", hjust = 0)
maplevel1 <-maplevel1 + annotate("text", x = 39, y = 36.75, size = 3, label = "Basrah", hjust = 0)
maplevel1 <-maplevel1 + annotate("text", x = 39, y = 36.5,  size = 3, label = "Diyala", hjust = 0)
maplevel1 <-maplevel1 + annotate("text", x = 39, y = 36.25, size = 3, label = "Dahuk", hjust = 0)
maplevel1 <-maplevel1 + annotate("text", x = 39, y = 36,    size = 3, label = "Erbil", hjust = 0)
maplevel1 <-maplevel1 + annotate("text", x = 39, y = 35.75, size = 3, label = "Kerbala", hjust = 0)
maplevel1 <-maplevel1 + annotate("text", x = 39, y = 35.5,  size = 3, label = "Kirkuk", hjust = 0)
maplevel1 <-maplevel1 + annotate("text", x = 39, y = 35.25, size = 3, label = "Missan", hjust = 0)
maplevel1 <-maplevel1 + annotate("text", x = 39, y = 35,    size = 3, label = "Muthanna", hjust = 0)
maplevel1 <-maplevel1 + annotate("text", x = 39, y = 34.75, size = 3, label = "Najaf", hjust = 0)
maplevel1 <-maplevel1 + annotate("text", x = 39, y = 34.5,  size = 3, label = "Ninewa", hjust = 0)
maplevel1 <-maplevel1 + annotate("text", x = 39, y = 34.25, size = 3, label = "Qadissiya", hjust = 0)
maplevel1 <-maplevel1 + annotate("text", x = 39, y = 34,    size = 3, label = "Salah al-Din", hjust = 0)
maplevel1 <-maplevel1 + annotate("text", x = 39, y = 33.75, size = 3, label = "Sulaymaniyah", hjust = 0)
maplevel1 <-maplevel1 + annotate("text", x = 39, y = 33.5,  size = 3, label = "Thi-Qar", hjust = 0)
maplevel1 <-maplevel1 + annotate("text", x = 39, y = 33.25, size = 3, label = "Wassit", hjust = 0)

maplevel1 <-maplevel1 + annotate("text", x = 40.4, y = 37.5,  size = 3, label = "AN", hjust = 0)
maplevel1 <-maplevel1 + annotate("text", x = 40.4, y = 37.25, size = 3, label = "BB", hjust = 0)
maplevel1 <-maplevel1 + annotate("text", x = 40.4, y = 37,    size = 3, label = "BG", hjust = 0)
maplevel1 <-maplevel1 + annotate("text", x = 40.4, y = 36.75, size = 3, label = "BA", hjust = 0)
maplevel1 <-maplevel1 + annotate("text", x = 40.4, y = 36.5,  size = 3, label = "DI", hjust = 0)
maplevel1 <-maplevel1 + annotate("text", x = 40.4, y = 36.25, size = 3, label = "DA", hjust = 0)
maplevel1 <-maplevel1 + annotate("text", x = 40.4, y = 36,    size = 3, label = "AR", hjust = 0)
maplevel1 <-maplevel1 + annotate("text", x = 40.4, y = 35.75, size = 3, label = "KA", hjust = 0)
maplevel1 <-maplevel1 + annotate("text", x = 40.4, y = 35.5,  size = 3, label = "KI", hjust = 0)
maplevel1 <-maplevel1 + annotate("text", x = 40.4, y = 35.25, size = 3, label = "MA", hjust = 0)
maplevel1 <-maplevel1 + annotate("text", x = 40.4, y = 35,    size = 3, label = "MU", hjust = 0)
maplevel1 <-maplevel1 + annotate("text", x = 40.4, y = 34.75, size = 3, label = "NJ", hjust = 0)
maplevel1 <-maplevel1 + annotate("text", x = 40.4, y = 34.5,  size = 3, label = "NI", hjust = 0)
maplevel1 <-maplevel1 + annotate("text", x = 40.4, y = 34.25, size = 3, label = "QA", hjust = 0)
maplevel1 <-maplevel1 + annotate("text", x = 40.4, y = 34,    size = 3, label = "SD", hjust = 0)
maplevel1 <-maplevel1 + annotate("text", x = 40.4, y = 33.75, size = 3, label = "SL", hjust = 0)
maplevel1 <-maplevel1 + annotate("text", x = 40.4, y = 33.5,  size = 3, label = "TQ", hjust = 0)
maplevel1 <-maplevel1 + annotate("text", x = 40.4, y = 33.25, size = 3, label = "WA", hjust = 0)

ggsave("plot/mapchoro/maplevel1.png", maplevel1, width=8, height=6,units="in", dpi=300)

##########################################################
#########################################################

#intervalle <- classIntervals(govnames$idp.pop, n = 5, style = "jenks")
#use the breaks from above to decide the break points
#irq_adm1_f$idp.pop.breaks <- cut(irq_adm1_f$idp.pop, breaks = c(intervalle$brks), dig.lab = 2)
#levels(irq_adm1_f$idp.pop.breaks) <- rev(levels(irq_adm1_f$idp.pop.breaks))

#palette <- brewer.pal(5, "Greys")
#colors <- findColours(intervalle, palette)

rm(maplevel1_idppop)
maplevel1_idppop <-  ggplot(irq_adm1_f, aes(x = long, y = lat, group = group, fill=class.planningcccm)) + 
  coord_equal()+
  geom_polygon(data = irq_adm1_f, aes(fill=class.planningcccm), alpha = 0.8) +
  scale_fill_brewer(palette="Blues", name="") + 
  geom_path(data = irq_adm1_f, aes(x = long, y = lat, group = group), color="#2a87c8", size=0.5)+
  theme_tufte(base_family="Helvetica")+
  theme(plot.title=element_text(face="bold", size=14),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank(),axis.ticks = element_blank(),
        axis.text.y = element_blank(),legend.position = "none")


maplevel1_idppop <- maplevel1_idppop +
  geom_text(aes(label = short, x = Longitude_c, y = Latitude_c, group = group, hjust=0.5, vjust=-0.5), size = 2.5)+ #add labels at centroids
  ggtitle("")      

## Adding anootation for Governorates short code
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 39, y = 37.5,  size = 3.5, label = "Anbar", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 39, y = 37.25, size = 3.5, label = "Babylon", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 39, y = 37,    size = 3.5, label = "Baghdad", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 39, y = 36.75, size = 3.5, label = "Basrah", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 39, y = 36.5,  size = 3.5, label = "Diyala", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 39, y = 36.25, size = 3.5, label = "Dahuk", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 39, y = 36,    size = 3.5, label = "Erbil", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 39, y = 35.75, size = 3.5, label = "Kerbala", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 39, y = 35.5,  size = 3.5, label = "Kirkuk", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 39, y = 35.25, size = 3.5, label = "Missan", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 39, y = 35,    size = 3.5, label = "Muthanna", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 39, y = 34.75, size = 3.5, label = "Najaf", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 39, y = 34.5,  size = 3.5, label = "Ninewa", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 39, y = 34.25, size = 3.5, label = "Qadissiya", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 39, y = 34,    size = 3.5, label = "Salah al-Din", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 39, y = 33.75, size = 3.5, label = "Sulaymaniyah", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 39, y = 33.5,  size = 3.5, label = "Thi-Qar", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 39, y = 33.25, size = 3.5, label = "Wassit", hjust = 0)

maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 40.8, y = 37.5,  size = 3.5, label = "AN", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 40.8, y = 37.25, size = 3.5, label = "BB", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 40.8, y = 37,    size = 3.5, label = "BG", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 40.8, y = 36.75, size = 3.5, label = "BA", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 40.8, y = 36.5,  size = 3.5, label = "DI", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 40.8, y = 36.25, size = 3.5, label = "DA", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 40.8, y = 36,    size = 3.5, label = "AR", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 40.8, y = 35.75, size = 3.5, label = "KA", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 40.8, y = 35.5,  size = 3.5, label = "KI", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 40.8, y = 35.25, size = 3.5, label = "MA", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 40.8, y = 35,    size = 3.5, label = "MU", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 40.8, y = 34.75, size = 3.5, label = "NJ", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 40.8, y = 34.5,  size = 3.5, label = "NI", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 40.8, y = 34.25, size = 3.5, label = "QA", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 40.8, y = 34,    size = 3.5, label = "SD", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 40.8, y = 33.75, size = 3.5, label = "SL", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 40.8, y = 33.5,  size = 3.5, label = "TQ", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 40.8, y = 33.25, size = 3.5, label = "WA", hjust = 0)

ggsave("plot/caseload/mapgov.png", maplevel1_idppop, width=8, height=6,units="in", dpi=300)


###########################################
rm(maplevel1_idppop)
maplevel1_idppop <-  ggplot(irq_adm1_f, aes(x = long, y = lat, group = group, fill=idp.pop.rank)) + 
  #coord_equal()+
  geom_polygon(data = irq_adm1_f, aes(fill=idp.pop.rank), alpha = 0.8) +
  scale_fill_brewer(palette="PuRd", name="Ratio- Class jenk") + 
  geom_path(data = irq_adm1_f, aes(x = long, y = lat, group = group), color="white")+
  theme_bw() ## Boundaries


maplevel1_idppop <- maplevel1_idppop +
  geom_text(aes(label = short, x = Longitude_c, y = Latitude_c, group = group, hjust=0.5, vjust=-0.5), size = 2.5)+ #add labels at centroids
  ggtitle("Ratio IDPs / Population (estimated pop.)")      

## Adding anootation for Governorates short code
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 39, y = 37.5,  size = 3, label = "Anbar", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 39, y = 37.25, size = 3, label = "Babylon", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 39, y = 37,    size = 3, label = "Baghdad", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 39, y = 36.75, size = 3, label = "Basrah", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 39, y = 36.5,  size = 3, label = "Diyala", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 39, y = 36.25, size = 3, label = "Dahuk", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 39, y = 36,    size = 3, label = "Erbil", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 39, y = 35.75, size = 3, label = "Kerbala", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 39, y = 35.5,  size = 3, label = "Kirkuk", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 39, y = 35.25, size = 3, label = "Missan", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 39, y = 35,    size = 3, label = "Muthanna", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 39, y = 34.75, size = 3, label = "Najaf", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 39, y = 34.5,  size = 3, label = "Ninewa", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 39, y = 34.25, size = 3, label = "Qadissiya", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 39, y = 34,    size = 3, label = "Salah al-Din", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 39, y = 33.75, size = 3, label = "Sulaymaniyah", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 39, y = 33.5,  size = 3, label = "Thi-Qar", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 39, y = 33.25, size = 3, label = "Wassit", hjust = 0)

maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 40.4, y = 37.5,  size = 3, label = "AN", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 40.4, y = 37.25, size = 3, label = "BB", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 40.4, y = 37,    size = 3, label = "BG", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 40.4, y = 36.75, size = 3, label = "BA", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 40.4, y = 36.5,  size = 3, label = "DI", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 40.4, y = 36.25, size = 3, label = "DA", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 40.4, y = 36,    size = 3, label = "AR", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 40.4, y = 35.75, size = 3, label = "KA", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 40.4, y = 35.5,  size = 3, label = "KI", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 40.4, y = 35.25, size = 3, label = "MA", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 40.4, y = 35,    size = 3, label = "MU", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 40.4, y = 34.75, size = 3, label = "NJ", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 40.4, y = 34.5,  size = 3, label = "NI", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 40.4, y = 34.25, size = 3, label = "QA", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 40.4, y = 34,    size = 3, label = "SD", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 40.4, y = 33.75, size = 3, label = "SL", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 40.4, y = 33.5,  size = 3, label = "TQ", hjust = 0)
maplevel1_idppop <-maplevel1_idppop + annotate("text", x = 40.4, y = 33.25, size = 3, label = "WA", hjust = 0)


ggsave("plot/mapchoro/maplevel1_idppop.png", maplevel1_idppop, width=8, height=6,units="in", dpi=300)
######################################################################

#### Population school rank
rm(maplevel1_classschool)
maplevel1_classschool <-  ggplot(irq_adm1_f, aes(x = long, y = lat, group = group, fill=class.school)) + 
  #coord_equal()+
  geom_polygon(data = irq_adm1_f, aes(fill=class.school), alpha = 0.8) +
  scale_fill_brewer(palette="PuRd", name="Rank- Class jenk") + 
  geom_path(data = irq_adm1_f, aes(x = long, y = lat, group = group), color="white")+
  theme_bw() ## Boundaries

maplevel1_classschool <- maplevel1_classschool +
  geom_text(aes(label = short, x = Longitude_c, y = Latitude_c, group = group, hjust=0.5, vjust=-0.5), size = 2.5)+ #add labels at centroids
  ggtitle("Rank IDPs in School")      

## Adding anootation for Governorates short code
maplevel1_classschool <-maplevel1_classschool + annotate("text", x = 39, y = 37.5,  size = 3, label = "Anbar", hjust = 0)
maplevel1_classschool <-maplevel1_classschool + annotate("text", x = 39, y = 37.25, size = 3, label = "Babylon", hjust = 0)
maplevel1_classschool <-maplevel1_classschool + annotate("text", x = 39, y = 37,    size = 3, label = "Baghdad", hjust = 0)
maplevel1_classschool <-maplevel1_classschool + annotate("text", x = 39, y = 36.75, size = 3, label = "Basrah", hjust = 0)
maplevel1_classschool <-maplevel1_classschool + annotate("text", x = 39, y = 36.5,  size = 3, label = "Diyala", hjust = 0)
maplevel1_classschool <-maplevel1_classschool + annotate("text", x = 39, y = 36.25, size = 3, label = "Dahuk", hjust = 0)
maplevel1_classschool <-maplevel1_classschool + annotate("text", x = 39, y = 36,    size = 3, label = "Erbil", hjust = 0)
maplevel1_classschool <-maplevel1_classschool + annotate("text", x = 39, y = 35.75, size = 3, label = "Kerbala", hjust = 0)
maplevel1_classschool <-maplevel1_classschool + annotate("text", x = 39, y = 35.5,  size = 3, label = "Kirkuk", hjust = 0)
maplevel1_classschool <-maplevel1_classschool + annotate("text", x = 39, y = 35.25, size = 3, label = "Missan", hjust = 0)
maplevel1_classschool <-maplevel1_classschool + annotate("text", x = 39, y = 35,    size = 3, label = "Muthanna", hjust = 0)
maplevel1_classschool <-maplevel1_classschool + annotate("text", x = 39, y = 34.75, size = 3, label = "Najaf", hjust = 0)
maplevel1_classschool <-maplevel1_classschool + annotate("text", x = 39, y = 34.5,  size = 3, label = "Ninewa", hjust = 0)
maplevel1_classschool <-maplevel1_classschool + annotate("text", x = 39, y = 34.25, size = 3, label = "Qadissiya", hjust = 0)
maplevel1_classschool <-maplevel1_classschool + annotate("text", x = 39, y = 34,    size = 3, label = "Salah al-Din", hjust = 0)
maplevel1_classschool <-maplevel1_classschool + annotate("text", x = 39, y = 33.75, size = 3, label = "Sulaymaniyah", hjust = 0)
maplevel1_classschool <-maplevel1_classschool + annotate("text", x = 39, y = 33.5,  size = 3, label = "Thi-Qar", hjust = 0)
maplevel1_classschool <-maplevel1_classschool + annotate("text", x = 39, y = 33.25, size = 3, label = "Wassit", hjust = 0)

maplevel1_classschool <-maplevel1_classschool + annotate("text", x = 40.4, y = 37.5,  size = 3, label = "AN", hjust = 0)
maplevel1_classschool <-maplevel1_classschool + annotate("text", x = 40.4, y = 37.25, size = 3, label = "BB", hjust = 0)
maplevel1_classschool <-maplevel1_classschool + annotate("text", x = 40.4, y = 37,    size = 3, label = "BG", hjust = 0)
maplevel1_classschool <-maplevel1_classschool + annotate("text", x = 40.4, y = 36.75, size = 3, label = "BA", hjust = 0)
maplevel1_classschool <-maplevel1_classschool + annotate("text", x = 40.4, y = 36.5,  size = 3, label = "DI", hjust = 0)
maplevel1_classschool <-maplevel1_classschool + annotate("text", x = 40.4, y = 36.25, size = 3, label = "DA", hjust = 0)
maplevel1_classschool <-maplevel1_classschool + annotate("text", x = 40.4, y = 36,    size = 3, label = "AR", hjust = 0)
maplevel1_classschool <-maplevel1_classschool + annotate("text", x = 40.4, y = 35.75, size = 3, label = "KA", hjust = 0)
maplevel1_classschool <-maplevel1_classschool + annotate("text", x = 40.4, y = 35.5,  size = 3, label = "KI", hjust = 0)
maplevel1_classschool <-maplevel1_classschool + annotate("text", x = 40.4, y = 35.25, size = 3, label = "MA", hjust = 0)
maplevel1_classschool <-maplevel1_classschool + annotate("text", x = 40.4, y = 35,    size = 3, label = "MU", hjust = 0)
maplevel1_classschool <-maplevel1_classschool + annotate("text", x = 40.4, y = 34.75, size = 3, label = "NJ", hjust = 0)
maplevel1_classschool <-maplevel1_classschool + annotate("text", x = 40.4, y = 34.5,  size = 3, label = "NI", hjust = 0)
maplevel1_classschool <-maplevel1_classschool + annotate("text", x = 40.4, y = 34.25, size = 3, label = "QA", hjust = 0)
maplevel1_classschool <-maplevel1_classschool + annotate("text", x = 40.4, y = 34,    size = 3, label = "SD", hjust = 0)
maplevel1_classschool <-maplevel1_classschool + annotate("text", x = 40.4, y = 33.75, size = 3, label = "SL", hjust = 0)
maplevel1_classschool <-maplevel1_classschool + annotate("text", x = 40.4, y = 33.5,  size = 3, label = "TQ", hjust = 0)
maplevel1_classschool <-maplevel1_classschool + annotate("text", x = 40.4, y = 33.25, size = 3, label = "WA", hjust = 0)

ggsave("plot/mapchoro/maplevel1_classschool.png", maplevel1_classschool, width=8, height=6,units="in", dpi=300)
######################################################################

####
rm(maplevel1_commprivaterank)
maplevel1_commprivaterank <-  ggplot(irq_adm1_f, aes(x = long, y = lat, group = group, fill=comm.private.rank)) + 
  #coord_equal()+
  geom_polygon(data = irq_adm1_f, aes(fill=comm.private.rank), alpha = 0.8) +
  scale_fill_brewer(palette="PuRd", name="Rank - Class jenk") + 
  geom_path(data = irq_adm1_f, aes(x = long, y = lat, group = group), color="white")+
  theme_bw() ## Boundaries

maplevel1_commprivaterank <- maplevel1_commprivaterank +
  geom_text(aes(label = short, x = Longitude_c, y = Latitude_c, group = group, hjust=0.5, vjust=-0.5), size = 2.5)+ #add labels at centroids
  ggtitle("Ratio Communal Setting  / Private Setting")      

## Adding anootation for Governorates short code
maplevel1_commprivaterank <-maplevel1_commprivaterank + annotate("text", x = 39, y = 37.5,  size = 3, label = "Anbar", hjust = 0)
maplevel1_commprivaterank <-maplevel1_commprivaterank + annotate("text", x = 39, y = 37.25, size = 3, label = "Babylon", hjust = 0)
maplevel1_commprivaterank <-maplevel1_commprivaterank + annotate("text", x = 39, y = 37,    size = 3, label = "Baghdad", hjust = 0)
maplevel1_commprivaterank <-maplevel1_commprivaterank + annotate("text", x = 39, y = 36.75, size = 3, label = "Basrah", hjust = 0)
maplevel1_commprivaterank <-maplevel1_commprivaterank + annotate("text", x = 39, y = 36.5,  size = 3, label = "Diyala", hjust = 0)
maplevel1_commprivaterank <-maplevel1_commprivaterank + annotate("text", x = 39, y = 36.25, size = 3, label = "Dahuk", hjust = 0)
maplevel1_commprivaterank <-maplevel1_commprivaterank + annotate("text", x = 39, y = 36,    size = 3, label = "Erbil", hjust = 0)
maplevel1_commprivaterank <-maplevel1_commprivaterank + annotate("text", x = 39, y = 35.75, size = 3, label = "Kerbala", hjust = 0)
maplevel1_commprivaterank <-maplevel1_commprivaterank + annotate("text", x = 39, y = 35.5,  size = 3, label = "Kirkuk", hjust = 0)
maplevel1_commprivaterank <-maplevel1_commprivaterank + annotate("text", x = 39, y = 35.25, size = 3, label = "Missan", hjust = 0)
maplevel1_commprivaterank <-maplevel1_commprivaterank + annotate("text", x = 39, y = 35,    size = 3, label = "Muthanna", hjust = 0)
maplevel1_commprivaterank <-maplevel1_commprivaterank + annotate("text", x = 39, y = 34.75, size = 3, label = "Najaf", hjust = 0)
maplevel1_commprivaterank <-maplevel1_commprivaterank + annotate("text", x = 39, y = 34.5,  size = 3, label = "Ninewa", hjust = 0)
maplevel1_commprivaterank <-maplevel1_commprivaterank + annotate("text", x = 39, y = 34.25, size = 3, label = "Qadissiya", hjust = 0)
maplevel1_commprivaterank <-maplevel1_commprivaterank + annotate("text", x = 39, y = 34,    size = 3, label = "Salah al-Din", hjust = 0)
maplevel1_commprivaterank <-maplevel1_commprivaterank + annotate("text", x = 39, y = 33.75, size = 3, label = "Sulaymaniyah", hjust = 0)
maplevel1_commprivaterank <-maplevel1_commprivaterank + annotate("text", x = 39, y = 33.5,  size = 3, label = "Thi-Qar", hjust = 0)
maplevel1_commprivaterank <-maplevel1_commprivaterank + annotate("text", x = 39, y = 33.25, size = 3, label = "Wassit", hjust = 0)

maplevel1_commprivaterank <-maplevel1_commprivaterank + annotate("text", x = 40.4, y = 37.5,  size = 3, label = "AN", hjust = 0)
maplevel1_commprivaterank <-maplevel1_commprivaterank + annotate("text", x = 40.4, y = 37.25, size = 3, label = "BB", hjust = 0)
maplevel1_commprivaterank <-maplevel1_commprivaterank + annotate("text", x = 40.4, y = 37,    size = 3, label = "BG", hjust = 0)
maplevel1_commprivaterank <-maplevel1_commprivaterank + annotate("text", x = 40.4, y = 36.75, size = 3, label = "BA", hjust = 0)
maplevel1_commprivaterank <-maplevel1_commprivaterank + annotate("text", x = 40.4, y = 36.5,  size = 3, label = "DI", hjust = 0)
maplevel1_commprivaterank <-maplevel1_commprivaterank + annotate("text", x = 40.4, y = 36.25, size = 3, label = "DA", hjust = 0)
maplevel1_commprivaterank <-maplevel1_commprivaterank + annotate("text", x = 40.4, y = 36,    size = 3, label = "AR", hjust = 0)
maplevel1_commprivaterank <-maplevel1_commprivaterank + annotate("text", x = 40.4, y = 35.75, size = 3, label = "KA", hjust = 0)
maplevel1_commprivaterank <-maplevel1_commprivaterank + annotate("text", x = 40.4, y = 35.5,  size = 3, label = "KI", hjust = 0)
maplevel1_commprivaterank <-maplevel1_commprivaterank + annotate("text", x = 40.4, y = 35.25, size = 3, label = "MA", hjust = 0)
maplevel1_commprivaterank <-maplevel1_commprivaterank + annotate("text", x = 40.4, y = 35,    size = 3, label = "MU", hjust = 0)
maplevel1_commprivaterank <-maplevel1_commprivaterank + annotate("text", x = 40.4, y = 34.75, size = 3, label = "NJ", hjust = 0)
maplevel1_commprivaterank <-maplevel1_commprivaterank + annotate("text", x = 40.4, y = 34.5,  size = 3, label = "NI", hjust = 0)
maplevel1_commprivaterank <-maplevel1_commprivaterank + annotate("text", x = 40.4, y = 34.25, size = 3, label = "QA", hjust = 0)
maplevel1_commprivaterank <-maplevel1_commprivaterank + annotate("text", x = 40.4, y = 34,    size = 3, label = "SD", hjust = 0)
maplevel1_commprivaterank <-maplevel1_commprivaterank + annotate("text", x = 40.4, y = 33.75, size = 3, label = "SL", hjust = 0)
maplevel1_commprivaterank <-maplevel1_commprivaterank + annotate("text", x = 40.4, y = 33.5,  size = 3, label = "TQ", hjust = 0)
maplevel1_commprivaterank <-maplevel1_commprivaterank + annotate("text", x = 40.4, y = 33.25, size = 3, label = "WA", hjust = 0)

ggsave("plot/mapchoro/maplevel1_commprivaterank.png", maplevel1_commprivaterank, width=8, height=6,units="in", dpi=300)
######################################################################

####
rm(maplevel1_commsettingrank)
maplevel1_commsettingrank<-  ggplot(irq_adm1_f, aes(x = long, y = lat, group = group, fill=class.communalsetting)) + 
  #coord_equal()+
  geom_polygon(data = irq_adm1_f, aes(fill=class.communalsetting), alpha = 0.8) +
  scale_fill_brewer(palette="PuRd", name="Rank- Class jenk") + 
  geom_path(data = irq_adm1_f, aes(x = long, y = lat, group = group), color="white")+
  theme_bw() ## Boundaries

maplevel1_commsettingrank<- maplevel1_commsettingrank+
  geom_text(aes(label = short, x = Longitude_c, y = Latitude_c, group = group, hjust=0.5, vjust=-0.5), size = 2.5)+ #add labels at centroids
  ggtitle("Rank IDP Ind. in Communal Setting")      

## Adding anootation for Governorates short code
maplevel1_commsettingrank<-maplevel1_commsettingrank+ annotate("text", x = 39, y = 37.5,  size = 3, label = "Anbar", hjust = 0)
maplevel1_commsettingrank<-maplevel1_commsettingrank+ annotate("text", x = 39, y = 37.25, size = 3, label = "Babylon", hjust = 0)
maplevel1_commsettingrank<-maplevel1_commsettingrank+ annotate("text", x = 39, y = 37,    size = 3, label = "Baghdad", hjust = 0)
maplevel1_commsettingrank<-maplevel1_commsettingrank+ annotate("text", x = 39, y = 36.75, size = 3, label = "Basrah", hjust = 0)
maplevel1_commsettingrank<-maplevel1_commsettingrank+ annotate("text", x = 39, y = 36.5,  size = 3, label = "Diyala", hjust = 0)
maplevel1_commsettingrank<-maplevel1_commsettingrank+ annotate("text", x = 39, y = 36.25, size = 3, label = "Dahuk", hjust = 0)
maplevel1_commsettingrank<-maplevel1_commsettingrank+ annotate("text", x = 39, y = 36,    size = 3, label = "Erbil", hjust = 0)
maplevel1_commsettingrank<-maplevel1_commsettingrank+ annotate("text", x = 39, y = 35.75, size = 3, label = "Kerbala", hjust = 0)
maplevel1_commsettingrank<-maplevel1_commsettingrank+ annotate("text", x = 39, y = 35.5,  size = 3, label = "Kirkuk", hjust = 0)
maplevel1_commsettingrank<-maplevel1_commsettingrank+ annotate("text", x = 39, y = 35.25, size = 3, label = "Missan", hjust = 0)
maplevel1_commsettingrank<-maplevel1_commsettingrank+ annotate("text", x = 39, y = 35,    size = 3, label = "Muthanna", hjust = 0)
maplevel1_commsettingrank<-maplevel1_commsettingrank+ annotate("text", x = 39, y = 34.75, size = 3, label = "Najaf", hjust = 0)
maplevel1_commsettingrank<-maplevel1_commsettingrank+ annotate("text", x = 39, y = 34.5,  size = 3, label = "Ninewa", hjust = 0)
maplevel1_commsettingrank<-maplevel1_commsettingrank+ annotate("text", x = 39, y = 34.25, size = 3, label = "Qadissiya", hjust = 0)
maplevel1_commsettingrank<-maplevel1_commsettingrank+ annotate("text", x = 39, y = 34,    size = 3, label = "Salah al-Din", hjust = 0)
maplevel1_commsettingrank<-maplevel1_commsettingrank+ annotate("text", x = 39, y = 33.75, size = 3, label = "Sulaymaniyah", hjust = 0)
maplevel1_commsettingrank<-maplevel1_commsettingrank+ annotate("text", x = 39, y = 33.5,  size = 3, label = "Thi-Qar", hjust = 0)
maplevel1_commsettingrank<-maplevel1_commsettingrank+ annotate("text", x = 39, y = 33.25, size = 3, label = "Wassit", hjust = 0)

maplevel1_commsettingrank<-maplevel1_commsettingrank+ annotate("text", x = 40.4, y = 37.5,  size = 3, label = "AN", hjust = 0)
maplevel1_commsettingrank<-maplevel1_commsettingrank+ annotate("text", x = 40.4, y = 37.25, size = 3, label = "BB", hjust = 0)
maplevel1_commsettingrank<-maplevel1_commsettingrank+ annotate("text", x = 40.4, y = 37,    size = 3, label = "BG", hjust = 0)
maplevel1_commsettingrank<-maplevel1_commsettingrank+ annotate("text", x = 40.4, y = 36.75, size = 3, label = "BA", hjust = 0)
maplevel1_commsettingrank<-maplevel1_commsettingrank+ annotate("text", x = 40.4, y = 36.5,  size = 3, label = "DI", hjust = 0)
maplevel1_commsettingrank<-maplevel1_commsettingrank+ annotate("text", x = 40.4, y = 36.25, size = 3, label = "DA", hjust = 0)
maplevel1_commsettingrank<-maplevel1_commsettingrank+ annotate("text", x = 40.4, y = 36,    size = 3, label = "AR", hjust = 0)
maplevel1_commsettingrank<-maplevel1_commsettingrank+ annotate("text", x = 40.4, y = 35.75, size = 3, label = "KA", hjust = 0)
maplevel1_commsettingrank<-maplevel1_commsettingrank+ annotate("text", x = 40.4, y = 35.5,  size = 3, label = "KI", hjust = 0)
maplevel1_commsettingrank<-maplevel1_commsettingrank+ annotate("text", x = 40.4, y = 35.25, size = 3, label = "MA", hjust = 0)
maplevel1_commsettingrank<-maplevel1_commsettingrank+ annotate("text", x = 40.4, y = 35,    size = 3, label = "MU", hjust = 0)
maplevel1_commsettingrank<-maplevel1_commsettingrank+ annotate("text", x = 40.4, y = 34.75, size = 3, label = "NJ", hjust = 0)
maplevel1_commsettingrank<-maplevel1_commsettingrank+ annotate("text", x = 40.4, y = 34.5,  size = 3, label = "NI", hjust = 0)
maplevel1_commsettingrank<-maplevel1_commsettingrank+ annotate("text", x = 40.4, y = 34.25, size = 3, label = "QA", hjust = 0)
maplevel1_commsettingrank<-maplevel1_commsettingrank+ annotate("text", x = 40.4, y = 34,    size = 3, label = "SD", hjust = 0)
maplevel1_commsettingrank<-maplevel1_commsettingrank+ annotate("text", x = 40.4, y = 33.75, size = 3, label = "SL", hjust = 0)
maplevel1_commsettingrank<-maplevel1_commsettingrank+ annotate("text", x = 40.4, y = 33.5,  size = 3, label = "TQ", hjust = 0)
maplevel1_commsettingrank<-maplevel1_commsettingrank+ annotate("text", x = 40.4, y = 33.25, size = 3, label = "WA", hjust = 0)

ggsave("plot/mapchoro/maplevel1_commsettingrank.png", maplevel1_commsettingrank, width=8, height=6,units="in", dpi=300)
######################################################################

####
rm(maplevel1_totalrank)
maplevel1_totalrank<-  ggplot(irq_adm1_f, aes(x = long, y = lat, group = group, fill=total.rank)) + 
  #coord_equal()+
  geom_polygon(data = irq_adm1_f, aes(fill=total.rank), alpha = 0.8) +
  scale_fill_brewer(palette="PuRd", name="Rank- Class jenk ") + 
  geom_path(data = irq_adm1_f, aes(x = long, y = lat, group = group), color="white")+
  theme_bw() ## Boundaries

maplevel1_totalrank<- maplevel1_totalrank+
  geom_text(aes(label = short, x = Longitude_c, y = Latitude_c, group = group, hjust=0.5, vjust=-0.5), size = 2.5)+ #add labels at centroids
  ggtitle("Rank Total Pop")      

## Adding anootation for Governorates short code
maplevel1_totalrank<-maplevel1_totalrank+ annotate("text", x = 39, y = 37.5,  size = 3, label = "Anbar", hjust = 0)
maplevel1_totalrank<-maplevel1_totalrank+ annotate("text", x = 39, y = 37.25, size = 3, label = "Babylon", hjust = 0)
maplevel1_totalrank<-maplevel1_totalrank+ annotate("text", x = 39, y = 37,    size = 3, label = "Baghdad", hjust = 0)
maplevel1_totalrank<-maplevel1_totalrank+ annotate("text", x = 39, y = 36.75, size = 3, label = "Basrah", hjust = 0)
maplevel1_totalrank<-maplevel1_totalrank+ annotate("text", x = 39, y = 36.5,  size = 3, label = "Diyala", hjust = 0)
maplevel1_totalrank<-maplevel1_totalrank+ annotate("text", x = 39, y = 36.25, size = 3, label = "Dahuk", hjust = 0)
maplevel1_totalrank<-maplevel1_totalrank+ annotate("text", x = 39, y = 36,    size = 3, label = "Erbil", hjust = 0)
maplevel1_totalrank<-maplevel1_totalrank+ annotate("text", x = 39, y = 35.75, size = 3, label = "Kerbala", hjust = 0)
maplevel1_totalrank<-maplevel1_totalrank+ annotate("text", x = 39, y = 35.5,  size = 3, label = "Kirkuk", hjust = 0)
maplevel1_totalrank<-maplevel1_totalrank+ annotate("text", x = 39, y = 35.25, size = 3, label = "Missan", hjust = 0)
maplevel1_totalrank<-maplevel1_totalrank+ annotate("text", x = 39, y = 35,    size = 3, label = "Muthanna", hjust = 0)
maplevel1_totalrank<-maplevel1_totalrank+ annotate("text", x = 39, y = 34.75, size = 3, label = "Najaf", hjust = 0)
maplevel1_totalrank<-maplevel1_totalrank+ annotate("text", x = 39, y = 34.5,  size = 3, label = "Ninewa", hjust = 0)
maplevel1_totalrank<-maplevel1_totalrank+ annotate("text", x = 39, y = 34.25, size = 3, label = "Qadissiya", hjust = 0)
maplevel1_totalrank<-maplevel1_totalrank+ annotate("text", x = 39, y = 34,    size = 3, label = "Salah al-Din", hjust = 0)
maplevel1_totalrank<-maplevel1_totalrank+ annotate("text", x = 39, y = 33.75, size = 3, label = "Sulaymaniyah", hjust = 0)
maplevel1_totalrank<-maplevel1_totalrank+ annotate("text", x = 39, y = 33.5,  size = 3, label = "Thi-Qar", hjust = 0)
maplevel1_totalrank<-maplevel1_totalrank+ annotate("text", x = 39, y = 33.25, size = 3, label = "Wassit", hjust = 0)

maplevel1_totalrank<-maplevel1_totalrank+ annotate("text", x = 40.4, y = 37.5,  size = 3, label = "AN", hjust = 0)
maplevel1_totalrank<-maplevel1_totalrank+ annotate("text", x = 40.4, y = 37.25, size = 3, label = "BB", hjust = 0)
maplevel1_totalrank<-maplevel1_totalrank+ annotate("text", x = 40.4, y = 37,    size = 3, label = "BG", hjust = 0)
maplevel1_totalrank<-maplevel1_totalrank+ annotate("text", x = 40.4, y = 36.75, size = 3, label = "BA", hjust = 0)
maplevel1_totalrank<-maplevel1_totalrank+ annotate("text", x = 40.4, y = 36.5,  size = 3, label = "DI", hjust = 0)
maplevel1_totalrank<-maplevel1_totalrank+ annotate("text", x = 40.4, y = 36.25, size = 3, label = "DA", hjust = 0)
maplevel1_totalrank<-maplevel1_totalrank+ annotate("text", x = 40.4, y = 36,    size = 3, label = "AR", hjust = 0)
maplevel1_totalrank<-maplevel1_totalrank+ annotate("text", x = 40.4, y = 35.75, size = 3, label = "KA", hjust = 0)
maplevel1_totalrank<-maplevel1_totalrank+ annotate("text", x = 40.4, y = 35.5,  size = 3, label = "KI", hjust = 0)
maplevel1_totalrank<-maplevel1_totalrank+ annotate("text", x = 40.4, y = 35.25, size = 3, label = "MA", hjust = 0)
maplevel1_totalrank<-maplevel1_totalrank+ annotate("text", x = 40.4, y = 35,    size = 3, label = "MU", hjust = 0)
maplevel1_totalrank<-maplevel1_totalrank+ annotate("text", x = 40.4, y = 34.75, size = 3, label = "NJ", hjust = 0)
maplevel1_totalrank<-maplevel1_totalrank+ annotate("text", x = 40.4, y = 34.5,  size = 3, label = "NI", hjust = 0)
maplevel1_totalrank<-maplevel1_totalrank+ annotate("text", x = 40.4, y = 34.25, size = 3, label = "QA", hjust = 0)
maplevel1_totalrank<-maplevel1_totalrank+ annotate("text", x = 40.4, y = 34,    size = 3, label = "SD", hjust = 0)
maplevel1_totalrank<-maplevel1_totalrank+ annotate("text", x = 40.4, y = 33.75, size = 3, label = "SL", hjust = 0)
maplevel1_totalrank<-maplevel1_totalrank+ annotate("text", x = 40.4, y = 33.5,  size = 3, label = "TQ", hjust = 0)
maplevel1_totalrank<-maplevel1_totalrank+ annotate("text", x = 40.4, y = 33.25, size = 3, label = "WA", hjust = 0)

ggsave("plot/mapchoro/maplevel1_totalrank.png", maplevel1_totalrank, width=8, height=6,units="in", dpi=300)
######################################################################

####
rm(maplevel1_composite)
maplevel1_composite<-  ggplot(irq_adm1_f, aes(x = long, y = lat, group = group, fill=class.composite)) + 
  #coord_equal()+
  geom_polygon(data = irq_adm1_f, aes(fill=class.composite), alpha = 0.8) +
  scale_fill_brewer(palette="PuRd", name="Rank- Class jenk") + 
  geom_path(data = irq_adm1_f, aes(x = long, y = lat, group = group), color="white")+
  theme_bw() ## Boundaries

maplevel1_composite<- maplevel1_composite+
  geom_text(aes(label = short, x = Longitude_c, y = Latitude_c, group = group, hjust=0.5, vjust=-0.5), size = 2.5)+ #add labels at centroids
  ggtitle("Rank Composite")      

## Adding anootation for Governorates short code
maplevel1_composite<-maplevel1_composite+ annotate("text", x = 39, y = 37.5,  size = 3, label = "Anbar", hjust = 0)
maplevel1_composite<-maplevel1_composite+ annotate("text", x = 39, y = 37.25, size = 3, label = "Babylon", hjust = 0)
maplevel1_composite<-maplevel1_composite+ annotate("text", x = 39, y = 37,    size = 3, label = "Baghdad", hjust = 0)
maplevel1_composite<-maplevel1_composite+ annotate("text", x = 39, y = 36.75, size = 3, label = "Basrah", hjust = 0)
maplevel1_composite<-maplevel1_composite+ annotate("text", x = 39, y = 36.5,  size = 3, label = "Diyala", hjust = 0)
maplevel1_composite<-maplevel1_composite+ annotate("text", x = 39, y = 36.25, size = 3, label = "Dahuk", hjust = 0)
maplevel1_composite<-maplevel1_composite+ annotate("text", x = 39, y = 36,    size = 3, label = "Erbil", hjust = 0)
maplevel1_composite<-maplevel1_composite+ annotate("text", x = 39, y = 35.75, size = 3, label = "Kerbala", hjust = 0)
maplevel1_composite<-maplevel1_composite+ annotate("text", x = 39, y = 35.5,  size = 3, label = "Kirkuk", hjust = 0)
maplevel1_composite<-maplevel1_composite+ annotate("text", x = 39, y = 35.25, size = 3, label = "Missan", hjust = 0)
maplevel1_composite<-maplevel1_composite+ annotate("text", x = 39, y = 35,    size = 3, label = "Muthanna", hjust = 0)
maplevel1_composite<-maplevel1_composite+ annotate("text", x = 39, y = 34.75, size = 3, label = "Najaf", hjust = 0)
maplevel1_composite<-maplevel1_composite+ annotate("text", x = 39, y = 34.5,  size = 3, label = "Ninewa", hjust = 0)
maplevel1_composite<-maplevel1_composite+ annotate("text", x = 39, y = 34.25, size = 3, label = "Qadissiya", hjust = 0)
maplevel1_composite<-maplevel1_composite+ annotate("text", x = 39, y = 34,    size = 3, label = "Salah al-Din", hjust = 0)
maplevel1_composite<-maplevel1_composite+ annotate("text", x = 39, y = 33.75, size = 3, label = "Sulaymaniyah", hjust = 0)
maplevel1_composite<-maplevel1_composite+ annotate("text", x = 39, y = 33.5,  size = 3, label = "Thi-Qar", hjust = 0)
maplevel1_composite<-maplevel1_composite+ annotate("text", x = 39, y = 33.25, size = 3, label = "Wassit", hjust = 0)

maplevel1_composite<-maplevel1_composite+ annotate("text", x = 40.4, y = 37.5,  size = 3, label = "AN", hjust = 0)
maplevel1_composite<-maplevel1_composite+ annotate("text", x = 40.4, y = 37.25, size = 3, label = "BB", hjust = 0)
maplevel1_composite<-maplevel1_composite+ annotate("text", x = 40.4, y = 37,    size = 3, label = "BG", hjust = 0)
maplevel1_composite<-maplevel1_composite+ annotate("text", x = 40.4, y = 36.75, size = 3, label = "BA", hjust = 0)
maplevel1_composite<-maplevel1_composite+ annotate("text", x = 40.4, y = 36.5,  size = 3, label = "DI", hjust = 0)
maplevel1_composite<-maplevel1_composite+ annotate("text", x = 40.4, y = 36.25, size = 3, label = "DA", hjust = 0)
maplevel1_composite<-maplevel1_composite+ annotate("text", x = 40.4, y = 36,    size = 3, label = "AR", hjust = 0)
maplevel1_composite<-maplevel1_composite+ annotate("text", x = 40.4, y = 35.75, size = 3, label = "KA", hjust = 0)
maplevel1_composite<-maplevel1_composite+ annotate("text", x = 40.4, y = 35.5,  size = 3, label = "KI", hjust = 0)
maplevel1_composite<-maplevel1_composite+ annotate("text", x = 40.4, y = 35.25, size = 3, label = "MA", hjust = 0)
maplevel1_composite<-maplevel1_composite+ annotate("text", x = 40.4, y = 35,    size = 3, label = "MU", hjust = 0)
maplevel1_composite<-maplevel1_composite+ annotate("text", x = 40.4, y = 34.75, size = 3, label = "NJ", hjust = 0)
maplevel1_composite<-maplevel1_composite+ annotate("text", x = 40.4, y = 34.5,  size = 3, label = "NI", hjust = 0)
maplevel1_composite<-maplevel1_composite+ annotate("text", x = 40.4, y = 34.25, size = 3, label = "QA", hjust = 0)
maplevel1_composite<-maplevel1_composite+ annotate("text", x = 40.4, y = 34,    size = 3, label = "SD", hjust = 0)
maplevel1_composite<-maplevel1_composite+ annotate("text", x = 40.4, y = 33.75, size = 3, label = "SL", hjust = 0)
maplevel1_composite<-maplevel1_composite+ annotate("text", x = 40.4, y = 33.5,  size = 3, label = "TQ", hjust = 0)
maplevel1_composite<-maplevel1_composite+ annotate("text", x = 40.4, y = 33.25, size = 3, label = "WA", hjust = 0)

ggsave("plot/mapchoro/maplevel1_composite.png", maplevel1_composite, width=8, height=6,units="in", dpi=300)
######################################################################



intervalleref <- classIntervals(govnames$idp.ref, n = 5, style = "jenks")
#use the breaks from above to decide the break points
irq_adm1_f$idp.ref.breaks <- cut(irq_adm1_f$idp.ref, breaks = c(intervalleref$brks), dig.lab = 2)
levels(irq_adm1_f$idp.ref.breaks) <- rev(levels(irq_adm1_f$idp.ref.breaks))


rm(maplevel1_idpref)
maplevel1_idpref <-  ggplot(irq_adm1_f, aes(x = long, y = lat, group = group, fill=idp.ref.breaks)) + 
  #coord_equal()+
  geom_polygon(data = irq_adm1_f, aes(fill=idp.ref.breaks), alpha = 0.5) +
  scale_fill_brewer(palette="Greys", name="Ratio % IDP/Refugees") + 
  geom_path(data = irq_adm1_f, aes(x = long, y = lat, group = group), color="white")+
  theme_bw() ## Boundaries

maplevel1_idpref <- maplevel1_idpref +
  geom_point(aes(size = total, x = Longitude_c, y = Latitude_c, group = group), alpha = 0.3, color="coral1")+  #add proportional symbol at centroids
  scale_size(name="Total IDP Ind.",labels=format_si())

maplevel1_idpref <- maplevel1_idpref +
  geom_text(aes(label = short, x = Longitude_c, y = Latitude_c, group = group, hjust=0.5, vjust=-0.5), size = 2.5)+ #add labels at centroids
  ggtitle("Ratio IDPs / Refugees")      

ggsave("plot/mapchoro/maplevel1_idpref.png", maplevel1_idpref, width=8, height=6,units="in", dpi=300)





#########################################

irq_adm2 <- readShapePoly('~/unhcr_r_project/displacement/shp/irq_admbnda_adm2_ocha_20140717.shp', proj4string=CRS("+proj=longlat"))
irq_adm2@data$id = rownames(irq_adm2@data)
irq_adm2_f <- fortify(irq_adm2, region="id")
irq_adm2_f <-join(irq_adm2_f, irq_adm2@data, by="id")


#########################################
rm(colors)
rm(intervalle)
rm(irq_adm1)
rm(irq_adm2)
rm(irq_adm1_f)
rm(irq_adm2_f)


rm(maplevel1)
rm(maplevel1_idppop)
rm(maplevel1_idpref)
rm(intervalleref)
rm(maplevel1_classschool)
rm(maplevel1_totalrank)
rm(maplevel1_commprivaterank)
rm(maplevel1_commsettingrank)
rm(palette)
