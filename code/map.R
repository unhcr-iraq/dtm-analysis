###Maps!!


################# Prepare map background ########
# Iraq bbox           38.782   28.833   49.433   37.358

#get_stamenmap(bbox = c(left = -95.80204, bottom = 29.38048, right = -94.92313, top = 30.14344),
#              zoom = 10,
#              maptype = c("terrain", "watercolor", "toner"),
#              crop = TRUE, messaging = FALSE, urlonly = FALSE,
#              filename = "ggmapTemp", color = c("color", "bw"))

#iraqstamentoner <-get_stamenmap(bbox = c(left = 37.782, bottom = 27.833, right = 50.433, top = 38.358),
#                                zoom = 6,
#                                maptype = "toner",
#                                crop = TRUE, messaging = FALSE, urlonly = FALSE,
#                                filename = "out/map/iraqstamentoner.png", color =  "bw")
#toner <- ggmap(iraqstamentoner)
#ggsave("plot/map/map-toner.png", toner, width=8, height=6,units="in", dpi=300)



#osm <- get_openstreetmap(bbox = c(left = 37.782, bottom = 27.833, right = 50.433, top = 38.358),
#                         scale = OSM_scale_lookup(6))
#osmm <- ggmap(osm)
#ggsave("plot/map/map-osm.png", osmm, width=8, height=6,units="in", dpi=300)

googleterrain <- get_map(location = c(lon =44.538574, lat = 32.750323),
                         color = "color", source = "google",maptype = "terrain", zoom = 6)
googleeterrain <- ggmap(googleterrain)
#ggsave("plot/map/map-googleterrain.png", googleeterrain, width=8, height=6,units="in", dpi=300)


#overlay<-stat_density2d(aes(Longitude,Latitude,fill=..level..),alpha=..level..,data=master,geom="polygon")+scale_alpha(guide="none")
#
#overlay<-stat_density2d(aes(Longitude,Latitude,fill=master$total),alpha=master$total,data=master,geom="polygon")+scale_alpha(guide="none")

overlay<-ggplot(master,aes(x=Longitude,y=Latitude))+
  stat_density2d(aes(fill=..level..), geom="polygon") +
  scale_fill_gradient(low="blue", high="green")+
  scale_alpha(guide="none")
ggsave("plot/map/map-overlay.png", overlay, width=8, height=6,units="in", dpi=300)

#rm(overlay)
#overlay<- stat_density2d(aes(x=Longitude,y=Latitude,fill=..level..),data=master, geom="polygon") +
#  scale_fill_gradient(low="blue", high="green")+
#  scale_alpha(guide="none")

#overlay<- stat_density2d(aes(x=master$Longitude,y=master$Latitude,fill=..level..), geom="polygon") +
#  scale_fill_gradient(low="blue", high="green")+
#  scale_alpha(guide="none")

#density <-ggmap(googleterrain)+
  #overlay+
#  geom_point(aes(Longitude,Latitude,color=master$total),size=2,data=master, alpha = 4/10) + 
#  scale_colour_gradient(expression(total),low="black",high="red") +
#  ggtitle("IDPs in Iraq")+theme(plot.title=element_text(size=20))

#rm(density2)
#density2 <-ggmap(googleterrain)
#density2 <-density2 +overlay
#ggsave("plot/map/map-density2.png", density2, width=8, height=6,units="in", dpi=300)

rm(density)
density <-ggmap(googleterrain)+
          #overlay+
          geom_point(aes(size = total, x = Longitude, y = Latitude),data=master, alpha = 0.5, color="coral1")+  #add proportional symbol at centroids
          
          #geom_point(aes(Longitude,Latitude,color=master$total),size=2,data=master, alpha = 4/10) + 
          #scale_colour_gradient(expression(total),low="black",high="red") +
          scale_size(name="Total IDP Ind.",labels=format_si())+
          ggtitle("IDPs in Iraq")+theme(plot.title=element_text(size=20))
ggsave("plot/map/map-density.png", density, width=8, height=6,units="in", dpi=300)
#############################################################
#############################################################


master.Openair <- subset(master, Open.air>1) 
rm(densityOpenair)
densityOpenair <-ggmap(googleterrain)+
  geom_point(aes(size = Open.air, x = Longitude, y = Latitude),data=master.Openair, color="coral1")+  #add proportional symbol at centroids
  scale_size(name="Total IDP Ind.",labels=format_si())+
  ggtitle("IDPs hosted in Open Air")+theme(plot.title=element_text(size=20))
ggsave("plot/map/map-densityOpenair.png", densityOpenair, width=8, height=6,units="in", dpi=300)
rm(master.Openair)
rm(densityOpenair)


master.school <- subset(master, Squatted.schools>1) 
rm(densityschool)
densityschool <-ggmap(googleterrain)+
  geom_point(aes(size = Squatted.schools, x = Longitude, y = Latitude),data=master.school,  color="coral1")+  #add proportional symbol at centroids
  scale_size(name="Total IDP Ind.",labels=format_si())+
  ggtitle("IDPs hosted in schools in Iraq")+theme(plot.title=element_text(size=20))
ggsave("plot/map/map-densityschool.png", densityschool, width=8, height=6,units="in", dpi=300)
rm(master.school)
rm(densityschool)

master.Improvised <- subset(master, Improvised.site>1) 
rm(densityImprovised)
densityImprovised <-ggmap(googleterrain)+
  geom_point(aes(size = Improvised.site, x = Longitude, y = Latitude),data=master.Improvised, color="coral1")+  #add proportional symbol at centroids
  scale_size(name="Total IDP Ind.",labels=format_si())+
  ggtitle("IDPs hosted in Improvised Sites")+theme(plot.title=element_text(size=20))
ggsave("plot/map/map-densityImprovised.png", densityImprovised, width=8, height=6,units="in", dpi=300)
rm(master.Improvised)
rm(densityImprovised)

master.Organised <- subset(master, Organised.site>1) 
rm(densityOrganised)
densityOrganised <-ggmap(googleterrain)+
  geom_point(aes(size = Organised.site, x = Longitude, y = Latitude),data=master.Organised, color="coral1")+  #add proportional symbol at centroids
  scale_size(name="Total IDP Ind.",labels=format_si())+
  ggtitle("IDPs hosted in Organised Sites")+theme(plot.title=element_text(size=20))
ggsave("plot/map/map-densityOrganised.png", densityOrganised, width=8, height=6,units="in", dpi=300)
rm(master.Organised)
rm(densityOrganised)

master.Rented <- subset(master, Rented.Accomodation>1) 
rm(densityRented)
densityRented <-ggmap(googleterrain)+
  geom_point(aes(size = Rented.Accomodation, x = Longitude, y = Latitude),data=master.Rented, color="coral1")+  #add proportional symbol at centroids
  scale_size(name="Total IDP Ind.",labels=format_si())+
  ggtitle("IDPs hosted in Rented Accomodation")+theme(plot.title=element_text(size=20))
ggsave("plot/map/map-densityRented.png", densityRented, width=8, height=6,units="in", dpi=300)
rm(master.Rented)
rm(densityRented)

master.Hosted <- subset(master, Hosted.or.Owned.Accomodation>1) 
rm(densityHosted)
densityHosted <-ggmap(googleterrain)+
  geom_point(aes(size = Hosted.or.Owned.Accomodation, x = Longitude, y = Latitude),data=master.Hosted, color="coral1")+  #add proportional symbol at centroids
  scale_size(name="Total IDP Ind.",labels=format_si())+
  ggtitle("IDPs hosted with relatives or non-relatives")+theme(plot.title=element_text(size=20))
ggsave("plot/map/map-densityHosted.png", densityHosted, width=8, height=6,units="in", dpi=300)
rm(master.Hosted)
rm(densityHosted)

#############################################################
#############################################################

rm(densityerbil)
densityerbil <-ggmap(googleterrain)+
  #overlay+
  geom_point(aes(size = total, x = Longitude, y = Latitude),data=master, color="orangered3")+
  ggtitle("IDPs in Erbil")+theme(plot.title=element_text(size=20)) +
  scale_size(name="Total IDP Ind.",labels=format_si())+
  ylim(35.5,36.75)+xlim(43,45)+
  facet_wrap(~Month.Displacement)

ggsave("plot/map/map-densityerbil.png", densityerbil, width=8, height=6,units="in", dpi=300)

#raster1 <- ggmap(googleterrain) + geom_point(aes(x = Longitude, y = Latitude), data = master,alpha = 4/10, colour = 'red', size = 2)
# Save this!
#ggsave("plot/map/map-raster1.png", raster1, width=8, height=6,units="in", dpi=300)

#raster <- ggmap(iraqstamentoner) + geom_point(aes(x = Longitude, y = Latitude), data = master,alpha = 4/10, colour = 'red', size = 2)
# Save this!
#ggsave("plot/map/map-raster.png", raster, width=8, height=6,units="in", dpi=300)

################################################################################
## Remove all points with wrong coordinates
mastermap <-    master[master$Longitude > 1, ]

# Note that for maps, we need to remove point with wrong coordinates!
rm(mapcommunal)

mapcommunal <- googleeterrain
mapcommunal <- mapcommunal +
  #stat_summary(fun.y = mean)+
  geom_point(aes(size = communal.setting, x = Longitude, y = Latitude),data=master, color="orangered3")+
  #geom_point(aes(mastermap$Longitude,  mastermap$Latitude, size = mastermap$communal.setting), alpha = 4/10) +
  #coord_equal() +
  #scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
  labs(x = "Longitude", y = "Latitude", fill = "% Communal settings/ Total") +
  ggtitle("IDP's hosted in Communal settings")+
  theme_bw()

#mapcommunal
# Save this!
ggsave("plot/map/map-pc-communal-setting.png", mapcommunal, width=8, height=6,units="in", dpi=300)
#########################################################################
#########################################################################
rm(mastermapcamp)
mastermapcamp <- mastermap[master$IDPs.in.Camps.or.transit.camps > 1, ]
rm(mapcamp)
mapcamp <- googleeterrain
mapcamp <- mapcamp + 
  geom_point(aes(size = IDPs.in.Camps.or.transit.camps, x = Longitude, y = Latitude),data=mastermapcamp, color="orangered3")+
  labs(x = "Longitude", y = "Latitude") +
  ggtitle("IDPs hosted in Camp")+
  theme_bw()
ggsave("plot/map/map-camp-setting.png", mapcamp, width=8, height=6,units="in", dpi=300)
#########################################################################
#########################################################################

rm(mapcommunalmonthorigin)
mapcommunalmonthorigin <-googleeterrain
mapcommunalmonthorigin <- mapcommunalmonthorigin +
  geom_point(aes(x=mastermap$Longitude,  y=mastermap$Latitude, size=mastermap$communal.setting), alpha = 0.5, color="coral1") +
  #facet_grid( mastermap$Origin.Governorate ~ mastermap$Month.Displacement) +
  labs(x = "Longitude", y = "Latitude", fill = "Total IDPs in Communal settings") +
  ggtitle("IDPs hosted in Camp settings")+
  #coord_equal() +
  theme_bw()
  #scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +

# Save this!
ggsave("plot/map/map-communal-month-origin.png", mapcommunalmonthorigin, width=8, height=6,units="in", dpi=300)
#########################################################################
#########################################################################


rm(mapmonthorigin)

mapmonthorigin <- googleeterrain

mapmonthorigin <- mapmonthorigin + geom_point(aes(x=Longitude,  y=Latitude, size=total), data=master, color="coral1")+
  #facet_grid(Origin.Governorate ~ Month.Displacement)+
  #facet_grid(mastermap$Origin.Governorate ~ mastermap$Month.Displacement)+
  labs(x = "Longitude", y = "Latitude")+
  ggtitle("# IDP - trend by month and governorate of Origin")+
  theme_bw()
ggsave("plot/map/map-month-origin.png", mapmonthorigin, width=10, height=9,units="in", dpi=300)
#########################################################################
#########################################################################

# Let's try hexabin to view the data!

rm(maptcommunalhex)
maptcommunalhex <- ggplot(master[master$Longitude > 1, ], aes(Longitude,  Latitude, z = communal.setting)) +
  stat_summary_hex()+
  #coord_equal() +
  theme_bw()+ scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
  labs(x = "Longitude", y = "Latitude", fill = "# hosted in Communal settings") +
  ggtitle("# IDP hosted in Communal settings")

print(maptcommunalhex)
# Save this!
ggsave("plot/map/map-total-communal-hex.png", maptcommunalhex, width=8, height=6,units="in", dpi=300)
#########################################################################
#########################################################################



rm(mapcommunalhex)
mapcommunalhex <- googleeterrain
mapcommunalhex <- mapcommunalhex+
                     stat_summary_hex(aes(mastermap$Longitude,  mastermap$Latitude, z = mastermap$pc.communal ), alpha = 7/10)+
                     #coord_equal() +
                     theme_bw() + 
                     #scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
                     labs(x = "Longitude", y = "Latitude", fill = "% Communal settings/ Total") +
                     ggtitle("% IDP hosted in Communal settings")

# Save this!
ggsave("plot/map/map-pc-communal-hex.png", mapcommunalhex, width=8, height=6,units="in", dpi=300)
#########################################################################
#########################################################################
                                      
rm(mapcommunalhexorigin)
mapcommunalhexorigin <-  ggplot(master[master$Longitude > 1, ], aes(Longitude,  Latitude, z = pc.communal)) +
                                        stat_summary_hex()+
                                        #facet_grid(Origin.Governorate ~ .) +
                                        facet_wrap( ~ Origin.Governorate, ncol=3)+
                                        coord_equal() +
                                        theme_bw()+ scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
                                        labs(x = "Longitude", y = "Latitude", fill = "% Communal settings/ Total") +
                                        ggtitle("% IDP hosted in Communal settings per Origin")
                                      
mapcommunalhexorigin
# Save this!
ggsave("plot/map/map-pc-communal-hex-origin.png", mapcommunalhexorigin, width=8, height=6,units="in", dpi=300)
#########################################################################
#########################################################################
                                      
rm(maptcamphex)
maptcamphex <-  googleeterrain
maptcamphex <- maptcamphex +
                stat_summary_hex(aes(mastermap$Longitude,  mastermap$Latitude, z = mastermap$IDPs.in.Camps.or.transit.camps), alpha = 7/10)+
                #coord_equal() +
                theme_bw()+ scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
                labs(x = "Longitude", y = "Latitude", fill = "# IDP hosted in Camp settings") +
                ggtitle("# IDP hosted in Camp settings")
                                      
maptcamphex
# Save this!
ggsave("plot/map/map-total-camp-hex.png", maptcamphex, width=8, height=6,units="in", dpi=300)
#########################################################################
#########################################################################
                                      
                                      
rm(mapcamphex)
mapcamphex <-  googleeterrain
mapcamphex <- mapcamphex +
              stat_summary_hex(aes(mastermap$Longitude,  mastermap$Latitude, z = mastermap$pc.camp), alpha = 7/10)+
              #coord_equal() +
              theme_bw()+
              scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
              labs(x = "Longitude", y = "Latitude", fill = "% Communal settings/ Total") +
              ggtitle("% IDP hosted in Camp settings")
                                      
mapcamphex
# Save this!
ggsave("plot/map/map-pc-camp-hex.png", mapcamphex, width=8, height=6,units="in", dpi=300)
#########################################################################
#########################################################################
                                      
rm(mapcommunalhexmonth)
mapcommunalhexmonth <- googleeterrain
mapcommunalhexmonth <- mapcommunalhexmonth +
                       stat_summary_hex(aes(mastermap$Longitude,  mastermap$Latitude, z = mastermap$pc.communal), alpha = 7/10)+
                       facet_wrap(  ~ Month.Displacement, ncol=3)+
                       #coord_equal() +
                       theme_bw() +
                       scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
                       labs(x = "Longitude", y = "Latitude", fill = "% Communal settings/ Total") +
                       ggtitle("% IDP hosted in communal settings per month")
                                      
#mapcommunalhexmonth
# Save this!
#ggsave("plot/map/map-pc-communal-hex-month.png", mapcommunalhexmonth, width=8, height=6,units="in", dpi=300)
#########################################################################
#########################################################################
                                      
rm(mapcommunalhexmonthorigin)
mapcommunalhexmonthorigin <- ggplot(master[master$Longitude > 1, ], aes(Longitude,  Latitude, z = pc.communal)) +
                             stat_summary_hex()+
                             facet_grid(Origin.Governorate ~ Month.Displacement)+
                             #coord_equal() +
                             theme_bw() + 
                             scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
                             labs(x = "Longitude", y = "Latitude", fill = "% Communal settings/ Total") +
                             ggtitle("% IDP hosted in Camp settings")
                                      
mapcommunalhexmonthorigin
# Save this!
ggsave("plot/map/map-communal-hex-month-origin.png", mapcommunalhexmonthorigin, width=8, height=6,units="in", dpi=300)
#########################################################################
#########################################################################
                                      
rm(mappriorityfood)
mappriorityfood  <-googleeterrain
mappriorityfood <- mappriorityfood +
                   stat_summary_hex(aes(mastermap$Longitude,  mastermap$Latitude, z = mastermap$priority.Food), alpha = 7/10)+
                   #coord_equal() +
                   theme_bw() + 
                   scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
                   labs(x = "Longitude", y = "Latitude", fill = "# IDP ") +
                   ggtitle("# IDP with Food Priority")
                                      
mappriorityfood
# Save this!
ggsave("plot/map/map-priority-food.png", mappriorityfood, width=8, height=6,units="in", dpi=300)
#########################################################################
#########################################################################
                                      
rm(mappriorityshelter)
mappriorityshelter <- googleeterrain
mappriorityshelter <- mappriorityshelter +
                       stat_summary_hex(aes(mastermap$Longitude,  mastermap$Latitude, z = mastermap$priority.Shelter), alpha = 7/10)+
                       #coord_equal() +
                       theme_bw() + 
                       scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
                       labs(x = "Longitude", y = "Latitude", fill = "# IDP") +
                       ggtitle("# IDP with Shelter Priority")
                                      
mappriorityshelter
 # Save this!
ggsave("plot/map/map-priority-shelter.png", mappriorityshelter, width=8, height=6,units="in", dpi=300)
#########################################################################
#########################################################################
                                      
rm(mapprioritynfi)
mapprioritynfi <-googleeterrain
mapprioritynfi <- mapprioritynfi +
                    stat_summary_hex(aes(mastermap$Longitude,  mastermap$Latitude, z = mastermap$priority.NFI), alpha = 7/10)+
                    #coord_equal() +
                    theme_bw() + 
                    scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
                    labs(x = "Longitude", y = "Latitude", fill = "# IDP") +
                    ggtitle("# IDP with NFI Priority")
                                      
mapprioritynfi
# Save this!
ggsave("plot/map/map-priority-nfi.png", mapprioritynfi, width=8, height=6,units="in", dpi=300)
#########################################################################
#########################################################################
                                      
rm(mapprioritywater)
mapprioritywater <- googleeterrain
mapprioritywater <- mapprioritywater +
                     stat_summary_hex(aes(mastermap$Longitude,  mastermap$Latitude, z = mastermap$priority.Water), alpha = 7/10)+
                     #coord_equal() +
                     theme_bw() + 
                     scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
                     labs(x = "Longitude", y = "Latitude", fill = "# IDP") +
                     ggtitle("# IDP with Water Priority")
                                      
mapprioritywater
# Save this!
ggsave("plot/map/map-priority-water.png", mapprioritywater, width=8, height=6,units="in", dpi=300)
#########################################################################
#########################################################################
                                      
rm(mapprioritycash)
mapprioritycash  <- googleeterrain
mapprioritycash  <- mapprioritycash+
                     stat_summary_hex(aes(mastermap$Longitude,  mastermap$Latitude, z = mastermap$priority.Cash ), alpha = 7/10)+
                     #coord_equal() +
                     theme_bw() + 
                     scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
                     labs(x = "Longitude", y = "Latitude", fill = "# IDP") +
                     ggtitle("# IDP with Cash Priority")
                                      
 mapprioritycash
# Save this!
 ggsave("plot/map/map-priority-cash.png", mapprioritycash, width=8, height=6,units="in", dpi=300)
#########################################################################
#########################################################################
                                      
rm(mappriorityhealth)
mappriorityhealth  <-googleeterrain
mappriorityhealth  <-  mappriorityhealth  +
                       stat_summary_hex( aes(mastermap$Longitude,  mastermap$Latitude, z = mastermap$priority.Health ), alpha = 7/10)+
                       #coord_equal() +
                       theme_bw() + 
                       scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
                       labs(x = "Longitude", y = "Latitude", fill = "# IDP") +
                       ggtitle("# IDP with Health Priority")
                                      
mappriorityhealth
# Save this!
ggsave("plot/map/map-priority-health.png", mappriorityhealth, width=8, height=6,units="in", dpi=300)
#########################################################################
#########################################################################
                                      
rm(mapprioritychild)
mapprioritychild  <-googleeterrain
mapprioritychild <- mapprioritychild +
                      stat_summary_hex(aes(mastermap$Longitude,  mastermap$Latitude, z = mastermap$priority.Child ), alpha = 7/10)+
                      #coord_equal() +
                      theme_bw() +  
                      scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
                      labs(x = "Longitude", y = "Latitude", fill = "# IDP") +
                      ggtitle("# IDP with Child Priority")
                                      
 ## Alternative  withtout background
                                      
 #mapprioritychild <- ggplot(master[master$Longitude > 1, ], aes(Longitude,  Latitude, z = priority.Child )) +
 #  stat_summary_hex()+
 #  coord_equal() +
 #  theme_bw()+ scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
 #  labs(x = "Longitude", y = "Latitude", fill = "# IDP") +
 #  ggtitle("# IDP with Child Priority")
                                      
                                      
# Save this!
ggsave("plot/map/map-priority-child.png", mapprioritychild, width=8, height=6,units="in", dpi=300)
#########################################################################
#########################################################################


rm(densityerbil) 

rm(density)
rm(googleeterrain)           
rm(googleterrain)
rm(mapcamp)
rm(mapcamphex)                
rm(mapcommunal)
rm(mapcommunalhex)
rm(mapcommunalhexmonth)
rm(mapcommunalhexmonthorigin)
rm(mapcommunalhexorigin)
rm(mapcommunalmonthorigin)
rm(maplevel1)
rm(maplevel1_idppop)
rm(maplevel1_idpref)
rm(mapmonthorigin)
rm(mapprioritycash)
rm(mapprioritychild)
rm(mappriorityfood)
rm(mappriorityhealth)
rm(mapprioritynfi)
rm(mappriorityshelter)
rm(mapprioritywater)
rm(maptcamphex)
rm(maptcommunalhex)

