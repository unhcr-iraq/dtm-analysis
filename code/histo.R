
############################### 3 Histogramm of refugee distribution per accomodation type


histo.totalclass <- ggplot(master, aes(x = as.factor(master$class.total))) + geom_histogram(binwidth = 0.5)
ggsave("plot/histo/histogramm-totalclass.png", histo.totalclass, width = 8, height = 6, units = "in", dpi = 300)



rm(master.histodistrib)
master.histodistrib <- melt(master, id = c(11, 7, 8, 43), measure = c(19:31))
master.histodistrib2 <- dcast(master.histodistrib, Governorate + Month.Displacement ~ variable, sum)

# levels(master.histodistrib$variable) str(master.histodistrib) str(master) summary(master.histodistrib) names(master) Interleaved histograms

histo.total <- ggplot(master.histodistrib, aes(x = variable, fill = variable)) + geom_histogram(position = "dodge")
ggsave("plot/histo/histogramm-total.png", histo.total, width = 8, height = 6, units = "in", dpi = 300)

histo.totalfacet <- ggplot(master.histodistrib, aes(x = value)) + geom_histogram(binwidth = 0.5, colour = "black", fill = "white") + facet_wrap(~variable, ncol = 3)
# + geom_vline(data=master.histodistrib, aes(xintercept=value.mean), linetype='dashed', size=1, colour='red')

ggsave("plot/histo/histogramm-totalfacet.png", histo.totalfacet, width = 8, height = 6, units = "in", dpi = 300)

histo.totalf <- ggplot(master, aes(x = as.factor(master$total))) + geom_histogram(binwidth = 0.5)
ggsave("plot/histo/histogramm-totalf.png", histo.totalf, width = 8, height = 6, units = "in", dpi = 300)

histo.Camps <- ggplot(master, aes(x = as.factor(master$IDPs.in.Camps.or.transit.camps))) + geom_histogram(binwidth = 0.5)
ggsave("plot/histo/histogramm-Camps.png", histo.Camps, width = 8, height = 6, units = "in", dpi = 300)

histo.Hotel <- ggplot(master, aes(x = as.factor(master$Rented.Hotel))) + geom_histogram(binwidth = 0.5)
ggsave("plot/histo/histogramm-Hotel.png", histo.Hotel, width = 8, height = 6, units = "in", dpi = 300)

histo.House <- ggplot(master, aes(x = as.factor(master$Rented.House))) + geom_histogram(binwidth = 0.5)
ggsave("plot/histo/histogramm-House.png", histo.House, width = 8, height = 6, units = "in", dpi = 300)

histo.OwnedHouse <- ggplot(master, aes(x = as.factor(master$IDP.Owned.House))) + geom_histogram(binwidth = 0.5)
ggsave("plot/histo/histogramm-OwnedHouse.png", histo.OwnedHouse, width = 8, height = 6, units = "in", dpi = 300)

histo.relative <- ggplot(master, aes(x = as.factor(master$With.Relative))) + geom_histogram(binwidth = 0.5)
ggsave("plot/histo/histogramm-relative.png", histo.relative, width = 8, height = 6, units = "in", dpi = 300)

histo.relative <- ggplot(master, aes(x = as.factor(master$With.Relative))) + geom_histogram(binwidth = 0.5)
ggsave("plot/histo/histogramm-relative.png", histo.relative, width = 8, height = 6, units = "in", dpi = 300)

histo.nonrelative <- ggplot(master, aes(x = as.factor(master$With.HC.non.Relative))) + geom_histogram(binwidth = 0.5)
ggsave("plot/histo/histogramm-nonrelative.png", histo.nonrelative, width = 8, height = 6, units = "in", dpi = 300)

histo.withrelative <- ggplot(master, aes(x = as.factor(master$School.Building))) + geom_histogram(binwidth = 0.5)
ggsave("plot/histo/histogramm-withrelative.png", histo.withrelative, width = 8, height = 6, units = "in", dpi = 300)

histo.shrine <- ggplot(master, aes(x = as.factor(master$Mosques.Holly.Shrines))) + geom_histogram(binwidth = 0.5)
ggsave("plot/histo/histogramm-shrine.png", histo.shrine, width = 8, height = 6, units = "in", dpi = 300)

histo.abbuild <- ggplot(master, aes(x = as.factor(master$Abandoned.public.buildings.under.construction))) + geom_histogram(binwidth = 0.5)
ggsave("plot/histo/histogramm-abbuild.png", histo.abbuild, width = 8, height = 6, units = "in", dpi = 300)

histo.center <- ggplot(master, aes(x = as.factor(master$Collective.centres))) + geom_histogram(binwidth = 0.5)
ggsave("plot/histo/histogramm-center.png", histo.center, width = 8, height = 6, units = "in", dpi = 300)

histo.infsettle <- ggplot(master, aes(x = as.factor(master$Informal.settlements))) + geom_histogram(binwidth = 0.5)
ggsave("plot/histo/histogramm-infsettle.png", histo.infsettle, width = 8, height = 6, units = "in", dpi = 300)

histo.milcamp <- ggplot(master, aes(x = as.factor(master$Military.Camps))) + geom_histogram(binwidth = 0.5)
ggsave("plot/histo/histogramm-milcamp.png", histo.milcamp, width = 8, height = 6, units = "in", dpi = 300)

histo.other <- ggplot(master, aes(x = as.factor(master$Unknown.or.other))) + geom_histogram(binwidth = 0.5)
ggsave("plot/histo/histogramm-other.png", histo.other, width = 8, height = 6, units = "in", dpi = 300)


rm(histo.Camps)
rm(histo.Hotel)
rm(histo.House)
rm(histo.OwnedHouse)
rm(histo.abbuild)
rm(histo.center)
rm(histo.infsettle)
rm(histo.milcamp)
rm(histo.nonrelative)
rm(histo.other)
rm(histo.relative)
rm(histo.shrine)
rm(histo.total)
rm(histo.totalclass)
rm(histo.totalf)
rm(histo.totalfacet)
rm(histo.withrelative)

rm(master.histodistrib)
rm(master.histodistrib2) 
