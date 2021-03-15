
# Plot Locations -----------------------------------------------------
library(sp); library(rgdal); library(plyr); library(dplyr)
# eglin_plots <- rbind(
#       readOGR("/Volumes/GARMIN/Garmin/GPX/Waypoints_19-AUG-17.gpx",
#               layer = "waypoints"),
#       readOGR("/Volumes/GARMIN/Garmin/GPX/Waypoints_18-AUG-17.gpx",
#               layer = "waypoints"),
#       readOGR("/Volumes/GARMIN/Garmin/GPX/Waypoints_17-AUG-17.gpx",
#               layer = "waypoints"),
#       readOGR("/Volumes/GARMIN/Garmin/GPX/Waypoints_16-AUG-17.gpx",
#               layer = "waypoints"))
# tyndall_plots <- rbind(
#       readOGR("/Volumes/GARMIN/Garmin/GPX/Waypoints_18-JUL-17.gpx",
#               layer = "waypoints"),
#       readOGR("/Volumes/GARMIN/Garmin/GPX/Waypoints_20-JUL-17.gpx",
#               layer = "waypoints"))
#
# avonpark_plots1 <- readOGR("/Volumes/GARMIN/Garmin/GPX/Waypoints_27-JUN-17.gpx",
#                            layer = "waypoints")
# avonpark_plots2 <- readOGR("/Volumes/GARMIN/Garmin/GPX/Waypoints_28-JUN-17.gpx",
#               layer = "waypoints")
# avonpark_plots3 <- readOGR("/Volumes/GARMIN/Garmin/GPX/Waypoints_29-JUN-17.gpx",
#               layer = "waypoints")

# blandingF1 <- readOGR("/Volumes/GARMIN/Garmin/GPX/Waypoints_21-JUN-17.gpx",
#         layer = "waypoints")
# blandingE1H1 <- readOGR("/Volumes/GARMIN/Garmin/GPX/Waypoints_14-JUN-17.gpx",
#         layer = "waypoints")

# list.files("/Volumes/GARMIN/Garmin/GPX", full.names = T)

flist <- list.files("/Volumes/GARMIN/Garmin/GPX", pattern = "-18", full.names = T)

# flist <- strsplit(flist, ".gpx")
waypt <- 1
for(f in flist){
      i = readOGR(dsn = paste(f), layer = "waypoints")
      i@data = i@data[,1:5]
      assign(paste("waypts",waypt,sep = ""), i)
      waypt = waypt+1
}
waypt
rm(i)
rm(f)
rm(flist)
rm(waypt)


blanding_k1 <- readOGR("/Volumes/GARMIN/Garmin/GPX/BlandingK1.gpx", layer = "waypoints")
blanding_k1@data <- blanding_k1@data[,1:5]
blanding_m1 <- readOGR("/Volumes/GARMIN/Garmin/GPX/BlandingM1.gpx", layer = "waypoints")
blanding_m1@data <- blanding_m1@data[,1:5]

jacks_j1 <- readOGR("/Volumes/GARMIN/Garmin/GPX/JacksonJ1.gpx", layer = "waypoints")
jacks_j1@data <- jacks_j1@data[,1:5]
jacks_k1 <- readOGR("/Volumes/GARMIN/Garmin/GPX/JacksonK1.gpx", layer = "waypoints")
jacks_k1@data <- jacks_k1@data[,1:5]
jacks_l1 <- readOGR("/Volumes/GARMIN/Garmin/GPX/JacksonL1.gpx", layer = "waypoints")
jacks_l1@data <- jacks_l1@data[,1:5]
jacks_m1 <- readOGR("/Volumes/GARMIN/Garmin/GPX/JacksonM1.gpx", layer = "waypoints")
jacks_m1@data <- jacks_m1@data[,1:5]
shelby_c1 <- readOGR("/Volumes/GARMIN/Garmin/GPX/ShelbyC1.gpx", layer = "waypoints")
shelby_c1@data <- shelby_c1@data[,1:5]
shelby_c1@data$name <- "ShelbyC1"



new_waypts <- rbind(waypts1,waypts2,waypts3,waypts4,waypts5,waypts6,waypts7,waypts8,waypts9,waypts10,waypts11,waypts12,waypts13,waypts14,waypts15,waypts16,waypts17,waypts18,waypts19,waypts20,waypts21,waypts22,waypts23, waypts24, waypts25, waypts26, waypts27, blanding_k1,blanding_m1, jacks_j1,jacks_k1,jacks_l1,jacks_m1,shelby_c1)

# waypts <-
# readOGR("/Volumes/GARMIN/Garmin/GPX/Waypoints_24-MAY-18.gpx", layer = "waypoints")


all_plots <- readOGR("data/plot-locations.shp")
summary(all_plots)
summary(new_waypts)
plots_2017 <- readOGR("data/plots-2018.shp")
summary(plots_2017)
# all_plots@coords
## Add elevation coordinates to match dimensions of 'all_plots' coords slot
# waypts@coords <- cbind(waypts@coords, waypts@data$ele)

names(plots_2017)
names(new_waypts)
new_waypts@data <- select(new_waypts@data, time, name, ele) %>%
      rename(elevGPS = ele) %>%
      mutate(descriptio = "Semipermanent plot", fire_year = "", imcy_inv = "")

new_waypts@data$name <- as.character(new_waypts@data$name)
new_waypts@data <- mutate(new_waypts@data,
                          instal = tolower(substr(new_waypts@data$name, start = 1, stop = nchar(new_waypts@data$name)-2)),
                          instal = stringr::str_trim(instal,side = "both"))

# waypts <- waypts[-7,]
# new_waypts@data <- droplevels(
      # select(new_waypts@data, time, name, descriptio:blockDesc, elevGPS, fire_year))

update_plots <- rbind(plots_2017,new_waypts)
duplicated(update_plots$name)
sort(as.character(update_plots$name))

# all_plots <- rbind(all_plots, new_waypts)
# spRbind(all_plots, waypts)
# duplicated(all_plots$name)
# all_plots <- remove.duplicates(all_plots)

# summary(all_plots)



# flame_height_locations <- all_plots[all_plots$descriptio!="Semipermanent plot",]
# plot_locations <- all_plots[all_plots$descriptio=="Semipermanent plot",]

# writeOGR(all_plots, "data", "all-locations",
#          "ESRI Shapefile", overwrite_layer = T)

update_plots$lon <- as.numeric(update_plots@coords[,1])
update_plots$lat <- as.numeric(update_plots@coords[,2])
update_plots@data <- select(update_plots@data, name, instal, lon, lat)

writeOGR(update_plots, "data", "plot-locations",
         "ESRI Shapefile", overwrite_layer = T)

library(ggmap)
bmap <- ggmap(get_googlemap(center = c(-84, 31), zoom = 6))
plots_df <- update_plots@data
# update_plots@bbox
bmap + geom_point(aes(x = lon, y = lat), data = plots_df)
ggmap(get_googlemap(center = c(-88.9, 30.955), zoom = 13)) +
      geom_point(aes(x = lon, y = lat), data = plots_df)
