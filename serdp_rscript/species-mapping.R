# Mapping Invasive Species

library(rgdal); library(maptools); library(sf); library(dplyr)


# Exploring new ‘sf’ package ----------------------------------------------

# system.time(microstegium <- readOGR("data/Microstegium", "points"))
system.time(microstegium <- st_read("data/Microstegium/points.shp"))# this is fast!
class(microstegium)
attr(microstegium, "sf_column")
print(microstegium[9:15], n=3)
methods(class = "sf")# you can do a lot of basic manipulations
# st_geometry(microstegium)# retrieve geometry list-column
# st_bbox(microstegium)# get the bounding box coordinates
# st_drivers()# get list of drivers available; all can read

## Example wrting a simple features object
## These two commands are equivalent
# st_write(microstegium, "microstegium_points.shp")
# st_write(microstegium, dsn = "microstegium_points.shp",
#          layer = "microstegium_points.shp",
#          driver = "ESRI Shapefile")

microstegium
plot(st_geometry(microstegium))

cogongrass <- st_read("data/Imperata/points.shp")
plot(st_geometry(cogongrass))

blanding_cogon <- st_read("data/CampBlanding/Cogon_Infestations.shp")
blanding_cogon <- st_transform(blanding_cogon, 4326)
plot(st_geometry(blanding_cogon), col = "red")
plot(st_geometry(cogongrass), add = T)


# Convert to Spatial Object -----------------------------------------------

