## Manipulate DEM

library(sp)
library(rgeos)
library(rgdal)
library(maptools)
library(sf)
library(raster)
library(rasterVis)

# avp_dem1 <- raster("data/AvonPark/dem_files/ned19_n27x75_w081x25_fl_kissimmeeriver_2011.img")

avp_dem <- raster::merge(
      raster("data/AvonPark/dem_files/ned19_n28x00_w081x50_fl_polk_east_2007.img"),
      raster("data/AvonPark/dem_files/ned19_n27x75_w081x50_fl_polk_east_2007.img"),
      raster("data/AvonPark/dem_files/ned19_n27x75_w081x25_fl_polk_east_2007.img"),
      raster("data/AvonPark/dem_files/ned19_n27x75_w081x25_fl_kissimmeeriver_2011.img"),
      raster("data/AvonPark/dem_files/ned19_n27x75_w081x50_fl_kissimmeeriver_2011.img")
      )
plot(avp_dem)

avp_boundary <- read_sf("data/AvonPark/Installation_Boundary.shp")
plot(avp_boundary, add = T)

# avp_dem <- raster::mask(avp_dem, avp_boundary)

writeRaster(raster::mask(avp_dem, avp_boundary),
            filename = "data/AvonPark/dem_clp.tif", format = "GTiff")


shelby_dem <- raster::merge(
      raster("data/CampShelby/dem_files/ned19_n31x00_w089x00_ms_campshelby_2006.img"),
      raster("data/CampShelby/dem_files/ned19_n31x25_w089x00_ms_campshelby_2006.img"),
      raster("data/CampShelby/dem_files/ned19_n31x25_w089x25_ms_campshelby_2006.img")
)
plot(shelby_dem)

shelby_boundary <- read_sf("data/CampShelby/SUP_Boundary.shp")
# plot(shelby_boundary, add=T)
s_buffer <- st_buffer(shelby_boundary, 1000)
s_buffer <- st_transform(s_buffer, projection(shelby_dem))
plot(s_buffer, add = T)

dem_clp <- mask(shelby_dem, s_buffer)
plot(dem_clp)

writeRaster(dem_clp, filename = "data/CampShelby/dem_clp.tif", format = "GTiff")


