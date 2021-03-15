## Extract drainage index to plot locations

library(sp)
library(rgdal)
library(maptools)
library(rgeos)
# library(raster)
library(sf)

plotlocs <- st_read("data/gis_files/plot-locations.shp")
unique(plotlocs$name)
plotlocs <- dplyr::filter(plotlocs, ! name %in% c("MovieTheaterCogon", "Cogon pop", "Cogon10x10", "CogonCoastal"))

di_raster <- raster::raster("data/gis_files/Drainage_Index_rasters/DI_tiffs/DI_2016_240m_CopyRaster")


extract2pts <- function(points, rgrid){
      point_values = raster::extract(x = rgrid, y = points)
      assign("extracted_values", cbind(points, point_values),
             envir = parent.frame())

      # return(extracted_values)

}

extract2pts(plotlocs, di_raster)
extracted_values <- extracted_values %>%
      dplyr::rename(drought_index = point_values)

readr::write_csv(as.data.frame(dplyr::select(extracted_values, plot_id, inst_name, drought_index)),
                 "data/processed_data/plot_drought_index.csv")
