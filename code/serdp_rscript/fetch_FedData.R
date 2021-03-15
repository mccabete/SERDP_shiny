# Fetching Assorted US Federal Data
# https://ropensci.org/technotes/2017/08/24/feddata-release/

# install.packages("FedData")

library(FedData)
library(magrittr)


states_polygon <- polygon_from_extent(
      raster::extent(
            -91.6548995969999396, 24.5210516160000225,
             -78.5410844319998773, 35.2153129580000268),
            proj4string = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
      )
# plot(states_polygon)

ned <- get_ned(template = states_polygon, label = "RC2636_states")
# raster::plot(ned)

dymet <- get_daymet(
      template = states_polygon,
      label = "RC2636_states",
      elements = c("prcp","tmax"),
      years = 2017:2018
)
