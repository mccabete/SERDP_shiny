# Fire Data
library(sp); library(rgdal); library(maptools); library(rgeos);
library(sf)
# library(plyr); library(dplyr);
library(ggplot2)
#library(RSAGA)
# library(raster)

plot_locations <- st_read("data/plot_visit.shp")

plot_locations <- plot_locations %>%
      dplyr::rename(inst = instal,
             # visit_date = vist_dt,
             # last_fire = lst_fr_,
             # yrs_since_fire = yrs_sn_,
             visit_yr = vist_yr,
             imcy = imcy_nv) %>%
      dplyr::filter(!is.na(visit_yr)) %>%
      dplyr::filter(!duplicated(plot_id)) %>%
      dplyr::select(name:imcy)

# summary(plot_locations)
# filter(plot_locations, is.na(last_fire))$plot_id
## no fire history for shelby k1

apafr_fires <- st_read("data/AvonPark/avp_fires_All.shp")
# summary(apafr_fires)
apafr_fires <- select(apafr_fires, -FID)

benning_fires <- st_read("data/FtBenning/benning_fires_2000_2018.shp")

blanding_fires <- st_read("data/CampBlanding/blanding_fires_200112_201804.shp")

eglin_fires <- st_read("data/EglinAFB/eglin_fires_2002_2018.shp")

gordon_fires <- st_read("data/FtGordon/gordon_fire_199501_201806.shp")

jackson_fires <- st_read("data/FtJackson/jackson_fires_2002_2018.shp")

moody_fires <- st_read("data/MoodyAFB/moody_fires_2002_2017.shp")

shelby_fires <- st_read("data/CampShelby/shelby_fires_2002_2019.shp")

tyndall_fires <- st_read("data/Tyndall/tyndall_fires_2000_2018.shp")



fire_master <- rbind(
      st_transform(apafr_fires, crs = 4326),
      st_transform(benning_fires, crs = 4326),
      st_transform(blanding_fires, crs = 4326),
      st_transform(eglin_fires, crs = 4326),
      st_transform(gordon_fire, crs = 4326),
      st_transform(jackson_fires, crs = 4326),
      st_transform(moody_fires, crs = 4326),
      st_transform(shelby_fires, crs = 4326),
      st_transform(tyndall_fires, crs = 4326)
)

str(fire_master)
# rm(list = ls(pattern = "_fir"))
fire_master$fYear <- as.numeric(fire_master$fYear)
summary(fire_master)

fire_master %>%
      filter(is.na(fYear))

fire_master$purpose <- tolower(fire_master$purpose)
sort(unique(fire_master$purpose))


fire_master$fType <- tolower(fire_master$fType)
sort(unique(fire_master$fType))

fire_master <- fire_master %>%
      mutate(fType = case_when(
            grepl("wild", fType) ~ "wildfire",
            grepl("wf", fType) ~ "wildfire",
            grepl("pres", fType) ~ "prescribed",
            grepl("rx", fType) ~ "prescribed",
            grepl("site", fType) ~ "prescribed",
            grepl("na", fType) ~ "unknown",
            grepl("hab", purpose) ~ "prescribed",
            grepl("res", purpose) ~ "prescribed",
            grepl("shelt", purpose) ~ "prescribed",
            grepl("safety", purpose) ~ "prescribed",
            grepl("training", purpose) ~ "wildfire",
            grepl("unpl", purpose) ~ "wildfire",
            grepl("seed", purpose) ~ "prescribed",
            TRUE ~ fType
      ))

(fire_master %>%
      filter(fType=="unknown"))$fCause

(fire_master %>%
            filter(fCause=="man"))$inst_name

(fire_master %>%
            filter(fType=="unknown"))$purpose

fire_master$fCause <- tolower(fire_master$fCause)
sort(unique(fire_master$fCause))
(pattern_check <- grepl(pattern = "ar", fire_master$fCause, fixed = T))
unique(pattern_check)
pattern_check[pattern_check==TRUE] %>% length(.)

fire_master <- fire_master %>%
      mutate(fCause = case_when(
            grepl("esc", fCause)  ~ "escaped",
            grepl("jump", fCause)  ~ "escaped",
            grepl("wf", fCause) ~ "lightning",
            grepl("wildfire", fCause) ~ "lightning",
            grepl("back", fCause)  ~ "prescribed",
            grepl("rx", fCause) ~ "prescribed",
            grepl("flank", fCause) ~ "prescribed",
            grepl("head", fCause) ~ "prescribed",
            grepl("ground", fCause) ~ "prescribed",
            grepl("reburn", fCause) ~ "prescribed",
            grepl("train", fCause) ~ "prescribed",
            grepl("ground", fCause) ~ "prescribed",
            grepl("eng", fCause) ~ "prescribed",
            grepl("man", fCause) ~ "prescribed",
            grepl("presc", fCause) ~ "prescribed",
            grepl("rekindle", fCause) ~ "prescribed",
            fType=="prescribed" ~ "prescribed",
            grepl("mil", fCause) ~ "mission",
            grepl("ar", fCause) ~ "mission",
            grepl("203", fCause) ~ "mission",
            grepl("aer", fCause) ~ "mission",
            grepl("af", fCause) ~ "mission",
            grepl("eod", fCause) ~ "mission",
            grepl("c4", fCause) ~ "mission",
            grepl("demo", fCause) ~ "mission",
            grepl("equip", fCause) ~ "mission",
            grepl("gren", fCause) ~ "mission",
            grepl("ordn", fCause) ~ "mission",
            grepl("rotor", fCause) ~ "mission",
            grepl("rz", fCause) ~ "mission",
            grepl("seek", fCause) ~ "mission",
            grepl("tracer", fCause) ~ "mission",
            grepl("troops", fCause) ~ "mission",
            grepl("uav", fCause) ~ "mission",
            grepl("utv", fCause) ~ "mission",
            grepl("light", fCause) ~ "lightning",
            grepl("lght", fCause) ~ "lightning",
            grepl("pow", fCause) ~ "powerline",
            grepl("camp", fCause)  ~ "other human",
            grepl("child", fCause)  ~ "other human",
            grepl("fireworks", fCause)  ~ "other human",
            grepl("na", fCause)  ~ "unknown",
            is.na(fCause) ~ "unknown",
            TRUE ~ fCause
            )
            )

str(fire_master)

fire_master$season <- tolower(fire_master$season)
unique(fire_master$season)

filter(fire_master, season=="ld/eg") %>% summary(.)

fire_master <- fire_master %>%
      mutate(season = case_when(
            season=="d" | grepl("dor", season) ~ "dormant",
            season=="winter" ~ "dormant",
            season=="g" | grepl("gro", season) ~ "growing",
            season=="spring" | season == "summer" ~ "growing",
            season=="ld/eg" ~ "growing"

      ))

# summary(fire_master)


st_write(select(fire_master, -geometry),
         "data/gis_files/all_installations_fires.shp")

fire_master_df <- as.data.frame(fire_master)


readr::write_csv(select(fire_master_df, -geometry),
                 "data/processed_data/all_installations_fires.csv")

# filter(fire_master, fCause=="proposed")

## Working on calculation a fire return interval ####

# plot(rxfire[1])
# plot(st_centroid(rxfire, of), add = T, pch = 3, size = 5)
# plot(tmp)
# st_point_on_surface(rxfire)

plot_locations %>%
      filter(inst=="blanding", visit_yr==2017)
blanding_fires %>%
      filter(fYear==2017) %>%
      summary(.)

temp <- fire_master
temp$FID <- seq(1,nrow(temp),1)
st_simplify(temp)
# temp1 <- gCentroid(temp, byid = T)
#
# temp1 <- SpatialPointsDataFrame(temp1, temp[1])

tmp <- st_centroid(temp, of_largest_polygon = T)
tmp <- select(tmp, FID)


tmp <- st_join(tmp, temp)

fri18 <- tmp %>%
      filter(fYear > 2002) %>%
      group_by(FID.x) %>%
      summarise(
            lastBrn18 = 2018-max(fYear, na.rm = TRUE),
            # lastBrn17 = 2017-max(fYear, na.rm = T),
            fires = length(FID.x),
            fri15yr_2018 = 15/fires,
            frindex_2018 = (1/fri15yr_2018)/lastBrn18)

fri18 <- st_join(temp,
                 select(fri18, fri15yr_2018, frindex_2018), largest = TRUE)

summary(fri18)

# plot(tmp[14])
# plot(plot_locations[1], add = T, pch = 3, size = 10)

### ### ### ### ### ###
## Seems that it will be better to calculate FRI per installation and then merge the data together
### ### ### ### ### ###

# st_crs(rxfire)
testFRI <- st_transform(plot_locations, crs = st_crs(fri18)) %>%
      mutate(ptFID = seq(1, nrow(.),1)) %>%
      st_join(.,
              select(fri18, fri15yr_2018, frindex_2018)) %>%
      filter(!duplicated(ptFID)) %>%
      arrange(inst)
testFRI

fri17 <- tmp %>%
      filter(visit_yr == 2017, fYear<2018) %>%
      group_by(plot_id) %>%
      summarise(
            lastBrn = 2017-max(fYear, na.rm = T),
            fires = length(FID),
            fri15yr = 15/fires,
            frindex = (1/fri15yr)/lastBrn)



fri <- st_join(blanding_fire, temp17)
fri <- fri %>%
      filter(!is.na(fri15yr)) %>%
      select(plot_id, fri15yr, fType)
plot(fri[1])

# detach("package:raster", unload=TRUE)

plot(gDifference(rxfire2012, rxfire[rxfire$burnYr>2012,], byid = T), col='red',pbg='white')
plot(gDifference(rxfire2012, rxfire2015), col='red',pbg='white', add=T)



# Intersect shapefiles ####
## Goal: calculate fire return interval for polygons of time since last fire

# intersecting polygons, ends up including small overlapping or touching areas
rxfire2017intersect <- intersect(rxfire2017,rxfire)
summary(rxfire2017intersect)
plot(rxfire2017intersect, col = rxfire2017intersect$burn_year)
plot(rxfire2017, add = T)

# rxfire2017@data <- left_join(
#       rxfire2017@data,
#       rxfire2017intersect@data %>%
#       group_by(FID.1) %>%
#       # length(FID.2)
#       summarise(
#             fires = length(FID.2),
#             fire_frequency = length(FID.2)/(max(burn_year)-min(burn_year))
#             ),
#       by = c("FID"="FID.1"))


# rxfire2015intersect <- intersect(rxfire2015,rxfire)
# summary(rxfire2015intersect)
# plot(rxfire2015intersect, col = rxfire2015intersect$burn_year)
# plot(rxfire2015, add = T)
# filter(rxfire2015intersect@data, FID==578)

# crs(plot_locations)
# plot_locations <- spTransform(plot_locations, CRSobj = crs(rxfire))
# intrsct <- intersect(plot_locations, rxfire)
# intrsct@data %>% group_by(name) %>% summarise(fri = length(name))

## Tool in development that might get what we want directly at some point
# devtools::install_github("openfigis/RFigisGeo")
# # library(RFigisGeo)
# rxfire2015intersect2 <- RFigisGeo::intersection(rxfire2015, rxfire)
# filter(rxfire2015intersect2@data, FID==578)


## Generate polygon centroids, intersect these with all Rx fires and join the attributes back to the original polygon shapefile

rxfire2015centroids <- gCentroid(rxfire2015, byid = T)
rxfire2015centroids <- SpatialPointsDataFrame(
      rxfire2015centroids, #SpatialPoints object
      rxfire2015[1]@data #Dataframe
      )
names(rxfire2015centroids) <- "ptFID"
# extract(rxfire2015, rxfire2015centroids)
## Check to make sure 'FID' numbers match up between centroids and polygons
intersect(rxfire2015centroids, rxfire2015)@data

## Intersect the identified centroids with the multipolygon shapefile
rxfire2015centroids <- intersect(rxfire2015centroids, rxfire)
# length(unique(rxfire2015centroids$FID))


## Join data back to the 2015 prescribed fire polygon shapefile attributes
rxfire2015@data <- left_join(
      rxfire2015@data,
      ## Summarise data to calculate number of fires and fire frequency
      rxfire2015centroids@data %>%
            filter(burnYr <= 2015) %>%
            group_by(ptFID) %>%
            summarise(
                  fires = length(FID),
                  fri10yr = 10/fires
            ),
      by = c("FID"="ptFID")
      )



