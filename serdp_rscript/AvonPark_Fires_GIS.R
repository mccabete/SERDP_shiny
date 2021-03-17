# Avon Park GIS Data

# load packages ####
library(sp); library(rgdal); library(maptools); library(rgeos);
library(sf)
library(plyr); library(dplyr)

# common field names ####
field_names <- c("inst_name", "fArea_ac", "fCause", "fType", "fDate",
                 "season", "purpose", "fYear", "fMonth")


# read in shapefiles ####
# burn2017 <- readOGR("data/Avon-Park/fire_shapefiles/Burns_2017.shp")

## st_xxxx functions are from the sf package

shp_list <- list.files("data/AvonPark/fire_shapefiles/", pattern = "[shp]$")
shp_list <- strsplit(shp_list, ".shp")
# avp_fires <- st_read("data/AvonPark/fire_shapefiles/Burns_2006.shp")
for(shp in shp_list){
      assign(shp, st_read(dsn = "data/AvonPark/fire_shapefiles/",
                          layer = shp) %>%
                   st_transform(., crs = 26917))# match projections
      # avp_fires = st_join(avp_fires, df)
}
# remove duplicated(?) shapefiles
rm(fixed_Burns_2017)
rm(burn06)

# Combine shapefiles into one ####
## check field names: create and rename as necessary
# Burns_2006
avp_fires <- Burns_2006 %>%
      mutate(inst_name = "Avon Park AFR",
             fDate = as.Date(paste(Year,Month,Day, sep = "-")),
             fMonth = Month,
             season = case_when(
                   between(fMonth, 2,9) ~ "growing",
                   TRUE ~ "dormant"),
             purpose = NA) %>%
      select(inst_name,
             fArea_ac = Acres,
             fCause = Ignition,
             fType = Type,
             fDate,
             season,
             purpose,
             fYear = Year,
             fMonth)

avp_fires <- rbind(
      avp_fires,
      burn07 %>%
            mutate(inst_name = "Avon Park AFR",
                   fDate = as.Date(paste(Year_, Month_, day_, sep = "-")),
                   fMonth = Month_,
                   season = case_when(
                         between(fMonth, 2,9) ~ "growing",
                         TRUE ~ "dormant"),
                   purpose = NA) %>%
            select(inst_name,
                   fArea_ac = Acres,
                   fCause = Ignition,
                   fType = Type,
                   fDate,
                   season,
                   purpose,
                   fYear = Year_,
                   fMonth)
)

avp_fires <- rbind(avp_fires,
                   burn08 %>%
                         mutate(inst_name = "Avon Park AFR",
                                fDate = as.Date(paste(Year_, Month_, day_, sep = "-")),
                                fMonth = Month_,
                                season = case_when(
                                      between(fMonth, 2,9) ~ "growing",
                                      TRUE ~ "dormant"),
                                purpose = NA) %>%
                         select(inst_name,
                                fArea_ac = Acres,
                                fCause = Ignition,
                                fType = Type,
                                fDate,
                                season,
                                purpose,
                                fYear = Year_,
                                fMonth)
)

avp_fires <- rbind(avp_fires,
                   Burns_2009 %>%
                         mutate(inst_name = "Avon Park AFR",
                                fDate = as.Date(paste(Year_, Month_, day_, sep = "-")),
                                fMonth = Month_,
                                season = case_when(
                                      between(fMonth, 2,9) ~ "growing",
                                      TRUE ~ "dormant"),
                                purpose = NA) %>%
                         select(inst_name,
                                fArea_ac = Acres,
                                fCause = Ignition,
                                fType = Type,
                                fDate,
                                season,
                                purpose,
                                fYear = Year_,
                                fMonth)
)

avp_fires <- rbind(avp_fires,
                   Burns_2010 %>%
                         mutate(inst_name = "Avon Park AFR",
                                fDate = as.Date(paste(Year_, Month_, day_, sep = "-")),
                                fMonth = Month_,
                                season = case_when(
                                      between(fMonth, 2,9) ~ "growing",
                                      TRUE ~ "dormant"),
                                purpose = NA) %>%
                         select(inst_name,
                                fArea_ac = Acres,
                                fCause = Ignition,
                                fType = Type,
                                fDate,
                                season,
                                purpose,
                                fYear = Year_,
                                fMonth)
)

avp_fires <- rbind(avp_fires,
                   Burns_2011 %>%
                         mutate(inst_name = "Avon Park AFR",
                                fDate = as.Date(paste(Year_, Month_, day_, sep = "-")),
                                fMonth = Month_,
                                season = case_when(
                                      between(fMonth, 2,9) ~ "growing",
                                      TRUE ~ "dormant"),
                                purpose = NA) %>%
                         select(inst_name,
                                fArea_ac = Acres,
                                fCause = Ignition,
                                fType = Type,
                                fDate,
                                season,
                                purpose,
                                fYear = Year_,
                                fMonth)
)


avp_fires <- rbind(avp_fires,
                   Burns_2012 %>%
                         mutate(inst_name = "Avon Park AFR",
                                fDate = as.Date(paste(Year_, Month_, day_, sep = "-")),
                                fMonth = Month_,
                                season = case_when(
                                      between(fMonth, 2,9) ~ "growing",
                                      TRUE ~ "dormant"),
                                purpose = NA) %>%
                         select(inst_name,
                                fArea_ac = Acres,
                                fCause = Ignition,
                                fType = Type,
                                fDate,
                                season,
                                purpose,
                                fYear = Year_,
                                fMonth)
)

avp_fires <- rbind(avp_fires,
                   Burns_2013 %>%
                         mutate(inst_name = "Avon Park AFR",
                                fDate = as.Date(paste(Year_, Month_, day_, sep = "-")),
                                fMonth = Month_,
                                season = case_when(
                                      between(fMonth, 2,9) ~ "growing",
                                      TRUE ~ "dormant"),
                                purpose = NA) %>%
                         select(inst_name,
                                fArea_ac = Acres,
                                fCause = Ignition,
                                fType = Type,
                                fDate,
                                season,
                                purpose,
                                fYear = Year_,
                                fMonth)
)

avp_fires <- rbind(avp_fires,
                   Burns_2014 %>%
                         mutate(inst_name = "Avon Park AFR",
                                fDate = as.Date(paste(Year_, Month_, day_, sep = "-")),
                                fMonth = Month_,
                                season = case_when(
                                      between(fMonth, 2,9) ~ "growing",
                                      TRUE ~ "dormant"),
                                purpose = NA) %>%
                         select(inst_name,
                                fArea_ac = Acres,
                                fCause = Ignition,
                                fType = Type,
                                fDate,
                                season,
                                purpose,
                                fYear = Year_,
                                fMonth)
)

avp_fires <- rbind(avp_fires,
                   Burns_2015 %>%
                         mutate(inst_name = "Avon Park AFR",
                                fDate = as.Date(paste(Year_, Month_, day_, sep = "-")),
                                fMonth = Month_,
                                season = case_when(
                                      between(fMonth, 2,9) ~ "growing",
                                      TRUE ~ "dormant"),
                                purpose = NA) %>%
                         select(inst_name,
                                fArea_ac = Acres,
                                fCause = Ignition,
                                fType = Type,
                                fDate,
                                season,
                                purpose,
                                fYear = Year_,
                                fMonth)
)

avp_fires <- rbind(avp_fires,
                   Burns_2016 %>%
                         mutate(inst_name = "Avon Park AFR",
                                fDate = as.Date(paste(Year_, Month_, Day_, sep = "-")),
                                fMonth = Month_,
                                season = case_when(
                                      between(fMonth, 2,9) ~ "growing",
                                      TRUE ~ "dormant"),
                                purpose = NA) %>%
                         select(inst_name,
                                fArea_ac = Acres,
                                fCause = Ignition,
                                fType = Type,
                                fDate,
                                season,
                                purpose,
                                fYear = Year_,
                                fMonth)
)

avp_fires <- rbind(avp_fires,
                   Burns_2017 %>%
                         mutate(inst_name = "Avon Park AFR",
                                fDate = as.Date(paste(Year_, Month_, Day_, sep = "-")),
                                fMonth = Month_,
                                season = case_when(
                                      between(fMonth, 2,9) ~ "growing",
                                      TRUE ~ "dormant"),
                                purpose = NA) %>%
                         select(inst_name,
                                fArea_ac = Acres,
                                fCause = Ignition,
                                fType = Type,
                                fDate,
                                season,
                                purpose,
                                fYear = Year_,
                                fMonth)
)

avp_fires <- rbind(avp_fires,
                   Burns_2018 %>%
                         mutate(inst_name = "Avon Park AFR",
                                fDate = as.Date(Date),
                                fMonth = lubridate::month(fDate),
                                fYear = lubridate::year(fDate),
                                season = case_when(
                                      between(fMonth, 2,9) ~ "growing",
                                      TRUE ~ "dormant"),
                                purpose = NA) %>%
                         select(inst_name,
                                fArea_ac = Acres,
                                fCause = Cause,
                                fType = Type,
                                fDate, season, purpose, fYear, fMonth)
)


rm(list=ls(pattern = "urn"))
rm(list=ls(pattern = "shp"))

# burn06$Type[burn06$Type=="prescribed"] <- "Prescribed"
# burn06@data <- droplevels(burn06@data)
# writeOGR(burn06, "data/AvonPark/fire_shapefiles/", "Burns_2006", driver = "ESRI Shapefile", overwrite_layer = T)
# summary(burn07)
# summary(burn08)
# summary(Burns_2009)
# Burns_2009$Type[Burns_2009$Type=="Prescibed"] <- "Prescribed"
# Burns_2009@data <- droplevels(Burns_2009@data)
# writeOGR(Burns_2009, "data/AvonPark/fire_shapefiles/", "Burns_2009", driver = "ESRI Shapefile", overwrite_layer = T)
# summary(Burns_2010)
# summary(Burns_2011)
# summary(Burns_2012)
# summary(Burns_2013)
# summary(Burns_2014)
# summary(Burns_2015)
# Burns_2015$Type[Burns_2015$Type=="Prescibed"|Burns_2015$Type=="Presrcibed"] <- "Prescribed"
# Burns_2015@data <- droplevels(Burns_2015@data)
# writeOGR(Burns_2015, "data/AvonPark/fire_shapefiles/", "Burns_2015", driver = "ESRI Shapefile", overwrite_layer = T)
# summary(Burns_2016)
# summary(Burns_2017)

# names(blanding_fire)
#
# rm(shp)
# rm(eglin_fires)
# shp_list <- as.list(.GlobalEnv)
# for(shp in shp_list){
#       df <- shp[shp@data$Type=="Prescribed",]
#       # df <-
#       writeOGR(df,
#                dsn = "data/AvonPark/fire_shapefiles/",
#                layer = paste("AvonPark_rxfire", sep=""),
#                driver = "ESRI Shapefile", overwrite_layer = T)
# }

# all_invasives <- readOGR("data/AvonPark/APAFR_AllInvasives_Merge2.shp")
# summary(all_invasives)
# unique(all_invasives$CommonName)
# avp_cogongrass <- all_invasives[all_invasives$CommonName=="Cogon Grass",]
# avp_cogongrass@data <- droplevels(avp_cogongrass@data)
# writeOGR(avp_cogongrass, "data/AvonPark/", "avp_cogon", driver = "ESRI Shapefile")
