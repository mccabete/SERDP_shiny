# Fort Jackson

# load packages ####
library(sp); library(rgdal); library(maptools); library(rgeos);
library(sf)
library(plyr); library(dplyr)

# common field names ####
field_names <- c("inst_name", "fArea_ac", "fCause", "fType", "fDate",
                 "season", "purpose", "fYear", "fMonth")

# load fire data ####
shp_list <- list.files("data/FtJackson/prescribedBurn/", pattern = "[shp]$", full.names = F)
path <- "data/FtJackson/prescribedBurn/"

yrs_15_18_list <- shp_list[2:5]

yr14 <- st_read(paste(path, shp_list[1], sep = ""))
yr10 <- st_read(paste(path, shp_list[14], sep = ""))
yr09 <- st_read(paste(path, shp_list[13], sep = ""))

yrs_02_08 <- shp_list[6:12]

# shp_list <- strsplit(shp_list, ".shp")


jackson_fires <- yr14
names(jackson_fires) <- tolower(names(jackson_fires))
jackson_fires <- jackson_fires %>%
      select(fDate = date_, fArea_ac = acres, fType = burn_type, comments) %>%
      mutate(comments = as.character(comments),
             fDate2 = as.character(fDate),
             fDate = lubridate::dmy(as.character(fDate)),
             inst_name = "Fort Jackson",
             fCause = NA,
             season = NA,
             purpose = NA,
             fYear = lubridate::year(fDate),
             fMonth = lubridate::month(fDate)
             ) %>%
      select(inst_name, fArea_ac, fCause, fType, fDate, season, purpose, fYear, fMonth, comments, fDate2)

# jackson_fires$fDate
# mutate(fYear = str_sub(paste(shp_list[3]), start = 15, end = 18))

projection <- st_crs(jackson_fires)
for(shp in yrs_02_08){

      sf = st_read(paste(path, shp, sep = ""))
      names(sf) = tolower(names(sf))

      sf = sf %>%
            select(fDate = date_, fArea_ac = acreage, fType = burn_type,
                   comments) %>%
            mutate(comments = as.character(comments),
                   fDate2 = as.character(fDate),
                   fDate = lubridate::ymd(as.character(fDate)),
                   inst_name = "Fort Jackson",
                   fCause = NA,
                   season = NA,
                   purpose = NA,
                   fYear = lubridate::year(fDate),
                   fMonth = lubridate::month(fDate)
            ) %>%
            select(inst_name, fArea_ac, fCause, fType, fDate, season, purpose, fYear, fMonth, comments, fDate2)

      sf = st_transform(sf, crs = projection)
      jackson_fires <- rbind(jackson_fires, sf)
      # assign(shp, sf)
}

# summary(jackson_fires)
# unique(jackson_fires$fDate)
# sf <- st_read(paste(path, yrs_15_18_list[1], sep = ""))
for(shp in yrs_15_18_list){

      sf = st_read(paste(path, shp, sep = ""))
      names(sf) = tolower(names(sf))

      sf = sf %>%
            select(fDate = yr_burned,
                   fArea_ac = acres,
                   season = burn_seaso,
                   comments) %>%
            mutate(fDate = as.character(fDate),
                   fDate = case_when(
                         nchar(fDate)==5 ~ paste(0, fDate, sep = ""),
                         TRUE ~ fDate),
                   fDate2 = fDate,
                   fDate = lubridate::mdy(fDate),
                   comments = as.character(comments),
                   inst_name = "Fort Jackson",
                   fCause = NA,
                   purpose = NA,
                   fType = NA,
                   fYear = lubridate::year(fDate),
                   fMonth = lubridate::month(fDate)
            ) %>%
            select(inst_name, fArea_ac, fCause, fType, fDate, season, purpose, fYear, fMonth, comments, fDate2)

      sf = st_transform(sf, crs = projection)
      jackson_fires <- rbind(jackson_fires, sf)
      # assign(shp, sf)
}

# str(jackson_fires)
# summary(jackson_fires)
#
# unique(jackson_fires$fType)
# unique(jackson_fires$season)
# unique(jackson_fires$comments)
# unique(jackson_fires$purpose)
# unique(jackson_fires$fCause)

jackson_fires <- jackson_fires %>%
      mutate(fType = tolower(as.character(fType)),
             season = tolower(as.character(season)),
             comments = tolower(comments)
             )

jackson_fires <- jackson_fires %>%
      mutate(
            season = case_when(
                  fType == "growing" ~ "growing",
                  fType == "dormant" ~ "dormant",
                  TRUE ~ season
            ),
            purpose = case_when(
                  fType == "seed bed" ~ "seed bed",
                  fType == "shelterwood" ~ "shelterwood",
                  TRUE ~ "NA"
            ),
            fCause = case_when(
                  comments == "helicopter" ~ "helicopter",
                  TRUE ~ "NA"
            ),
            fType = "prescribed"
            ) %>%
      select(-fDate2, -comments)

rm(list=ls(pattern = "yr"))
rm(projection)
rm(sf)

