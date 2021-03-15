#' # Script for doing QA/QC on the canopy cover data

#' ## Canopy Cover Data
#'

#+ load packages ####
library(plyr); library(dplyr);
library(readr)
library(stringi)
library(ggplot2)

#+ load plot visit data ####
plot_visit_data <- read_csv("data/processed_data/plot_visit_data.csv")
summary(plot_visit_data)
n_distinct(plot_visit_data$plot_id)

#+ load canopy cover data ####
canopy_cover <- read_csv("data/raw_data/densiometer-data.csv")
summary(canopy_cover)
unique(canopy_cover$plot_id)
unique(canopy_cover$installation)

#' * 4 `NA` values in 'fill_dots' & 'distance'
#' * need to create a unique plot id
#'
#' The distance is a bit complicated because of the way sampling was changed from year 1 to year 2. I think we keep all observations for now because we could relate the canopy cover at the farther distances to the tick trapping in year 1.
#'

#+ check installation and plot_id NAs ####
filter(canopy_cover, is.na(installation))
filter(canopy_cover, is.na(plot_id))
## extra rows; Excel artifact
## remove extra rows and extra columns

canopy_cover <- canopy_cover %>%
      mutate(plot_id = paste(installation, plot_id, sep = " "))
summary(canopy_cover)

#+ check distance and fill_dots NAs ####
filter(canopy_cover, is.na(fill_dots))
## probably pre-calculated the canopy cover...not sure with distance; since it was early in the project I would assume that it was at the 10m distance
filter(canopy_cover, plot_id=="blanding c1")
canopy_cover$distance[is.na(canopy_cover$distance)] <- 10
summary(canopy_cover)

#+ check unique installation and plot id ####
unique(canopy_cover$plot_id)# 95 plots
unique(plot_visit_data$plot_id)# 95 plots
canopy_cover$plot_id[canopy_cover$plot_id=="blanding cogan"] <- "blanding theater_cogon"

# Check for missing in the canopy cover data
unique(plot_visit_data$plot_id) %in% unique(canopy_cover$plot_id)
anti_join(plot_visit_data, canopy_cover)$plot_id
## missing canopy cover data for Blanding D1 from 2017-06-20 visit; do we have this data somewhere? Drew's phone/pics?
### I found these pics and entered them into the data entry sheet on Dropbox

summary(canopy_cover)

#+ write out processed data
write_csv(canopy_cover, "data/processed_data/canopy-cover.csv")

canopy_cover <- read_csv("data/processed_data/canopy-cover.csv")
summary(canopy_cover)

filter(canopy_cover, is.na(fill_dots)) %>%
  select(date, plot_id, direction, fill_dots, pct_canopy_cover)

filter(canopy_cover, plot_id=="blanding c1") %>%
  select(date, plot_id, direction, fill_dots, pct_canopy_cover)

canopy_cover$fill_dots[canopy_cover$plot_id=="blanding c1"
                       & canopy_cover$direction=="N"] <- 23.3
canopy_cover$fill_dots[canopy_cover$plot_id=="blanding c1"
                       & canopy_cover$direction=="S"] <- 24.7
canopy_cover$fill_dots[canopy_cover$plot_id=="blanding c1"
                       & canopy_cover$direction=="E"] <- 44.5
canopy_cover$fill_dots[canopy_cover$plot_id=="blanding c1"
                       & canopy_cover$direction=="W"] <- 37

## Added missing fill_dots in blanding c1 ##

canopy_cover$fill_dots[canopy_cover$date=="20180515" & canopy_cover$plot_id=="blanding c1" &
                       canopy_cover$direction=="N"] <- 27
canopy_cover$fill_dots[canopy_cover$date=="20180515" & canopy_cover$plot_id=="blanding c1" &
                         canopy_cover$direction=="S"] <- 23
canopy_cover$fill_dots[canopy_cover$date=="20180515" & canopy_cover$plot_id=="blanding c1" &
                         canopy_cover$direction=="E"] <- 15
canopy_cover$fill_dots[canopy_cover$date=="20180515" & canopy_cover$plot_id=="blanding c1" &
                         canopy_cover$direction=="W"] <- 77
## Messed up and forgot dates, fixing the 2018 blanding c1's ##

canopy_cover$date[canopy_cover$plot_id=="blanding c1" & canopy_cover$date==20170607] <- 20170609

canopy_cover$date[canopy_cover$plot_id=="avonpark a2" & canopy_cover$date==20180601] <- 20180531
canopy_cover$date[canopy_cover$plot_id=="blanding c1" & canopy_cover$date==20180515] <- 20180516
canopy_cover$date[canopy_cover$plot_id=="shelby f1" & canopy_cover$date==20170923] <- 20170921



canopy_cover$date <- as.Date(as.character(canopy_cover$date), format = "%Y%m%d")
canopy_cover <- canopy_cover %>%
  filter(distance %in% c(10))

canopy_cover <- canopy_cover %>%
  mutate(visit_year = lubridate::year(canopy_cover$date),
         plot_id = case_when(
               plot_id=="gordon z1" ~ "gordon a1",
               plot_id=="gordon y1" ~ "gordon b1",
               plot_id=="gordon x1" ~ "gordon c1",
               plot_id=="gordon w1" ~ "gordon d1",
               plot_id=="gordon v1" ~ "gordon e1",
               plot_id=="gordon t1" ~ "gordon f1",
               plot_id=="gordon s1" ~ "gordon g1",
               plot_id=="gordon r1" ~ "gordon h1",
               TRUE ~ plot_id
         ))

summary(canopy_cover)

filter(canopy_cover, plot_id=="blanding c1") %>%
  select(installation, plot_id, date)

unique(canopy_cover$plot_id)

write_csv(canopy_cover, "data/processed_data/canopy-cover.csv")

#### Steven checked processing, looks good ####

canopy_grouped <- canopy_cover %>%
  group_by(installation, plot_id, date, visit_year) %>%
  summarise(avg_pct_canopy_cover = mean(pct_canopy_cover, na.rm = T))

summary(canopy_grouped)

#### Done grouping, combination of canopy, quad1m and quad25cm on new script: quadrat_biomass_canopy_cover-qaqc.R ####
