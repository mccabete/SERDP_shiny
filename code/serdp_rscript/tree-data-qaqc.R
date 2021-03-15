#' # Script for doing QA/QC on tree data

library(plyr); library(dplyr);
library(readr)
library(stringi)
library(ggplot2)
library(tidyverse)

#+ load processed plot visit data ####
plot_visit_data <- read_csv("data/processed_data/plot_visit_data.csv")

#' ## Tree Stem Data
#+ tree data ####
tree_data <- read_csv("data/raw_data/trees.csv")
### tree_data_sub <- filter(tree_data, date>"2017-06-01", plot_id != "bland03", plot_id != "bland02")

summary(tree_data)

filter(tree_data, is.na(plot_id)) %>%
  select(date, installation, plot_id, record_id, stem_id, dbh, tag, species)

unique(tree_data$plot_id)

tree_data = filter(tree_data, !is.na(plot_id))

filter(tree_data, is.na(dbh)) %>%
  select(date, plot_id, record_id, stem_id, dbh, tag, species, health)

filter(tree_data, dbh==0) %>%
  select(date, plot_id, stem_id, tag, species, dbh)

tree_data$dbh[tree_data$tag=="1138"] <- 15.2
tree_data$dbh[tree_data$tag=="1139"] <- 14.0

### Corrected two tree DBH that were entered as 0          ###
### Two tree DBH are to be left NA, trees burned up/downed ###

filter(tree_data, dbh<3) %>%
  select(date, plot_id, stem_id, tag, species, dbh)

tree_data$dbh[tree_data$tag=="3056"] <- 25.7
tree_data$dbh[tree_data$tag=="4395"] <- 11.4

### Changed trees with dbh that were <3 with estimated values ###


tree_data$plot_id[tree_data$plot_id=="cogon plot"] <- "theater_cogon"

tree_data <- tree_data %>%
  mutate(plot_id = paste(installation, plot_id, sep = " "),
         plot_id=tolower(plot_id)) %>%
  filter(date>20170601)

filter(tree_data, is.na(azimuth), plot_id!="blanding theater_cogon") %>%
  select(date, stem_id, plot_id, distance, azimuth, species, tag, dbh)

filter(tree_data, is.na(azimuth), plot_id=="blanding theater_cogon") %>%
  select(date, stem_id, installation, plot_id, distance, azimuth, species, tag, dbh)

tree_data$azimuth[tree_data$tag=="4092"] <- 320
tree_data$azimuth[tree_data$tag=="4402"] <- 337
tree_data$azimuth[tree_data$tag=="4409"] <- 340
tree_data$azimuth[tree_data$tag=="4380"] <- 27
tree_data$azimuth[tree_data$tag=="4389"] <- 37
tree_data$azimuth[tree_data$tag=="4397"] <- 70
tree_data$azimuth[tree_data$tag=="4396"] <- 91
tree_data$azimuth[tree_data$tag=="4450"] <- 127
tree_data$azimuth[tree_data$tag=="4449"] <- 163
tree_data$azimuth[tree_data$stem_id=="2793"] <- 161

tree_data$azimuth[tree_data$tag=="3447"] <- 23.8

tree_data$azimuth[tree_data$stem_id=="3072"] <- 299.5
tree_data$azimuth[tree_data$stem_id=="3078"] <- 307.5
tree_data$azimuth[tree_data$stem_id=="3079"] <- 307.7

filter(tree_data, azimuth=="-3") %>%
  select(record_id, date, plot_id, tag, species, dbh, azimuth)

tree_data$azimuth[tree_data$tag=="3766"] <- 0

filter(tree_data, azimuth=="-1") %>%
  select(record_id, date, plot_id, tag, species, dbh, azimuth)

tree_data$azimuth[tree_data$tag=="3765"] <- 0
## Added azimuth values for trees in Benning A1/J1 and Gordon V1 ##
## Stem IDs 1199 and 1807 have no azimuth recorded ##

tree_data <- tree_data %>%
      mutate(plot_id = case_when(
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

unique(plot_visit_data$plot_id) %in% unique(tree_data$plot_id)

n_distinct(tree_data$plot_id)
n_distinct(plot_visit_data$plot_id)

summary(tree_data)

unique(tree_data$plot_id)

filter(tree_data, is.na(health), plot_id!="blanding theater_cogon") %>%
  select(installation, plot_id, date, tag, species, health, canopy)

tree_data$health[tree_data$plot_id=="jackson m1" & tree_data$tag=="3235"] <- 0

tree_data$health[tree_data$tag=="3888"] <- 0

## Added 0 values to tree health for 3235 and 3888 ##
## No health or canopy entered for tag 4083 ##


filter(tree_data, is.na(distance), plot_id!="blanding theater_cogon") %>%
  select(installation, plot_id, date, tag, species, distance)

tree_data$distance[tree_data$tag=="1755"] <- 11.5
## Distance -- Changed tag 1755, tag 120 Eglin P1 has no value NA ##

filter(tree_data, is.na(height)) %>%
  select(date, plot_id, stem_id, tag, species, dbh, height)

filter(tree_data, is.na(char)) %>%
  select(date, plot_id, stem_id, tag, species, dbh, char)

summary(tree_data)

filter(tree_data, distance=="124") %>%
  select(date, plot_id, stem_id, tag, species, dbh, distance)

tree_data$distance[tree_data$tag=="1995"] <- 12.4

filter(tree_data, distance=="121") %>%
  select(date, plot_id, stem_id, tag, species, dbh, distance)

tree_data$distance[tree_data$tag=="3933"] <- 12.1

filter(tree_data, distance=="101") %>%
  select(date, plot_id, stem_id, tag, species, dbh, distance)

tree_data$distance[tree_data$tag=="1996"] <- 10.0

filter(tree_data, distance>12.6) %>%
  select(date, plot_id, stem_id, tag, species, dbh, distance, azimuth)

tree_data$distance[tree_data$tag=="1386"] <- 7.3
tree_data$distance[tree_data$stem_id=="1321"] <- 10.5
tree_data$distance[tree_data$stem_id=="1383"] <- 10.9
tree_data$distance[tree_data$tag=="3034"] <- 8.9
tree_data$distance[tree_data$tag=="4223"] <- 8.3
tree_data$distance[tree_data$tag=="3775"] <- 5.3
tree_data$distance[tree_data$stem_id=="1818"] <- 8.1
## Corrected distances over 12.6 due to straight error or no decimal ##

filter(tree_data, height>50) %>%
  select(date, plot_id, stem_id, tag, species, dbh, height)

tree_data$height[tree_data$tag=="3650"] <- 7.6
tree_data$height[tree_data$tag=="4613"] <- 13.2
## Corrected unrealistic high heights that were missing decimal points ##

filter(tree_data, height<2) %>%
  select(date, plot_id, stem_id, tag, species, dbh, height)
## Low tree heights of 1.4, 1.4, 0.6 are correct ##

filter(tree_data, char>15) %>%
  select(date, plot_id, stem_id, tag, species, dbh, char)
tree_data$char[tree_data$tag=="3076"] <- 0.42
## Changed char from 42 to 0.42 ##

summary(tree_data)
tree_data$date <- as.Date(as.character(tree_data$date), format = "%Y%m%d")
tree_data <- tree_data %>%
  mutate(visit_year = lubridate::year(tree_data$date),
         )

unique(tree_data$plot_id)

# write_csv(tree_data, "data/processed_data/trees.csv")
#
# tree_data <- read_csv("data/processed_data/trees.csv")
# summary(tree_data)
# names(tree_data)

tree_data <- tree_data %>%
      rename(species_name = Species)

unique(tree_data$species_name)
unique(tree_data$Genus)

filter(tree_data, is.na(species), installation=="tyndall")$plot_id

tree_data$Genus[is.na(tree_data$species)==TRUE & tree_data$installation=="tyndall"] <- "Pinus"
tree_data$species_name[is.na(tree_data$species)==TRUE & tree_data$installation=="tyndall"] <- "ellioti"
tree_data$species[is.na(tree_data$species)==TRUE & tree_data$installation=="tyndall"] <- "PIEL"

filter(tree_data, is.na(species), plot_id=="shelby b2")

tree_data$Genus[tree_data$tag==1761 & tree_data$plot_id=="shelby b2"] <- "Quercus"

filter(tree_data, is.na(species), plot_id=="shelby c1")$health
tree_data$Genus[tree_data$azimuth==251 & tree_data$plot_id=="shelby c1"] <- "Pinus"
tree_data$Species[tree_data$azimuth==251 & tree_data$plot_id=="shelby c1"] <- "taeda"
tree_data$species[tree_data$azimuth==251 & tree_data$plot_id=="shelby c1"] <- "PITA"


tree_data <- tree_data %>%
  mutate(species_name = paste(Genus, Species, sep = " "))

unique(tree_data$plot_id)

write_csv(tree_data, "data/processed_data/trees.csv")


# Quick plot checks ####
# tree_data <- read_csv("data/processed_data/trees.csv")
# qplot(x = dbh, y = height, data = tree_data)
# qplot(x = height, y = char, data = tree_data) +
#       geom_abline(intercept = 0, slope = 1)
# ## all looks good - "outlier" values already checked
# # str(tree_data)
# # summary(tree_data)
#
# sort(unique(tree_data$species))
# filter(tree_data, species=="tlt") %>%
#       select(plot_id, dbh, date)
