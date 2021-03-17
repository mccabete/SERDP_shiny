# Split woody subplot data to installations

## CHECK: depends on source("R_scripts/subplot-woody-species-qaqc.R")

library(plyr); library(dplyr);
library(readr)
library(stringi)

subplot_data <- read_csv("data/processed_data/woodysubplot.csv")

# Begin filtering by installation ####

#### Filter for Camp Blanding ####

woody_subplot_blanding <- subplot_data %>%
      filter(installation=="blanding") %>%
      select(plot_id, date, visit_year, species_name, stems_100m2)

write_csv(woody_subplot_blanding, "data/processed_by_installation/camp_blanding/woody_subplot_blanding.csv")

#### Filter for Avon Park AFR ####

woody_subplot_avonpark <- subplot_data %>%
      filter(installation=="avonpark") %>%
      select(plot_id, date, visit_year, species_name, stems_100m2)

write_csv(woody_subplot_avonpark, "data/processed_by_installation/avon_park_afr/woody_subplot_avonpark.csv")

#### Filter for Eglin AFB ####

woody_subplot_eglin <- subplot_data %>%
      filter(installation=="eglin") %>%
      select(plot_id, date, visit_year, species_name, stems_100m2)

write_csv(woody_subplot_eglin, "data/processed_by_installation/eglin_afb/woody_subplot_eglin.csv")

#### Filter for Tyndall AFB ####

woody_subplot_tyndall <- subplot_data %>%
      filter(installation=="tyndall") %>%
      select(plot_id, date, visit_year, species_name, stems_100m2)

write_csv(woody_subplot_tyndall, "data/processed_by_installation/tyndall_afb/woody_subplot_tyndall.csv")

#### Filter for Fort Jackson ####

woody_subplot_jackson <- subplot_data %>%
      filter(installation=="jackson") %>%
      select(plot_id, date, visit_year, species_name, stems_100m2)

write_csv(woody_subplot_jackson, "data/processed_by_installation/fort_jackson/woody_subplot_jackson.csv")

#### Filter for Fort Benning ####

woody_subplot_benning <- subplot_data %>%
      filter(installation=="benning") %>%
      select(plot_id, date, visit_year, species_name, stems_100m2)

write_csv(woody_subplot_benning, "data/processed_by_installation/fort_benning/woody_subplot_benning.csv")

#### Filter for Camp Shelby ####

woody_subplot_shelby <- subplot_data %>%
      filter(installation=="shelby") %>%
      select(plot_id, date, visit_year, species_name, stems_100m2)

write_csv(woody_subplot_shelby, "data/processed_by_installation/camp_shelby/woody_subplot_shelby.csv")

#### Filter for Fort Gordon ####

woody_subplot_gordon <- subplot_data %>%
      filter(installation=="gordon") %>%
      select(plot_id, date, visit_year, species_name, stems_100m2)

write_csv(woody_subplot_gordon, "data/processed_by_installation/fort_gordon/woody_subplot_gordon.csv")

#### Filter for Moody AFB ####

woody_subplot_moody <- subplot_data %>%
      filter(installation=="moody") %>%
      select(plot_id, date, visit_year, species_name, stems_100m2)

write_csv(woody_subplot_moody, "data/processed_by_installation/moody_afb/woody_subplot_moody.csv")
