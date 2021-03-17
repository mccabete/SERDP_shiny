# Script Description ------------------------------------------------------
### Name: ticks-by-installation.R
### Purpose: Summarize tick data for each installation generating data ### files to send to installation contacts along with the installation
### report.
###
### Authored by Steven Cabrera & Whalen Dillon
###
### Accuracy of output depends on tick-data-qaqc.R script


## Begin processing by installation ####
library(dplyr)
library(readr)

plot_visit_data <- read_csv("data/processed_data/plot_visit_data.csv")
tick_data <- read_csv("data/processed_data/ticks.csv")

ticks_grouped <- tick_data %>%
      group_by(installation, plot_id, visit_year, species_name, life_stage) %>%
      summarise(tick_count = sum(count, na.rm = T))

summary(ticks_grouped)

# is.na(ticks_grouped$species_name)
# ticks_grouped <- ticks_grouped %>%
# filter(!is.na(species_name), count>0) %>%
# select(installation, plot_id, date, visit_year, species_name, life_stage, tick_count)

## Filter for Camp Blanding ####

ticks_blanding <- ticks_grouped %>%
      filter(installation=="blanding", !is.na(species_name))

blanding_plots <- plot_visit_data %>%
      filter(installation=="blanding") %>%
      select(installation, plot_id, visit_year, plot_visit_date = visit_date)

ticks_blanding <- left_join(blanding_plots, ticks_blanding)

ticks_blanding <- ticks_blanding %>%
      mutate(tick_count = ifelse(is.na(tick_count)==TRUE, 0, tick_count)) %>%
      select(plot_id, plot_visit_date, visit_year, species_name, life_stage, tick_count)

write_csv(ticks_blanding, "data/processed_by_installation/camp_blanding/ticks_blanding.csv")

#### Filter for Avon Park ####

ticks_avonpark <- ticks_grouped %>%
      filter(installation=="avonpark", !is.na(species_name))

avonpark_plots <- plot_visit_data %>%
      filter(installation=="avonpark") %>%
      select(installation, plot_id, visit_year, plot_visit_date = visit_date)

ticks_avonpark <- left_join(avonpark_plots, ticks_avonpark)

ticks_avonpark <- ticks_avonpark %>%
      mutate(tick_count = ifelse(is.na(tick_count)==TRUE, 0, tick_count)) %>%
      select(plot_id, plot_visit_date, visit_year, species_name, life_stage, tick_count)

write_csv(ticks_avonpark, "data/processed_by_installation/avon_park_afr/ticks_avonpark.csv")

#### Filter for Eglin AFB ####

ticks_eglin <- ticks_grouped %>%
      filter(installation=="eglin", !is.na(species_name))

eglin_plots <- plot_visit_data %>%
      filter(installation=="eglin") %>%
      select(installation, plot_id, visit_year, plot_visit_date = visit_date)

ticks_eglin <- left_join(eglin_plots, ticks_eglin)

ticks_eglin <- ticks_eglin %>%
      mutate(tick_count = ifelse(is.na(tick_count)==TRUE, 0, tick_count)) %>%
      select(plot_id, plot_visit_date, visit_year, species_name, life_stage, tick_count)

write_csv(ticks_eglin, "data/processed_by_installation/eglin_afb/ticks_eglin.csv")

#### Filter for Tyndall AFB ####

ticks_tyndall <- ticks_grouped %>%
      filter(installation=="tyndall", !is.na(species_name))

tyndall_plots <- plot_visit_data %>%
      filter(installation=="tyndall") %>%
      select(installation, plot_id, visit_year, plot_visit_date = visit_date)

ticks_tyndall <- left_join(tyndall_plots, ticks_tyndall)

ticks_tyndall <- ticks_tyndall %>%
      mutate(tick_count = ifelse(is.na(tick_count)==TRUE, 0, tick_count)) %>%
      select(plot_id, plot_visit_date, visit_year, species_name, life_stage, tick_count)

write_csv(ticks_tyndall, "data/processed_by_installation/tyndall_afb/ticks_tyndall.csv")

#### Filter for Fort Jackson ####

ticks_jackson <- ticks_grouped %>%
      filter(installation=="jackson", !is.na(species_name))

jackson_plots <- plot_visit_data %>%
      filter(installation=="jackson") %>%
      select(installation, plot_id, visit_year, plot_visit_date = visit_date)

ticks_jackson <- left_join(jackson_plots, ticks_jackson)

ticks_jackson <- ticks_jackson %>%
      mutate(tick_count = ifelse(is.na(tick_count)==TRUE, 0, tick_count)) %>%
      select(plot_id, plot_visit_date, visit_year, species_name, life_stage, tick_count)

write_csv(ticks_jackson, "data/processed_by_installation/fort_jackson/ticks_jackson.csv")

#### Filter for Fort Benning ####

ticks_benning <- ticks_grouped %>%
      filter(installation=="benning", !is.na(species_name))

benning_plots <- plot_visit_data %>%
      filter(installation=="benning") %>%
      select(installation, plot_id, visit_year, plot_visit_date = visit_date)

ticks_benning <- left_join(benning_plots, ticks_benning)

ticks_benning <- ticks_benning %>%
      mutate(tick_count = ifelse(is.na(tick_count)==TRUE, 0, tick_count)) %>%
      select(plot_id, plot_visit_date, visit_year, species_name, life_stage, tick_count)

write_csv(ticks_benning, "data/processed_by_installation/fort_benning/ticks_benning.csv")

#### Filter for Camp Shelby ####

ticks_shelby <- ticks_grouped %>%
      filter(installation=="shelby", !is.na(species_name))

shelby_plots <- plot_visit_data %>%
      filter(installation=="shelby") %>%
      select(installation, plot_id, visit_year, plot_visit_date = visit_date)

ticks_shelby <- left_join(shelby_plots, ticks_shelby)

ticks_shelby <- ticks_shelby %>%
      mutate(tick_count = ifelse(is.na(tick_count)==TRUE, 0, tick_count)) %>%
      select(plot_id, plot_visit_date, visit_year, species_name, life_stage, tick_count)

write_csv(ticks_shelby, "data/processed_by_installation/camp_shelby/ticks_shelby.csv")

#### Filter for Fort Gordon####

ticks_gordon <- ticks_grouped %>%
      filter(installation=="gordon", !is.na(species_name))

gordon_plots <- plot_visit_data %>%
      filter(installation=="gordon") %>%
      select(installation, plot_id, visit_year, plot_visit_date = visit_date)

ticks_gordon <- left_join(gordon_plots, ticks_gordon)

ticks_gordon <- ticks_gordon %>%
      mutate(tick_count = ifelse(is.na(tick_count)==TRUE, 0, tick_count)) %>%
      select(plot_id, plot_visit_date, visit_year, species_name, life_stage, tick_count)

write_csv(ticks_gordon, "data/processed_by_installation/fort_gordon/ticks_gordon.csv")

#### Filter for Moody AFB ####

ticks_moody <- ticks_grouped %>%
      filter(installation=="moody", !is.na(species_name))

moody_plots <- plot_visit_data %>%
      filter(installation=="moody") %>%
      select(installation, plot_id, visit_year, plot_visit_date = visit_date)

ticks_moody <- left_join(moody_plots, ticks_moody)

ticks_moody <- ticks_moody %>%
      mutate(tick_count = ifelse(is.na(tick_count)==TRUE, 0, tick_count)) %>%
      select(plot_id, plot_visit_date, visit_year, species_name, life_stage, tick_count)

write_csv(ticks_moody, "data/processed_by_installation/moody_afb/ticks_moody.csv")

#### Steven stopped, question about multiple dates for all data and random NAs ####
# fixed NAs
