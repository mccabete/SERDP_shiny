# Tree data by installation ####

## CHECK: depends on tree-data-qaqc.R

library(plyr); library(dplyr);
library(readr)

tree_data <- read_csv("data/processed_data/trees.csv")

#### Sorting raw data to each installation ####

trees_raw_blanding <- tree_data %>%
      filter(installation=="blanding") %>%
      select(plot_id, date, visit_year, tag, species_name, dbh, canopy, health,
             distance, azimuth, height, char)

write_csv(trees_raw_blanding, "data/processed_by_installation/camp_blanding/trees_raw_blanding.csv")

trees_raw_avonpark <- tree_data %>%
      filter(installation=="avonpark") %>%
      select(plot_id, date, visit_year, tag, species_name, dbh, canopy, health,
             distance, azimuth, height, char)

write_csv(trees_raw_avonpark, "data/processed_by_installation/avon_park_afr/trees_raw_avonpark.csv")

trees_raw_eglin <- tree_data %>%
      filter(installation=="eglin") %>%
      select(plot_id, date, visit_year, tag, species_name, dbh, canopy, health,
             distance, azimuth, height, char)

write_csv(trees_raw_eglin, "data/processed_by_installation/eglin_afb/trees_raw_eglin.csv")

trees_raw_tyndall <- tree_data %>%
      filter(installation=="tyndall") %>%
      select(plot_id, date, visit_year, tag, species_name, dbh, canopy, health,
             distance, azimuth, height, char)

write_csv(trees_raw_tyndall, "data/processed_by_installation/tyndall_afb/trees_raw_tyndall.csv")

trees_raw_jackson <- tree_data %>%
      filter(installation=="jackson") %>%
      select(plot_id, date, visit_year, tag, species_name, dbh, canopy, health,
             distance, azimuth, height, char)

write_csv(trees_raw_jackson, "data/processed_by_installation/fort_jackson/trees_raw_jackson.csv")

trees_raw_benning <- tree_data %>%
      filter(installation=="benning") %>%
      select(plot_id, date, visit_year, tag, species_name, dbh, canopy, health,
             distance, azimuth, height, char)

write_csv(trees_raw_benning, "data/processed_by_installation/fort_benning/trees_raw_benning.csv")

trees_raw_shelby <- tree_data %>%
      filter(installation=="shelby") %>%
      select(plot_id, date, visit_year, tag, species_name, dbh, canopy, health,
             distance, azimuth, height, char)

write_csv(trees_raw_shelby, "data/processed_by_installation/camp_shelby/trees_raw_shelby.csv")

trees_raw_gordon <- tree_data %>%
      filter(installation=="gordon") %>%
      select(plot_id, date, visit_year, tag, species_name, dbh, canopy, health,
             distance, azimuth, height, char)

write_csv(trees_raw_gordon, "data/processed_by_installation/fort_gordon/trees_raw_gordon.csv")

trees_raw_moody <- tree_data %>%
      filter(installation=="moody") %>%
      select(plot_id, date, visit_year, tag, species_name, dbh, canopy, health,
             distance, azimuth, height, char)

write_csv(trees_raw_moody, "data/processed_by_installation/moody_afb/trees_raw_moody.csv")

#### Summary of tree data to each installation ####

tree_data <- read_csv("data/processed_data/trees.csv")
summary(tree_data)
names(tree_data)

tree_data <- tree_data %>%
      mutate(health = if_else(health==0, "dead", "alive"),
             health = if_else(is.na(health)==TRUE, "alive", health))

write_csv(tree_data, "data/processed_data/trees.csv")

trees_grouped <- tree_data %>%
      group_by(installation, plot_id, date, visit_year, species, canopy, health, Genus, Species) %>%
      summarise(total_dbh = sum(dbh, na.rm = T),
                avg_height = mean(height, na.rm = T),
                avg_char = mean(char, na.rm = T))

trees_grouped <- trees_grouped %>%
      mutate(species_name = paste(Genus, Species, sep = " "))

summary(trees_grouped)

#### Filter for Camp Blanding ####

tree_summary_blanding <- trees_grouped %>%
      filter(installation=="blanding")

tree_summary_blanding <- tree_summary_blanding %>%
      ungroup(.) %>%
      select(plot_id, date, visit_year, species_name, canopy, health,
             total_dbh, avg_height, avg_char)

write_csv(tree_summary_blanding, "data/processed_by_installation/camp_blanding/tree_summary.csv")

#### Filter for Avon Park AFR ####

tree_summary_avonpark <- trees_grouped %>%
      filter(installation=="avonpark")

tree_summary_avonpark <- tree_summary_avonpark %>%
      ungroup(.) %>%
      select(plot_id, date, visit_year, species_name, canopy, health,
             total_dbh, avg_height, avg_char)

summary(tree_summary_avonpark)

write_csv(tree_summary_avonpark, "data/processed_by_installation/avon_park_afr/tree_summary.csv")

#### Filter for Eglin AFB ####

tree_summary_eglin <- trees_grouped %>%
      filter(installation=="eglin")

tree_summary_eglin <- tree_summary_eglin %>%
      ungroup(.) %>%
      select(plot_id, date, visit_year, species_name, canopy, health,
             total_dbh, avg_height, avg_char)

summary(tree_summary_eglin)

write_csv(tree_summary_eglin, "data/processed_by_installation/eglin_afb/tree_summary.csv")

#### Filter for Tyndall AFB ####

tree_summary_tyndall <- trees_grouped %>%
      filter(installation=="tyndall")

tree_summary_tyndall <- tree_summary_tyndall %>%
      ungroup(.) %>%
      select(plot_id, date, visit_year, species_name, canopy, health,
             total_dbh, avg_height, avg_char)

summary(tree_summary_tyndall)

write_csv(tree_summary_tyndall, "data/processed_by_installation/tyndall_afb/tree_summary.csv")

#### Filter for Fort Jackson ####

tree_summary_jackson <- trees_grouped %>%
      filter(installation=="jackson")

tree_summary_jackson <- tree_summary_jackson %>%
      ungroup(.) %>%
      select(plot_id, date, visit_year, species_name, canopy, health,
             total_dbh, avg_height, avg_char)

summary(tree_summary_jackson)

write_csv(tree_summary_jackson, "data/processed_by_installation/fort_jackson/tree_summary.csv")

#### Filter for Fort Benning ####

tree_summary_benning <- trees_grouped %>%
      filter(installation=="benning")

tree_summary_benning <- tree_summary_benning %>%
      ungroup(.) %>%
      select(plot_id, date, visit_year, species_name, canopy, health,
             total_dbh, avg_height, avg_char)

summary(tree_summary_benning)

write_csv(tree_summary_benning, "data/processed_by_installation/fort_benning/tree_summary.csv")

#### Filter for Camp Shelby ####

tree_summary_shelby <- trees_grouped %>%
      filter(installation=="shelby")

tree_summary_shelby <- tree_summary_shelby %>%
      ungroup(.) %>%
      select(plot_id, date, visit_year, species_name, canopy, health,
             total_dbh, avg_height, avg_char)

summary(tree_summary_shelby)

write_csv(tree_summary_shelby, "data/processed_by_installation/camp_shelby/tree_summary.csv")

#### Filter for Fort Gordon ####

tree_summary_gordon <- trees_grouped %>%
      filter(installation=="gordon")

tree_summary_gordon <- tree_summary_gordon %>%
      ungroup(.) %>%
      select(plot_id, date, visit_year, species_name, canopy, health,
             total_dbh, avg_height, avg_char)

summary(tree_summary_gordon)

write_csv(tree_summary_gordon, "data/processed_by_installation/fort_gordon/tree_summary.csv")

#### Filter for Moody AFB ####

tree_summary_moody <- trees_grouped %>%
      filter(installation=="moody")

tree_summary_moody <- tree_summary_moody %>%
      ungroup(.) %>%
      select(plot_id, date, visit_year, species_name, canopy, health,
             total_dbh, avg_height, avg_char)

summary(tree_summary_moody)

write_csv(tree_summary_moody, "data/processed_by_installation/moody_afb/tree_summary.csv")


#### Steven done filtering out tree summary data to installations ####
