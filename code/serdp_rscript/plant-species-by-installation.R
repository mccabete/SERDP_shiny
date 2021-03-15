# Split species data to individual installations

## CHECK: depends on plant-species-data-qaqc.R

library(plyr); library(dplyr);
library(readr)

species_data <- read_csv("data/processed_data/species1m.csv")

#### Steven begin processing for individual installations ####

summary(species_data)
names(species_data)

species_grouped <- species_data %>%
      group_by(installation, plot_id, date, visit_year, veg_id, functional_group, Genus, Species) %>%
      summarise(avg_pct_cover = mean(pct_cover, na.rm = T),
                num_stems_m2 = sum(ht_under50cm, ht50_100cm, ht_over100cm, na.rm=T)/4)

summary(species_grouped)

species_grouped <- species_grouped %>%
      mutate(species_name = paste(Genus, Species, sep = " "))

# Added species_name concatenation of Genus & Species

filter(species_grouped, num_stems_m2>400)$plot_id
filter(species_data, plot_id=="avonpark e2", veg_id=="imcy") %>%
      select(ht_under50cm:ht_over100cm)

species_data <- species_data %>%
      filter(!is.na(transect_id))

write_csv(species_data, "data/processed_data/species1m.csv")

## Removed avon park cogon plots from species_data

species_data <- read_csv("data/processed_data/species1m.csv")

summary(species_grouped)

#### Filter for Fort Benning ####

species_benning <- species_grouped %>%
      filter(installation=="benning")

summary(species_benning)

species_benning <- filter(species_benning) %>%
      ungroup(.) %>%
      select(plot_id, date, visit_year, species_name, functional_group, avg_pct_cover, num_stems_m2)

write_csv(species_benning, "data/processed_by_installation/fort_benning/species_benning.csv")

#### Filter for Camp Blanding ####

species_blanding <- species_grouped %>%
      filter(installation=="blanding")

summary(species_blanding)

species_blanding <- filter(species_blanding) %>%
      ungroup(.) %>%
      select(plot_id, date, visit_year, species_name, functional_group, avg_pct_cover, num_stems_m2)

write_csv(species_blanding, "data/processed_by_installation/camp_blanding/species_blanding.csv")

#### Filter for Avon Park AFR ####

species_avonpark <- species_grouped %>%
      filter(installation=="avonpark")

summary(species_avonpark)

species_avonpark <- filter(species_avonpark) %>%
      ungroup(.) %>%
      select(plot_id, date, visit_year, species_name, functional_group, avg_pct_cover, num_stems_m2)

write_csv(species_avonpark, "data/processed_by_installation/avon_park_afr/species_avonpark.csv")

#### Filter for Eglin AFB ####

species_eglin <- species_grouped %>%
      filter(installation=="eglin")

summary(species_eglin)

species_eglin <- filter(species_eglin) %>%
      ungroup(.) %>%
      select(plot_id, date, visit_year, species_name, functional_group, avg_pct_cover, num_stems_m2)

write_csv(species_eglin, "data/processed_by_installation/eglin_afb/species_eglin.csv")

#### Filter for Tyndall AFB ####

species_tyndall <- species_grouped %>%
      filter(installation=="tyndall")

summary(species_tyndall)

species_tyndall <- filter(species_tyndall) %>%
      ungroup(.) %>%
      select(plot_id, date, visit_year, species_name, functional_group, avg_pct_cover, num_stems_m2)

write_csv(species_tyndall, "data/processed_by_installation/tyndall_afb/species_tyndall.csv")

#### Filter for Fort Jackson ####

species_jackson <- species_grouped %>%
      filter(installation=="jackson")

summary(species_jackson)

species_jackson <- filter(species_jackson) %>%
      ungroup(.) %>%
      select(plot_id, date, visit_year, species_name, functional_group, avg_pct_cover, num_stems_m2)

write_csv(species_jackson, "data/processed_by_installation/fort_jackson/species_jackson.csv")

#### Filter for Camp Shelby ####

species_shelby <- species_grouped %>%
      filter(installation=="shelby")

summary(species_shelby)

species_shelby <- filter(species_shelby) %>%
      ungroup(.) %>%
      select(plot_id, date, visit_year, species_name, functional_group, avg_pct_cover, num_stems_m2)

write_csv(species_shelby, "data/processed_by_installation/camp_shelby/species_shelby.csv")

#### Filter for Fort Gordon ####

species_gordon <- species_grouped %>%
      filter(installation=="gordon")

summary(species_gordon)

species_gordon <- filter(species_gordon) %>%
      ungroup(.) %>%
      select(plot_id, date, visit_year, species_name, functional_group, avg_pct_cover, num_stems_m2)

write_csv(species_gordon, "data/processed_by_installation/fort_gordon/species_gordon.csv")

#### Filter for Moody AFB ####

species_moody <- species_grouped %>%
      filter(installation=="moody")

summary(species_moody)

species_moody <- filter(species_moody) %>%
      ungroup(.) %>%
      select(plot_id, date, visit_year, species_name, functional_group, avg_pct_cover, num_stems_m2)

write_csv(species_moody, "data/processed_by_installation/moody_afb/species_moody.csv")

#### Completion of filtering all species to each of 9 installations ####
