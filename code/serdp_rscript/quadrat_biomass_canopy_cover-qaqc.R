## Script for doing QA/QC on combination of 1m quadrat, ####
## 25cm biomass, and canopy cover for installation delivery

#+ load packages ####
library(plyr); library(dplyr);
library(readr)
library(stringi)
library(ggplot2)
library(tidyverse)

#+ load plot visit data ####
plot_visit_data <- read_csv("data/processed_data/plot_visit_data.csv")

#+ load 1m quadrat data ####
quadrat_data <- read_csv("data/processed_data/quadrat1m.csv")

#+ load 25cm biomass data ####
biomass_data <- read_csv("data/processed_data/quadrat25cm.csv")

#+ load canopy cover data ####
canopy_cover <- read_csv("data/processed_data/canopy-cover.csv")

#### Grouping for 1m Quadrat data ####

summary(quadrat_data)

quadrat_grouped <- quadrat_data %>%
  filter(quadrat_id %in% c("n10","e10","s10","w10")) %>%
  group_by(installation, plot_id, date, visit_year) %>%
  summarise(avg_woody_veg_ht = mean(c(woody_veg_ht1, woody_veg_ht2, woody_veg_ht3), na.rm = T),
            avg_herb_veg_ht = mean(c(herb_veg_ht1, herb_veg_ht2, herb_veg_ht3), na.rm = T),
            avg_litter_ht = mean(c(litter_ht1, litter_ht2, litter_ht3), na.rm = T),
            avg_pct_green = mean(pct_green, na.rm = T),
            avg_pct_litter = mean(pct_litter, na.rm = T),
            avg_pct_wood_litter = mean(pct_wood_litter, na.rm = T),
            avg_pct_bare = mean(pct_bare, na.rm = T))

#### Grouping for 25cm biomass data ####

summary(biomass_data)

biomass_grouped <- biomass_data %>%
  group_by(installation, plot_id, date, visit_year) %>%
  summarise(avg_standing_fuel_mass_wet_m2 = mean(standing_fuel_mass_wet, na.rm = T)*16,
            avg_litter_mass_wet_m2 = mean(litter_mass_wet, na.rm = T)*16,
            avg_standing_fuel_mass_dry_m2 = mean(standing_fuel_mass_dry, na.rm = T)*16,
            avg_litter_mass_dry_m2 = mean(litter_mass_dry, na.rm = T)*16)

#### Grouping for canopy cover ####

canopy_grouped <- canopy_cover %>%
  group_by(installation, plot_id, date, visit_year) %>%
  summarise(avg_pct_canopy_cover = mean(pct_canopy_cover, na.rm = T))

#### Joining grouped data frames ####

summary(quadrat_grouped)
summary(biomass_grouped)
summary(canopy_grouped)

quadrat_biomass <-
  left_join(quadrat_grouped, biomass_grouped, by = NULL, copy = FALSE)

summary(quadrat_biomass)

anti_join(canopy_grouped, quadrat_biomass, by = NULL) %>%
  select(plot_id, date)  # Fixed date issues

quadrat_biomass_canopy_cover <-
  left_join(quadrat_biomass, canopy_grouped, by = NULL, copy = FALSE)

summary(quadrat_biomass_canopy_cover)
names(quadrat_biomass_canopy_cover)

unique(quadrat_biomass_canopy_cover$plot_id)


# Filter by installation ####

#### Filter for Camp Blanding ####

quadrat_biomass_canopy_blanding <- quadrat_biomass_canopy_cover %>%
  filter(installation=="blanding")

quadrat_biomass_canopy_blanding <- filter(quadrat_biomass_canopy_blanding) %>%
  ungroup(.) %>%
  select(plot_id, date, visit_year, avg_woody_veg_ht, avg_herb_veg_ht,
         avg_litter_ht, avg_pct_green, avg_pct_litter, avg_pct_wood_litter,
         avg_pct_bare, avg_standing_fuel_mass_wet_m2, avg_litter_mass_wet_m2,
         avg_standing_fuel_mass_dry_m2, avg_litter_mass_dry_m2,
         avg_pct_canopy_cover)

summary(quadrat_biomass_canopy_blanding)

write_csv(quadrat_biomass_canopy_blanding,
          "data/processed_by_installation/camp_blanding/quadrat_biomass_canopy_blanding.csv")

#### Filter for Avon Park AFR ####

quadrat_biomass_canopy_avonpark <- quadrat_biomass_canopy_cover %>%
  filter(installation=="avonpark")

quadrat_biomass_canopy_avonpark <- filter(quadrat_biomass_canopy_avonpark) %>%
  ungroup(.) %>%
  select(plot_id, date, visit_year, avg_woody_veg_ht, avg_herb_veg_ht,
         avg_litter_ht, avg_pct_green, avg_pct_litter, avg_pct_wood_litter,
         avg_pct_bare, avg_standing_fuel_mass_wet_m2, avg_litter_mass_wet_m2,
         avg_standing_fuel_mass_dry_m2, avg_litter_mass_dry_m2,
         avg_pct_canopy_cover)

summary(quadrat_biomass_canopy_avonpark)

write_csv(quadrat_biomass_canopy_avonpark,
          "data/processed_by_installation/avon_park_afr/quadrat_biomass_canopy_avonpark.csv")

#### Filter for Eglin AFB ####

quadrat_biomass_canopy_eglin <- quadrat_biomass_canopy_cover %>%
  filter(installation=="eglin")

quadrat_biomass_canopy_eglin <- filter(quadrat_biomass_canopy_eglin) %>%
  ungroup(.) %>%
  select(plot_id, date, visit_year, avg_woody_veg_ht, avg_herb_veg_ht,
         avg_litter_ht, avg_pct_green, avg_pct_litter, avg_pct_wood_litter,
         avg_pct_bare, avg_standing_fuel_mass_wet_m2, avg_litter_mass_wet_m2,
         avg_standing_fuel_mass_dry_m2, avg_litter_mass_dry_m2,
         avg_pct_canopy_cover)

summary(quadrat_biomass_canopy_eglin)

write_csv(quadrat_biomass_canopy_eglin,
          "data/processed_by_installation/eglin_afb/quadrat_biomass_canopy_eglin.csv")

#### Filter for Tyndall AFB ####

quadrat_biomass_canopy_tyndall <- quadrat_biomass_canopy_cover %>%
  filter(installation=="tyndall")

quadrat_biomass_canopy_tyndall <- filter(quadrat_biomass_canopy_tyndall) %>%
  ungroup(.) %>%
  select(plot_id, date, visit_year, avg_woody_veg_ht, avg_herb_veg_ht,
         avg_litter_ht, avg_pct_green, avg_pct_litter, avg_pct_wood_litter,
         avg_pct_bare, avg_standing_fuel_mass_wet_m2, avg_litter_mass_wet_m2,
         avg_standing_fuel_mass_dry_m2, avg_litter_mass_dry_m2,
         avg_pct_canopy_cover)

summary(quadrat_biomass_canopy_tyndall)

write_csv(quadrat_biomass_canopy_tyndall,
          "data/processed_by_installation/tyndall_afb/quadrat_biomass_canopy_tyndall.csv")

#### Filter for Fort Jackson ####

quadrat_biomass_canopy_jackson <- quadrat_biomass_canopy_cover %>%
  filter(installation=="jackson")

quadrat_biomass_canopy_jackson <- filter(quadrat_biomass_canopy_jackson) %>%
  ungroup(.) %>%
  select(plot_id, date, visit_year, avg_woody_veg_ht, avg_herb_veg_ht,
         avg_litter_ht, avg_pct_green, avg_pct_litter, avg_pct_wood_litter,
         avg_pct_bare, avg_standing_fuel_mass_wet_m2, avg_litter_mass_wet_m2,
         avg_standing_fuel_mass_dry_m2, avg_litter_mass_dry_m2,
         avg_pct_canopy_cover)

summary(quadrat_biomass_canopy_jackson)

write_csv(quadrat_biomass_canopy_jackson,
          "data/processed_by_installation/fort_jackson/quadrat_biomass_canopy_jackson.csv")

#### Filter for Fort Benning ####

quadrat_biomass_canopy_benning <- quadrat_biomass_canopy_cover %>%
  filter(installation=="benning")

quadrat_biomass_canopy_benning <- filter(quadrat_biomass_canopy_benning) %>%
  ungroup(.) %>%
  select(plot_id, date, visit_year, avg_woody_veg_ht, avg_herb_veg_ht,
         avg_litter_ht, avg_pct_green, avg_pct_litter, avg_pct_wood_litter,
         avg_pct_bare, avg_standing_fuel_mass_wet_m2, avg_litter_mass_wet_m2,
         avg_standing_fuel_mass_dry_m2, avg_litter_mass_dry_m2,
         avg_pct_canopy_cover)

summary(quadrat_biomass_canopy_benning)

write_csv(quadrat_biomass_canopy_benning,
          "data/processed_by_installation/fort_benning/quadrat_biomass_canopy_benning.csv")

#### Filter for Camp Shelby ####

quadrat_biomass_canopy_shelby <- quadrat_biomass_canopy_cover %>%
  filter(installation=="shelby")

quadrat_biomass_canopy_shelby <- filter(quadrat_biomass_canopy_shelby) %>%
  ungroup(.) %>%
  select(plot_id, date, visit_year, avg_woody_veg_ht, avg_herb_veg_ht,
         avg_litter_ht, avg_pct_green, avg_pct_litter, avg_pct_wood_litter,
         avg_pct_bare, avg_standing_fuel_mass_wet_m2, avg_litter_mass_wet_m2,
         avg_standing_fuel_mass_dry_m2, avg_litter_mass_dry_m2,
         avg_pct_canopy_cover)

summary(quadrat_biomass_canopy_shelby)

write_csv(quadrat_biomass_canopy_shelby,
          "data/processed_by_installation/camp_shelby/quadrat_biomass_canopy_shelby.csv")
#### Filter for Camp Blanding ####

quadrat_biomass_canopy_gordon <- quadrat_biomass_canopy_cover %>%
  filter(installation=="gordon")

quadrat_biomass_canopy_gordon <- filter(quadrat_biomass_canopy_gordon) %>%
  ungroup(.) %>%
  select(plot_id, date, visit_year, avg_woody_veg_ht, avg_herb_veg_ht,
         avg_litter_ht, avg_pct_green, avg_pct_litter, avg_pct_wood_litter,
         avg_pct_bare, avg_standing_fuel_mass_wet_m2, avg_litter_mass_wet_m2,
         avg_standing_fuel_mass_dry_m2, avg_litter_mass_dry_m2,
         avg_pct_canopy_cover)

summary(quadrat_biomass_canopy_gordon)

write_csv(quadrat_biomass_canopy_gordon,
          "data/processed_by_installation/fort_gordon/quadrat_biomass_canopy_gordon.csv")

#### Filter for Moody AFB ####

quadrat_biomass_canopy_moody <- quadrat_biomass_canopy_cover %>%
  filter(installation=="moody")

quadrat_biomass_canopy_moody <- filter(quadrat_biomass_canopy_moody) %>%
  ungroup(.) %>%
  select(plot_id, date, visit_year, avg_woody_veg_ht, avg_herb_veg_ht,
         avg_litter_ht, avg_pct_green, avg_pct_litter, avg_pct_wood_litter,
         avg_pct_bare, avg_standing_fuel_mass_wet_m2, avg_litter_mass_wet_m2,
         avg_standing_fuel_mass_dry_m2, avg_litter_mass_dry_m2,
         avg_pct_canopy_cover)

summary(quadrat_biomass_canopy_moody)

write_csv(quadrat_biomass_canopy_moody,
          "data/processed_by_installation/moody_afb/quadrat_biomass_canopy_moody.csv")
