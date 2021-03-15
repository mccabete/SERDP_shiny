# Separate dung data for each installation ####

## CHECK: depends on dung-data-qaqc.R having been processed

library(plyr); library(dplyr);
library(readr)

#### Steven begin processing by installation ####

dung_data <- read_csv("data/processed_data/dung.csv")
summary(dung_data)
names(dung_data)

dung_grouped <- dung_data %>%
      group_by(installation, plot_id, date, visit_year, species) %>%
      summarise(dung1m = sum(dung1m, na.rm = T), dung2m = sum(dung2m, na.rm = T))

summary(dung_grouped)

#### Filter for Camp Blanding ####

dung_blanding <- dung_grouped %>%
      filter(installation=="blanding", !is.na(species))

blanding_plots <- plot_visit_data %>%
      filter(installation=="blanding") %>%
      select(installation, plot_id, visit_year, date=visit_date)

dung_blanding <- left_join(blanding_plots, dung_blanding)

dung_blanding <- dung_blanding %>%
      mutate(dung1m = ifelse(is.na(dung1m)==TRUE, 0, dung1m),
             dung2m = ifelse(is.na(dung2m)==TRUE, 0, dung2m)) %>%
      select(plot_id, date, visit_year, species, dung1m, dung2m)

write_csv(dung_blanding, "data/processed_by_installation/camp_blanding/dung_blanding.csv")

#### Filter for Avon Park ####

dung_avonpark <- dung_grouped %>%
      filter(installation=="avonpark", !is.na(species))

avonpark_plots <- plot_visit_data %>%
      filter(installation=="avonpark") %>%
      select(installation, plot_id, visit_year, date=visit_date)

dung_avonpark <- left_join(avonpark_plots, dung_avonpark)

dung_avonpark <- dung_avonpark %>%
      mutate(dung1m = ifelse(is.na(dung1m)==TRUE, 0, dung1m),
             dung2m = ifelse(is.na(dung2m)==TRUE, 0, dung2m)) %>%
      select(plot_id, date, visit_year, species, dung1m, dung2m)

write_csv(dung_avonpark, "data/processed_by_installation/avon_park_afr/dung_avonpark.csv")

#### Filter for Eglin AFB ####

dung_eglin <- dung_grouped %>%
      filter(installation=="eglin", !is.na(species))

eglin_plots <- plot_visit_data %>%
      filter(installation=="eglin") %>%
      select(installation, plot_id, visit_year, date=visit_date)

dung_eglin <- left_join(eglin_plots, dung_eglin)

dung_eglin <- dung_eglin %>%
      mutate(dung1m = ifelse(is.na(dung1m)==TRUE, 0, dung1m),
             dung2m = ifelse(is.na(dung2m)==TRUE, 0, dung2m)) %>%
      select(plot_id, date, visit_year, species, dung1m, dung2m)

write_csv(dung_eglin, "data/processed_by_installation/eglin_afb/dung_eglin.csv")

#### Filter for Tyndall AFB ####

dung_tyndall <- dung_grouped %>%
      filter(installation=="tyndall", !is.na(species))

tyndall_plots <- plot_visit_data %>%
      filter(installation=="tyndall") %>%
      select(installation, plot_id, visit_year, date=visit_date)

dung_tyndall <- left_join(tyndall_plots, dung_tyndall)

dung_tyndall <- dung_tyndall %>%
      mutate(dung1m = ifelse(is.na(dung1m)==TRUE, 0, dung1m),
             dung2m = ifelse(is.na(dung2m)==TRUE, 0, dung2m)) %>%
      select(plot_id, date, visit_year, species, dung1m, dung2m)

write_csv(dung_tyndall, "data/processed_by_installation/tyndall_afb/dung_tyndall.csv")


#### Filter for Fort Jackson ####

dung_jackson <- dung_grouped %>%
      filter(installation=="jackson", !is.na(species))

jackson_plots <- plot_visit_data %>%
      filter(installation=="jackson") %>%
      select(installation, plot_id, visit_year, date=visit_date)

dung_jackson <- left_join(jackson_plots, dung_jackson)

dung_jackson <- dung_jackson %>%
      mutate(dung1m = ifelse(is.na(dung1m)==TRUE, 0, dung1m),
             dung2m = ifelse(is.na(dung2m)==TRUE, 0, dung2m)) %>%
      select(plot_id, date, visit_year, species, dung1m, dung2m)

write_csv(dung_jackson, "data/processed_by_installation/fort_jackson/dung_jackson.csv")

#### Filter for Fort Benning ####

dung_benning <- dung_grouped %>%
      filter(installation=="benning", !is.na(species))

benning_plots <- plot_visit_data %>%
      filter(installation=="benning") %>%
      select(installation, plot_id, visit_year, date=visit_date)

dung_benning <- left_join(benning_plots, dung_benning)

dung_benning <- dung_benning %>%
      mutate(dung1m = ifelse(is.na(dung1m)==TRUE, 0, dung1m),
             dung2m = ifelse(is.na(dung2m)==TRUE, 0, dung2m)) %>%
      select(plot_id, date, visit_year, species, dung1m, dung2m)

write_csv(dung_benning, "data/processed_by_installation/fort_benning/dung_benning.csv")

#### Filter for Camp Shelby ####

dung_shelby <- dung_grouped %>%
      filter(installation=="shelby", !is.na(species))

shelby_plots <- plot_visit_data %>%
      filter(installation=="shelby") %>%
      select(installation, plot_id, visit_year, date=visit_date)

dung_shelby <- left_join(shelby_plots, dung_shelby)

dung_shelby <- dung_shelby %>%
      mutate(dung1m = ifelse(is.na(dung1m)==TRUE, 0, dung1m),
             dung2m = ifelse(is.na(dung2m)==TRUE, 0, dung2m)) %>%
      select(plot_id, date, visit_year, species, dung1m, dung2m)

write_csv(dung_shelby, "data/processed_by_installation/camp_shelby/dung_shelby.csv")

#### Filter for Fort Gordon ####

dung_gordon <- dung_grouped %>%
      filter(installation=="gordon", !is.na(species))

gordon_plots <- plot_visit_data %>%
      filter(installation=="gordon") %>%
      select(installation, plot_id, visit_year, date=visit_date)

dung_gordon <- left_join(gordon_plots, dung_gordon)

dung_gordon <- dung_gordon %>%
      mutate(dung1m = ifelse(is.na(dung1m)==TRUE, 0, dung1m),
             dung2m = ifelse(is.na(dung2m)==TRUE, 0, dung2m)) %>%
      select(plot_id, date, visit_year, species, dung1m, dung2m)

write_csv(dung_gordon, "data/processed_by_installation/fort_gordon/dung_gordon.csv")

#### Filter for Moody AFB ####

dung_moody <- dung_grouped %>%
      filter(installation=="moody", !is.na(species))

moody_plots <- plot_visit_data %>%
      filter(installation=="moody") %>%
      select(installation, plot_id, visit_year, date=visit_date)

dung_moody <- left_join(moody_plots, dung_moody)

dung_moody <- dung_moody %>%
      mutate(dung1m = ifelse(is.na(dung1m)==TRUE, 0, dung1m),
             dung2m = ifelse(is.na(dung2m)==TRUE, 0, dung2m)) %>%
      select(plot_id, date, visit_year, species, dung1m, dung2m)

write_csv(dung_moody, "data/processed_by_installation/moody_afb/dung_moody.csv")

#### Steven stopped processing to installation, ask about random NAs as in ticks ####
# ^ random  NAs fixed
