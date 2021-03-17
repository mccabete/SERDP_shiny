#' # Script for doing QA/QC on cogongrass data

#+ load packages ####
library(plyr); library(dplyr);
library(readr)
library(stringi)
library(ggplot2)
library(tidyverse)

#' ## Cogongrass Data Sampled from Invasions
#'
#+ cogongrass data ####
cogon_data <- read_csv("data/raw_data/cogon-data.csv")

cogon_data$plot_id[cogon_data$plot_id=="theatercogon"] <- "theater_cogon"

cogon_data <- cogon_data %>%
  mutate(plot_id = paste(site, plot_id, sep = " "),
         plot_id=tolower(plot_id)) %>%
  filter(date>20170601)

cogon_data$date <- as.Date(as.character(cogon_data$date), format = "%Y%m%d")
cogon_data <- cogon_data %>%
  mutate(visit_year = lubridate::year(cogon_data$date),
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

sort(unique(cogon_data$plot_id))

summary(cogon_data)

filter(cogon_data, is.na(dry_biomass)) %>%
  select(date, plot_id, Quadrant, CogonSampleID, dry_biomass)

#### Missing dry biomass from Shelby due to transportation regulations ####
#### Other missing biomass measurements (blanding theater and shelby 2018) due to mixed cogon biomass ####

write_csv(cogon_data, "data/processed_data/cogongrass.csv")

#### Steven stopped processing ####

cogon_data <- read_csv("data/processed_data/cogongrass.csv")
plot_visit_data <- read_csv("data/processed_data/plot_visit_data.csv")

summary(cogon_data)
summary(plot_visit_data)
unique(plot_visit_data$imcy_inv)

cogon_grouped <- cogon_data %>%
  group_by(site, plot_id, date, visit_year) %>%
  summarise(avg_tiller_density_m2 = mean(tiller_density), na.rm = T)

plot_visit_cogon <-
  left_join(plot_visit_data, cogon_grouped)

plot_visit_cogon <- plot_visit_cogon %>%
  ungroup(.) %>%
  select(installation, plot_id, visit_year, imcy_inv, avg_tiller_density_m2,
         xcoord_lon, ycoord_lat, years_since_fire)

#### Cogon for Blanding ####

plot_visit_cogongrass_blanding <- plot_visit_cogon %>%
  filter(installation=="blanding") %>%
  select(plot_id, visit_year, imcy_inv, avg_tiller_density_m2, xcoord_lon, ycoord_lat, years_since_fire)

write_csv(plot_visit_cogongrass_blanding, "data/processed_by_installation/camp_blanding/plot_visit_invasion_status_blanding.csv")

#### Cogon for Avon Park ####

plot_visit_cogongrass_avonpark <- plot_visit_cogon %>%
  filter(installation=="avonpark") %>%
  select(plot_id, visit_year, imcy_inv, avg_tiller_density_m2, xcoord_lon, ycoord_lat, years_since_fire)

write_csv(plot_visit_cogongrass_avonpark, "data/processed_by_installation/avon_park_afr/plot_visit_invasion_status_avonpark.csv")

#### Cogon for Eglin ####

plot_visit_cogongrass_eglin <- plot_visit_cogon %>%
  filter(installation=="eglin") %>%
  select(plot_id, visit_year, imcy_inv, avg_tiller_density_m2, xcoord_lon, ycoord_lat, years_since_fire)

write_csv(plot_visit_cogongrass_eglin, "data/processed_by_installation/eglin_afb/plot_visit_invasion_status_eglin.csv")

#### Cogon for Tyndall ####

plot_visit_cogongrass_tyndall <- plot_visit_cogon %>%
  filter(installation=="tyndall") %>%
  select(plot_id, visit_year, imcy_inv, avg_tiller_density_m2, xcoord_lon, ycoord_lat, years_since_fire)

write_csv(plot_visit_cogongrass_tyndall, "data/processed_by_installation/tyndall_afb/plot_visit_invasion_status_tyndall.csv")

#### Cogon for Jackson ####

plot_visit_cogongrass_jackson <- plot_visit_cogon %>%
  filter(installation=="jackson") %>%
  select(plot_id, visit_year, imcy_inv, avg_tiller_density_m2, xcoord_lon, ycoord_lat, years_since_fire)

write_csv(plot_visit_cogongrass_jackson, "data/processed_by_installation/fort_jackson/plot_visit_invasion_status_jackson.csv")

#### Cogon for Benning ####

plot_visit_cogongrass_benning <- plot_visit_cogon %>%
  filter(installation=="benning") %>%
  select(plot_id, visit_year, imcy_inv, avg_tiller_density_m2, xcoord_lon, ycoord_lat, years_since_fire)

write_csv(plot_visit_cogongrass_benning, "data/processed_by_installation/fort_benning/plot_visit_invasion_status_benning.csv")

#### Cogon for Shelby ####

plot_visit_cogongrass_shelby <- plot_visit_cogon %>%
  filter(installation=="shelby") %>%
  select(plot_id, visit_year, imcy_inv, avg_tiller_density_m2, xcoord_lon, ycoord_lat, years_since_fire)

write_csv(plot_visit_cogongrass_shelby, "data/processed_by_installation/camp_shelby/plot_visit_invasion_status_shelby.csv")

#### Cogon for Gordon ####

plot_visit_cogongrass_gordon <- plot_visit_cogon %>%
  filter(installation=="gordon") %>%
  select(plot_id, visit_year, imcy_inv, avg_tiller_density_m2, xcoord_lon, ycoord_lat, years_since_fire)

write_csv(plot_visit_cogongrass_gordon, "data/processed_by_installation/fort_gordon/plot_visit_invasion_status_gordon.csv")

#### Cogon for Moody ####

plot_visit_cogongrass_moody <- plot_visit_cogon %>%
  filter(installation=="moody") %>%
  select(plot_id, visit_year, imcy_inv, avg_tiller_density_m2, xcoord_lon, ycoord_lat, years_since_fire)

write_csv(plot_visit_cogongrass_moody, "data/processed_by_installation/moody_afb/plot_visit_invasion_status_moody.csv")

#### Steven checked processing, looks good ####

###################### Whalen's processing #####
# cogon_biomass <- cogon_data %>%
#       # select(site,plot_id,Quad,fresh_biomass) %>%
#       filter(site!="NA") %>%
#       mutate(biomass=fresh_biomass*16)
# cogon_biomass <- left_join(
#       cogon_biomass,
#       select(plot_data, installation,plot_id,years_since_fire),
#       by=c("site"="installation","plot_id"))
# ggplot(cogon_biomass, aes(years_since_fire, biomass)) +
#       geom_point() +
#       theme_bw() +
#       xlab("Years since fire") +
#       ylab(expression(paste("Standing biomass (g/", m^{2}, ")"))) +
#       ggtitle("Cogongrass invasion subsample")
# ggsave("~/Dropbox (UF)/SERDP-Project/figures/cogon-biomass.png", height = 7)
