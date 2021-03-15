#' # Script for doing QA/QC on the 25cm quadrat data (biomass estimates)
#'
#+ load packages ####
library(plyr); library(dplyr);
library(readr)
library(stringi)
library(ggplot2)

#+ load processed plot visit data ####
plot_visit_data <- read_csv("data/processed_data/plot_visit_data.csv")

#+ 25cm biomass data ####
biomass_data <- read_csv("data/raw_data/quadrat25cm.csv")
biomass_data
summary(biomass_data)
biomass_data <- biomass_data %>%
      mutate(plot_id = paste(installation, plot_id, sep = " "))
## NA's for date
filter(biomass_data, is.na(date))$plot_id
filter(biomass_data, plot_id == "blanding c1")$date
## missing the date of the 2018 visit to Blanding C1
biomass_data$date[is.na(biomass_data$date)] <- 20180516
## Exclude pilot plot sampling from data
biomass_data <- biomass_data %>% filter(date > 20170601)
unique(biomass_data$plot_id)
biomass_data$plot_id[biomass_data$plot_id=="blanding cogan"] <- "blanding theater_cogon"

summary(biomass_data)
## 7 NAs for dry standing fuel mass
filter(biomass_data, is.na(standing_fuel_mass_dry))$standing_fuel_mass_wet
filter(biomass_data, is.na(standing_fuel_mass_dry)) %>%
      select(date, plot_id, transect_id, standing_fuel_mass_wet)
filter(biomass_data, is.na(pct_litter)| is.na(pct_bare)) %>%
  select(plot_id, transect_id, pct_litter,pct_bare, pct_green, litter_mass_dry, standing_fuel_mass_dry)

## Avon Park A1 Cogon2 & E2 Cogon3 true NA for pct_litter, pct_bare, pct_green ##

biomass_data <- filter(biomass_data, transect_id %in% c("east","west","north","south"))

## Filtered out cogon data from 25cm quadrat biomass data ##

filter(biomass_data, is.na(pct_fuel)| is.na(pct_bare) | is.na(pct_green)) %>%
  select(plot_id, transect_id, pct_litter,pct_bare, pct_green, litter_mass_dry, standing_fuel_mass_dry)

biomass_data$pct_litter[biomass_data$plot_id=="avonpark c2" &
                          biomass_data$transect_id=="south"] <- 80
biomass_data$pct_green[biomass_data$plot_id=="avonpark c2" &
                          biomass_data$transect_id=="south"] <- 55

biomass_data$pct_bare[biomass_data$plot_id=="avonpark f1" &
                        biomass_data$transect_id=="west"] <- 75

filter(biomass_data, is.na(pct_fuel)| is.na(pct_bare) | is.na(pct_green) | is.na(pct_litter) |
         is.na(standing_fuel_mass_dry))%>%
  select(date, plot_id, transect_id, pct_fuel, pct_litter,pct_bare, pct_green, standing_fuel_mass_dry)

biomass_data$pct_fuel[biomass_data$plot_id=="jackson h1" &
                        biomass_data$transect_id=="north"] <- 60

biomass_data$pct_bare[biomass_data$plot_id=="jackson h1" &
                        biomass_data$transect_id=="north"] <- 0

biomass_data$pct_litter[biomass_data$plot_id=="jackson h1" &
                        biomass_data$transect_id=="north"] <- 100

biomass_data$pct_green[biomass_data$plot_id=="jackson h1" &
                        biomass_data$transect_id=="north"] <- 80

names(biomass_data)

## Shelby B1 in 2017 is probably missing because cogongrass in MS
## Not sure why the others are missing
filter(biomass_data, plot_id=="blanding b1", transect_id=="south")
biomass_data$standing_fuel_mass_dry[biomass_data$plot_id=="blanding b1" &
biomass_data$transect_id=="south"] <- 0

## Reading in cogon data for litter biomass ##

cogond <- read_csv("data/raw_data/cogon-data.csv")
filter(cogond, site=="avonpark", Quadrant=="cogon 3") %>%
  select(site, date, plot_id, Quadrant, CogonSampleID, fresh_biomass, dry_biomass)

filter(cogond, site=="shelby") %>%
  select(site, date, plot_id, Quadrant, CogonSampleID, fresh_biomass, dry_biomass)

biomass_data$standing_fuel_mass_dry[is.na(biomass_data$standing_fuel_mass_dry) &
                                      biomass_data$standing_fuel_mass_wet==0] <- 0

biomass_data$date[biomass_data$plot_id=="blanding c1" & biomass_data$date==20170608] <- 20170609

filter(biomass_data, plot_id=="blanding c1") %>%
  select(plot_id, date)

biomass_data$date <- as.Date(as.character(biomass_data$date), format = "%Y%m%d")
biomass_data <- biomass_data %>%
  mutate(visit_year = lubridate::year(biomass_data$date),
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
summary(biomass_data)

unique(biomass_data$plot_id)

write_csv(biomass_data, "data/processed_data/quadrat25cm.csv")


# biomass_data <- read_csv("data/processed_data/quadrat25cm.csv")
summary(biomass_data)
names(biomass_data)

biomass_grouped <- biomass_data %>%
  group_by(installation, plot_id, date, visit_year) %>%
  summarise(avg_standing_fuel_mass_wet_m2 = mean(standing_fuel_mass_wet, na.rm = T)*16,
            avg_litter_mass_wet_m2 = mean(litter_mass_wet, na.rm = T)*16,
            avg_standing_fuel_mass_dry_m2 = mean(standing_fuel_mass_dry, na.rm = T)*16,
            avg_litter_mass_dry_m2 = mean(litter_mass_dry, na.rm = T)*16)

summary(biomass_grouped)

## Done grouping, combination of canopy, quad1m and quad25cm on new script: quadrat_biomass_canopy_cover-qaqc.R ####

# biomass_data$fuel_mass_wet <- round(as.numeric(biomass_data$fuel_mass_wet),2)
# biomass_data <- left_join(
#       select(biomass_data, installation,plot_id,fuel_mass_wet,litter_mass_wet),
#       select(plot_data, installation:full_names),
#       by = c("installation","plot_id")
# )
# # biomass_data$last_fire_year <- as.integer(biomass_data$last_fire_year)
# # biomass_data$years_since_fire <- 2017 - biomass_data$last_fire_year
#
# ggplot(biomass_data,
#        aes(years_since_fire, fuel_mass_wet*16)) +
#       geom_point(aes(color = installation), position = "jitter") +
#       # facet_grid(.~installation) +
#       # geom_smooth(method = "loess")
#       theme_bw() +
#       ylab(expression(paste("Standing biomass (g/", m^{2}, ")"))) +
#       xlab("Years since last fire") +
#       ggtitle("Fresh standing biomass across burn units at each installation",
#               subtitle = "(measurements at 25cm quadrats)")
# # ggsave("~/Dropbox (UF)/SERDP-Project/figures/standing-biomass-hitter.png", height=7)
#
# ggplot(biomass_data %>% group_by(installation, last_fire_year),
#        aes(years_since_fire, litter_mass_wet*16)) +
#       geom_point(aes(color = installation), position = "jitter") +
#       # facet_grid(.~installation) +
#       theme_bw() +
#       ylab(expression(paste("Litter biomass (g/", m^{2}, ")"))) +
#       xlab("Years since last fire") +
#       ggtitle("Fresh litter biomass across burn units at each installation",
#               subtitle = "(measurements at 25cm quadrats)")
# # ggsave("~/Dropbox (UF)/SERDP-Project/figures/litter-biomass-jitter.png", height=7)
