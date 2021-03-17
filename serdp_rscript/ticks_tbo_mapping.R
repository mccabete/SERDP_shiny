# Map showing variation in the metrics of tick-borne disease risk
# (tick abundance and infection prevalence) among installations.


# Data Description for Preliminary Map of Tick-Borne Disease Risk
# SERDP RC 2636
# 10/20/2020
# Prepared by Whalen Dillon, whalendillon@gmail.com
# 
# File name: map_tickAbun_strictlyHuman_pathPrev.html
# 
# Ticks were primarily collected using a CO2 baited tick-trapping method. While we also collected ticks encountered in the area while making other measurements, i.e. off of personnel, the data underlying this map is only from ticks directly from the tick trapping. This enables a more straightforward estimation of abundance as the number of ticks per trap. The estimate of pathogen prevalence is based strictly on the pathogens positively known to cause human disease, so prevalence is the proportion of ticks that carrying at least one of these pathogens. Finally, we multiplied the abundance value by prevalence value to get the number of ticks per trap carrying at least one human pathogen. The selected data results in a conservative estimate of tick-borne disease exposure risk to humans at this stage of analysis. The map has a layer for each of these values summarized to the installation level. 
# 
# •	Tick abundance (A) = total number of ticks / total number of traps 
# •	Human pathogen prevalence (P) = ticks carrying human pathogens / total number of ticks
# •	P x A = number of ticks carrying at least one human pathogen
# 
# If viewed while connected to the internet the map file should render background tiles of the U.S.A., otherwise the dots will be floating on a white background. Each of the data layers should be viewed independently, so the user will want to toggle off the two that are not of immediate interest.


library(tidyverse)
library(sf)
#library(tmap)
#library(tmaptools)

## Load Data ####

## From tbo_data.R
tbo_plot_agg <- read_csv("serdp_data/tbo_ticks_plot_year.csv")
tbo_trap_ticks <- read_csv("serdp_data/tbo_ticks_onTraps.csv")
tbo_human_strict_traps <- read_csv("serdp_data/tbo_human_strict_onTraps.csv")

## From plot_data_report.Rmd
trap_effort <- read_csv("serdp_data/installation_trapping_effort.csv")

sites_sf <- st_read("serdp_data/gis_files/selected_installations.shp")

unique(tbo_plot_agg$Installation)
unique(sites_sf$FULLNAME)

## Munge data ####
## Rename installations to match across data
trap_effort$installation <- str_to_title(trap_effort$installation)
trap_effort <- trap_effort %>%
      rename(Installation = installation) %>%
      mutate(
            Installation = case_when(
                  Installation=="Avonpark" ~ "Avon Park AFR",
                  Installation=="Benning" ~ "Ft. Benning",
                  Installation=="Blanding" ~ "Camp Blanding",
                  Installation=="Eglin" ~ "Eglin AFB",
                  Installation=="Gordon" ~ "Ft. Gordon",
                  Installation=="Jackson" ~ "Ft. Jackson",
                  Installation=="Moody" ~ "Moody AFB",
                  Installation=="Shelby" ~ "Camp Shelby",
                  Installation=="Tyndall" ~ "Tyndall AFB"
            )
      )

tbo_plot_agg <- tbo_plot_agg %>%
      mutate(
            Installation = case_when(
                  Installation=="Avon" ~ "Avon Park AFR",
                  Installation=="Benning" ~ "Ft. Benning",
                  Installation=="Blanding" ~ "Camp Blanding",
                  Installation=="Eglin" ~ "Eglin AFB",
                  Installation=="Gordon" ~ "Ft. Gordon",
                  Installation=="Jackson" ~ "Ft. Jackson",
                  Installation=="Moody" ~ "Moody AFB",
                  Installation=="Shelby" ~ "Camp Shelby",
                  Installation=="Tyndall" ~ "Tyndall AFB"
            )
      )

tbo_trap_ticks <- tbo_trap_ticks %>%
      mutate(
            Installation = case_when(
                  Installation=="Avon" ~ "Avon Park AFR",
                  Installation=="Benning" ~ "Ft. Benning",
                  Installation=="Blanding" ~ "Camp Blanding",
                  Installation=="Eglin" ~ "Eglin AFB",
                  Installation=="Gordon" ~ "Ft. Gordon",
                  Installation=="Jackson" ~ "Ft. Jackson",
                  Installation=="Moody" ~ "Moody AFB",
                  Installation=="Shelby" ~ "Camp Shelby",
                  Installation=="Tyndall" ~ "Tyndall AFB"
            )
      )

tbo_human_strict_traps <- tbo_human_strict_traps %>%
      mutate(
            Installation = case_when(
                  Installation=="Avon" ~ "Avon Park AFR",
                  Installation=="Benning" ~ "Ft. Benning",
                  Installation=="Blanding" ~ "Camp Blanding",
                  Installation=="Eglin" ~ "Eglin AFB",
                  Installation=="Gordon" ~ "Ft. Gordon",
                  Installation=="Jackson" ~ "Ft. Jackson",
                  Installation=="Moody" ~ "Moody AFB",
                  Installation=="Shelby" ~ "Camp Shelby",
                  Installation=="Tyndall" ~ "Tyndall AFB"
            )
      )

mapping_data <- tbo_human_strict_traps
## Use only ticks collected via trapping, i.e. excluded any drags/people

tbo_inst_N <- mapping_data %>%
      filter(human_path=="No") %>%
      group_by(Installation) %>%
      summarise(
            Tick_abundanceN = sum(Tick_abundance),
            Pathogen_abundanceN = sum(Pathogen_abundance),
            )
tbo_inst_Y <- mapping_data %>%
      filter(human_path=="Yes") %>%
      group_by(Installation) %>%
      summarise(
            Tick_abundanceY = sum(Tick_abundance),
            Pathogen_abundanceY = sum(Pathogen_abundance),
      )

tbo_inst <- left_join(tbo_inst_N, tbo_inst_Y)
tbo_inst <- tbo_inst %>%
      mutate(Tick_abundanceY = ifelse(is.na(Tick_abundanceY)==TRUE, 0,
                                      Tick_abundanceY),
             Pathogen_abundanceY = ifelse(is.na(Pathogen_abundanceY)==TRUE, 0,
                                      Pathogen_abundanceY),
             total_ticks = Tick_abundanceN + Tick_abundanceY,
             Human_pathogen_prevalence = Tick_abundanceY/total_ticks)
tbo_inst <- left_join(tbo_inst, trap_effort)
tbo_inst <- tbo_inst %>%
      mutate(ticks_per_trap = total_ticks/trap_effort,
             PxA = Human_pathogen_prevalence*ticks_per_trap
             )

sites_tbo_sf <- left_join(
      sites_sf,
      tbo_inst,
      by = c("FULLNAME"="Installation")
) %>%
      select(FULLNAME, total_ticks, Human_pathogen_prevalence, ticks_per_trap, PxA)



## Make a map ####

# toggle between interactive "view" mode and static "plot" mode
#tmap_mode("view")
# tmap_mode("plot")

# qtm(sites_sf, fill = "red")

# bm <- read_osm(st_bbox(sites_sf))# this didn't work last attempt
sites_tbo_sf <- sites_tbo_sf %>%
      mutate(Ticks = total_ticks,
             Prevalence = Human_pathogen_prevalence,
             Abundance = ticks_per_trap
             )
sites_tbo_sf$Trap_effort <- trap_effort$trap_effort
#sites_tbo_sf$geometry <- as.character(sites_tbo_sf$geometry)
#write.csv(sites_tbo_sf, "/projectnb/dietzelab/mccabete/SERDP_shiny/code/serdp_data/Tick_prevelence_absense_map.csv")
st_write(sites_tbo_sf, "/projectnb/dietzelab/mccabete/SERDP_shiny/code/serdp_data/Tick_prevelence_absense_map.shp")

## Adds layers for abundance, prevalence, and their product.
## So, need to toggle layers on/off for it to make any kind of sense.
# tp_map <- tm_shape(sites_tbo_sf) +
#       # tm_polygons(text = "FULLNAME") +
#       # # tm_dots(col = "Pathogen_prevalence", size = "Pathogen_prevalence",
#       #         group = "Pathogens") +
#       # tm_shape(sites_tbo_sf) +
#       tm_dots(col = "Abundance", size = "Abundance",
#               # breaks = c(0, 50, 100, 200, 408),
#               group = "Ticks per trap (abundance)") +
#       tm_shape(sites_tbo_sf) +
#       tm_dots(col = "Prevalence", size = "Prevalence",
#               # breaks = c(0, 50, 100, 200, 408),
#               group = "Human pathogen prevalence ") +
#       tm_shape(sites_tbo_sf) +
#       tm_dots(col = "PxA", size = "PxA",
#               # breaks = c(0, 50, 100, 200, 408),
#               group = "Prevalence x Abundance") +
#       tm_text(text = "FULLNAME", just = "right", size = 1.5, xmod = -2, bg.color = "gray40")
# tp_map
# tmap_save(tp_map, filename = "figures/map_tickAbun_strictlyHuman_pathPrev.html")
# tmap_save(tp_map, filename = "figures/map_tickAbun_pathPrev.png")

#
# library(ggmap)
# bb <- st_bbox(sites_sf)
# names(bb) <- c("left","bottom","right","top")
# bm <- get_map(bb)
# ggmap(bm) +
#       geom_point(data = sites_tbo_sf)

