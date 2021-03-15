## Combine fire return interval data

library(tidyverse)

fri_plots <- read_csv("data/AvonPark/avp_15yr_fri.csv")
fri_plots <- rbind(fri_plots,
                   read_csv("data/CampBlanding/blanding_15yr_fri.csv"))
fri_plots <- rbind(fri_plots,
                   read_csv("data/CampShelby/shelby_15yr_fri.csv"))
fri_plots <- rbind(fri_plots,
                   read_csv("data/EglinAFB/eglin_15yr_fri.csv"))
fri_plots <- rbind(fri_plots,
                   read_csv("data/FtBenning/benning_15yr_fri.csv"))
fri_plots <- rbind(fri_plots,
                   read_csv("data/FtGordon/gordon_15yr_fri.csv"))
fri_plots <- rbind(fri_plots,
                   read_csv("data/FtJackson/jackson_15yr_fri.csv"))
fri_plots <- rbind(fri_plots,
                   read_csv("data/MoodyAFB/moody_15yr_fri.csv"))
fri_plots <- rbind(fri_plots,
                   read_csv("data/Tyndall/tyndall_15yr_fri.csv"))

write_csv(fri_plots, "data/processed_data/fire_return_plots.csv")
