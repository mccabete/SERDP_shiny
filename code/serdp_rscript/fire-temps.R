#' #Fire Temperature Data
#+ load packages ####
library(plyr); library(dplyr)
library(readr); library(ggplot2)


#+ Load Bivens Test Burn Data ----------------------------------------------
ground <- read_csv("data/fire_temperatures/bivens-testburn-20170407-ground.csv")
names(ground) <- c("date","time","tempC")# rename columns
# str(ground)

twelve <- read_csv("data/fire_temperatures/bivens-testburn-20170407-12cm.csv")
names(twelve) <- c("date","time","tempC")

fifty <- read_csv("data/fire_temperatures/bivens-testburn-20170407-50cm.csv")
names(fifty) <- c("date","time","tempC")

# par(mfrow=c(3,1))
# plot(ground$time, ground$tempC)
# plot(twelve$time, twelve$tempC)
# plot(fifty$time, fifty$tempC)

#+ Slice Test Burn Temperature ####
test_burn_standing <- rbind(
      filter(ground, between(time, 37380,37525) & between(tempC, 20,1500)) %>%
            mutate(location = "0cm"),
      filter(twelve, between(time, 37380,37525) & between(tempC, 20,1500)) %>%
            mutate(location = "12cm"),
      filter(fifty, between(time, 37380,37525) & between(tempC, 20,1500)) %>%
            mutate(location = "50cm")
)
test_burn_standing$fuel_type <- "standing"
# ggplot(fire1, aes(time, tempC, col = location)) +
#       geom_line() +
#       theme_bw() +
#       ggtitle("Standing Fuel - Bivens")


test_burn_thatch <- rbind(
      filter(ground, between(time, 37630,38000)) %>%
            mutate(location = "0cm"),
      filter(twelve, between(time, 37630,38000)) %>%
            mutate(location = "12cm"),
      filter(fifty, between(time, 37630,38000)) %>%
            mutate(location = "50cm")
)
test_burn_thatch$fuel_type <- "litter"
# ggplot(fire2, aes(time, tempC, col = location)) +
#       geom_line() +
#       theme_bw() +
#       ggtitle("Litter Fuel - Bivens")


# library(lubridate)



#+ Load FABIO Prescribed Burn Data 2017/07/06 ------------------------------
fabio_blanding_0cm <- read_csv("data/fire_temperatures/fabio-bland-c-0cm-20170706.csv")
names(fabio_blanding_0cm) <- c("date","time","tempC")
fabio_blanding_0cm <- mutate(fabio_blanding_0cm, location = "0cm", probe_type = "old")
fabio_blanding_25cm <- read_csv("data/fire_temperatures/fabio-bland-c-25cm-20170706.csv")
names(fabio_blanding_25cm) <- c("date","time","tempC")
fabio_blanding_25cm <- mutate(fabio_blanding_25cm, location = "25cm", probe_type = "old")
fabio_blanding_50cm <- read_csv("data/fire_temperatures/fabio-bland-c-50cm-20170706.csv")
names(fabio_blanding_50cm) <- c("date","time","tempC")
fabio_blanding_50cm <- mutate(fabio_blanding_50cm, location = "50cm", probe_type = "old")

# par(mfrow=c(3,1))
# plot(fabio_blanding_0cm$time, fabio_blanding_0cm$tempC, main = "FABIO Prescribed Fire 7/6/2017")
# plot(fabio_blanding_25cm$time, fabio_blanding_25cm$tempC)
# plot(fabio_blanding_50cm$time, fabio_blanding_50cm$tempC)

#+ Load FABIO Burn Biomass Data ----
fabio_burns <- read_csv("data/raw_data/fires_data/fabio-burns.csv")
fabio_burns$date <- as.Date(
      as.character(fabio_burns$date), format = "%Y%m%d")

# fabio_burns_biomass <- select(fabio_burns, fire_id:notes)

fires_biomass <- fabio_burns %>%
      mutate(pct_green = factor(pct_green, levels = c("25","75")),
             f_litter_biomass = factor(litter_biomass, levels = c("0","500","1000","2000"), labels = c("0g litter","500g litter","1000g litter","2000g litter")),
             f_biomass = factor(biomass, levels = sort(unique(biomass)), labels = paste(sort(unique(biomass)), "g", sep="")),
             total_biomass = biomass + litter_biomass,
             pct_consumed = 100*(total_biomass - remaining_biomass)/total_biomass,
             est_pct_fuel_moisture = 100*(remaining_biomass - dry_remaining_biomass)/remaining_biomass,
             pct_fuel_moisture = 100*(fuel_moisture_wet - fuel_moisture_dry)/fuel_moisture_wet,
             max_flame_ht = apply(cbind(flame_ht1,flame_ht2), 1, function(x) max(x, na.rm = T)),
             avg_flame_ht = rowMeans(cbind(flame_ht1,flame_ht2), na.rm=T),
             max_fuel_ht = apply(cbind(green_ht1,green_ht2,green_ht3,brown_ht1,brown_ht2,brown_ht3), 1, function(x) max(x, na.rm=T)),
             avg_litter_depth = rowMeans(cbind(litter_depth1,litter_depth2,litter_depth3), na.rm=T),
             avg_green_ht = rowMeans(cbind(green_ht1,green_ht2,green_ht3), na.rm=T),
             avg_brown_ht = rowMeans(cbind(brown_ht1,brown_ht2,brown_ht3), na.rm=T),
             avg_fuel_ht = rowMeans(cbind(avg_green_ht,avg_brown_ht), na.rm=T)) %>%
      select(date:pct_green, biomass_type, structure, rate_of_spread_50cm:avg_fuel_ht, -notes)

write_csv(fires_biomass, "data/fabio-fires-biomass.csv")

# View(fires_biomass %>% filter(biomass_type=="wiregrass") %>%
#            arrange(litter_biomass)
#      )


ggplot(fires_biomass, aes(f_litter_biomass, factor(biomass))) +
      geom_point(aes(color = pct_green, shape = biomass_type), size = 2,
                 position = position_jitterdodge(jitter.width = 0)) +
      scale_color_discrete(h.start = 20) +
      theme_bw()


#+ Experimental Burns July 5, 2017: Fire IDs 1-8 ####

# load data from FABIO 1 sensors
fabio1_0cm <- read_csv("data/fire_temperatures/fabio1-0cm-20170705.csv")
names(fabio1_0cm) <- c("date","time","tempC")
fabio1_0cm <- mutate(fabio1_0cm, location = "0cm", probe_type = "old")
fabio1_25cm <- read_csv("data/fire_temperatures/fabio1-25cm-20170705.csv")
names(fabio1_25cm) <- c("date","time","tempC")
fabio1_25cm <- mutate(fabio1_25cm, location = "25cm", probe_type = "old")
fabio1_50cm <- read_csv("data/fire_temperatures/fabio1-50cm-20170705.csv")
names(fabio1_50cm) <- c("date","time","tempC")
fabio1_50cm <- mutate(fabio1_50cm, location = "50cm", probe_type = "old")

# load data from FABIO 2 sensors
fabio2_0cm <- read_csv("data/fire_temperatures/fabio2-0cm-20170705.csv")
names(fabio2_0cm) <- c("date","time","tempC")
fabio2_0cm <- mutate(fabio2_0cm, location = "0cm", probe_type = "old")
fabio2_25cm <- read_csv("data/fire_temperatures/fabio2-25cm-20170705.csv")
names(fabio2_25cm) <- c("date","time","tempC")
fabio2_25cm <- mutate(fabio2_25cm, location = "25cm", probe_type = "old")
fabio2_50cm <- read_csv("data/fire_temperatures/fabio2-50cm-20170705.csv")
names(fabio2_50cm) <- c("date","time","tempC")
fabio2_50cm <- mutate(fabio2_50cm, location = "50cm", probe_type = "old")

# plot all data from sensors
# par(mfrow=c(3,1))
# plot(fabio1_0cm$time, fabio1_0cm$tempC, main = "FABIO 1 Burns 7/5/2017")
# plot(fabio1_25cm$time, fabio1_25cm$tempC)
# plot(fabio1_50cm$time, fabio1_50cm$tempC)
#
# plot(fabio2_0cm$time, fabio2_0cm$tempC, main = "FABIO 2 Burns 7/5/2017")
# plot(fabio2_25cm$time, fabio2_25cm$tempC)
# plot(fabio2_50cm$time, fabio2_50cm$tempC)

#+ Slice Experimental Burn Temperatures: Fire IDs 1-8 ####
fireid1 <- rbind(
      filter(fabio1_0cm, between(time, 45330,45400)),
      filter(fabio1_25cm, between(time, 45330,45400)),
      filter(fabio1_50cm, between(time, 45330,45400))
      ) %>%
      mutate(fire_id = 1)
# fireid1 <- left_join(fireid1, select(fabio_burns_biomass, -fabio_id))

fireid2 <- rbind(
      filter(fabio2_0cm, between(time, 47500,48000)),
      filter(fabio2_25cm, between(time, 47500,48000)),
      filter(fabio2_50cm, between(time, 47500,48000))
      ) %>%
      mutate(fire_id = 2)
# fireid2 <- left_join(fireid2, select(fabio_burns_biomass, -fabio_id))

fireid3 <- rbind(
      filter(fabio2_0cm, between(time, 53820,53940)),
      filter(fabio2_25cm, between(time, 53820,53940)),
      filter(fabio2_50cm, between(time, 53820,53940))
      ) %>%
      mutate(fire_id = 3)
# fireid3 <- left_join(fireid3, select(fabio_burns_biomass, -fabio_id))

fireid4 <- rbind(
      filter(fabio1_0cm, between(time, 54930,55000)),
      filter(fabio1_25cm, between(time, 54930,55000)),
      filter(fabio1_50cm, between(time, 54930,55000))
      ) %>%
      mutate(fire_id = 4)
# fireid4 <- left_join(fireid4, select(fabio_burns_biomass, -fabio_id))

fireid5 <- rbind(
      filter(fabio2_0cm, between(time, 55900,56100)),
      filter(fabio2_25cm, between(time, 55900,56100)),
      filter(fabio2_50cm, between(time, 55900,56100))
      ) %>%
      mutate(fire_id = 5)
# fireid5 <- left_join(fireid5, select(fabio_burns_biomass, -fabio_id))

fireid6 <- rbind(
      filter(fabio1_0cm, between(time, 59600,60240)),
      filter(fabio1_25cm, between(time, 59600,60240)),
      filter(fabio1_50cm, between(time, 59600,60240))
      ) %>%
      mutate(fire_id = 6)
# fireid6 <- left_join(fireid6, select(fabio_burns_biomass, -fabio_id))

fireid7 <- rbind(
      filter(fabio_blanding_0cm, between(time, 48500,48880)),
      filter(fabio_blanding_25cm, between(time, 48500,48880)),
      filter(fabio_blanding_50cm, between(time, 48500,48880))
      ) %>%
      mutate(fire_id = 7)
# fireid7 <- left_join(fireid7, fabio_burns_biomass)

fireid8 <- rbind(
      filter(fabio_blanding_0cm, between(time, 49620,50200)),
      filter(fabio_blanding_25cm, between(time, 49620,50200)),
      filter(fabio_blanding_50cm, between(time, 49620,50200))
      ) %>%
      mutate(fire_id = 8)
# fireid8 <- left_join(fireid8, fabio_burns_biomass)


#+ Experimental Burns July 12, 2017: Fire IDs 9-17 ####
# load data from FABIO 1 sensors
fabio1_0cm_new <- read_csv("data/fire_temperatures/fabio1-0cm-new-20170712.csv")
names(fabio1_0cm_new) <- c("date","time","tempC")
fabio1_0cm_new <- mutate(fabio1_0cm_new, location = "0cm", probe_type = "new")
fabio1_0cm_old <- read_csv("data/fire_temperatures/fabio1-0cm-old-20170712.csv")
names(fabio1_0cm_old) <- c("date","time","tempC")
fabio1_0cm_old <- mutate(fabio1_0cm_old, location = "0cm", probe_type = "old")

fabio1_25cm_new <- read_csv("data/fire_temperatures/fabio1-25cm-new-20170712.csv")
names(fabio1_25cm_new) <- c("date","time","tempC")
fabio1_25cm_new <- mutate(fabio1_25cm_new, location = "25cm", probe_type = "new")
fabio1_25cm_old <- read_csv("data/fire_temperatures/fabio1-25cm-old-20170712.csv")
names(fabio1_25cm_old) <- c("date","time","tempC")
fabio1_25cm_old <- mutate(fabio1_25cm_old, location = "25cm", probe_type = "old")

fabio1_50cm_new <- read_csv("data/fire_temperatures/fabio1-50cm-new-20170712.csv")
names(fabio1_50cm_new) <- c("date","time","tempC")
fabio1_50cm_new <- mutate(fabio1_50cm_new, location = "50cm", probe_type = "new")
fabio1_50cm_old <- read_csv("data/fire_temperatures/fabio1-50cm-old-20170712.csv")
names(fabio1_50cm_old) <- c("date","time","tempC")
fabio1_50cm_old <- mutate(fabio1_50cm_old, location = "50cm", probe_type = "old")

# load data from FABIO 2 sensors
fabio2_0cm_old <- read_csv("data/fire_temperatures/fabio2-0cm-old-20170712.csv")
names(fabio2_0cm_old) <- c("date","time","tempC")
fabio2_0cm_old <- mutate(fabio2_0cm_old, location = "0cm", probe_type = "old")

fabio2_25cm_old <- read_csv("data/fire_temperatures/fabio2-25cm-old-20170712.csv")
names(fabio2_25cm_old) <- c("date","time","tempC")
fabio2_25cm_old <- mutate(fabio2_25cm_old, location = "25cm", probe_type = "old")

fabio2_50cm_old <- read_csv("data/fire_temperatures/fabio2-50cm-old-20170712.csv")
names(fabio2_50cm_old) <- c("date","time","tempC")
fabio2_50cm_old <- mutate(fabio2_50cm_old, location = "50cm", probe_type = "old")

d1 <- rbind(fabio1_0cm_old, fabio1_25cm_old, fabio1_50cm_old,
            fabio1_0cm_new, fabio1_25cm_new, fabio1_50cm_new)

#+ Slice Experimental Burn Temperatures: Fire IDs 9-17 ####
# plot all data from sensors
# par(mfrow=c(2,2))
# plot(fabio1_0cm_old$time, fabio1_0cm_old$tempC,
#      main = "FABIO 1 Burns 7/12/2017-old")
# plot(fabio1_0cm_new$time, fabio1_0cm_new$tempC,
#      main = "FABIO 1 Burns 7/12/2017-new")
# plot(fabio1_25cm_old$time, fabio1_25cm_old$tempC,
#      main = "FABIO 1 Burns 7/12/2017-old")
# plot(fabio1_25cm_new$time, fabio1_25cm_new$tempC,
#      main = "FABIO 1 Burns 7/12/2017-new")
# plot(fabio1_50cm_old$time, fabio1_50cm_old$tempC,
#      main = "FABIO 1 Burns 7/12/2017-old")
# plot(fabio1_50cm_new$time, fabio1_50cm_new$tempC,
#      main = "FABIO 1 Burns 7/12/2017-new")

# qplot(time, tempC, data = d1, facets = location~., color = probe_type, linetype=probe_type, geom ="line", alpha = 0.5) +
#       # geom_line() +
#       theme_bw() +
#       ggtitle("FABIO 1 Burns 7/12/2017 - Old vs New Temp Probes")
# ggsave(filename = "figures/fabio1_burns_oldVSnew_20170712.png", width = 8, height = 10, dpi = 300)

# par(mfrow=c(3,1))
# plot(fabio2_0cm_old$time, fabio2_0cm_old$tempC, main = "FABIO 2 Burns 7/12/2017")
# plot(fabio2_25cm_old$time, fabio2_25cm_old$tempC)
# plot(fabio2_50cm_old$time, fabio2_50cm_old$tempC)

fireid9 <- rbind(
      filter(fabio2_0cm_old, between(time, 37300,39030)),
      filter(fabio2_25cm_old, between(time, 37300,39030)),
      filter(fabio2_50cm_old, between(time, 37300,39030))
      ) %>%
      mutate(fire_id = 9)
# fireid9 <- left_join(fireid9, select(fabio_burns_biomass, -fabio_id))

fireid10 <- rbind(
      filter(fabio2_0cm_old, between(time, 41600,42120)),
      filter(fabio2_25cm_old, between(time, 41600,42120)),
      filter(fabio2_50cm_old, between(time, 41600,42120))
      ) %>%
      mutate(fire_id = 10)
# fireid10 <- left_join(fireid10, select(fabio_burns_biomass, -fabio_id))

fireid11 <- rbind(
      filter(fabio1_0cm_old, between(time, 41700,42160)),
      filter(fabio1_25cm_old, between(time, 41700,42160)),
      filter(fabio1_50cm_old, between(time, 41700,42160))
      ) %>%
      mutate(fire_id = 11)
# fireid11 <- left_join(fireid11, select(fabio_burns_biomass, -fabio_id))

fireid12 <- rbind(
      filter(fabio2_0cm_old, between(time, 46150,47700)),
      filter(fabio2_25cm_old, between(time, 46150,47700)),
      filter(fabio2_50cm_old, between(time, 46150,47700))
      ) %>%
      mutate(fire_id = 12)
# fireid12 <- left_join(fireid12, select(fabio_burns_biomass, -fabio_id))

fireid13 <- rbind(
      filter(fabio1_0cm_old, between(time, 46970,47600)),
      filter(fabio1_25cm_old, between(time, 46970,47600)),
      filter(fabio1_50cm_old, between(time, 46970,47600))
      ) %>%
      mutate(fire_id = 13)
# fireid13 <- left_join(fireid13, select(fabio_burns_biomass, -fabio_id))

fireid14 <- rbind(
      filter(fabio2_0cm_old, between(time, 49180,50120)),
      filter(fabio2_25cm_old, between(time, 49180,50120)),
      filter(fabio2_50cm_old, between(time, 49180,50120))
      ) %>%
      mutate(fire_id = 14)
# fireid14 <- left_join(fireid14, select(fabio_burns_biomass, -fabio_id))

fireid15 <- rbind(
      filter(fabio1_0cm_old, between(time, 49120,49560)),
      filter(fabio1_25cm_old, between(time, 49120,49560)),
      filter(fabio1_50cm_old, between(time, 49120,49560))
      ) %>%
      mutate(fire_id = 15)
# fireid15 <- left_join(fireid15, select(fabio_burns_biomass, -fabio_id))

fireid16 <- rbind(
      filter(fabio1_0cm_old, between(time, 51930,52160)),
      filter(fabio1_25cm_old, between(time, 51930,52160)),
      filter(fabio1_50cm_old, between(time, 51930,52160))
      ) %>%
      mutate(fire_id = 16)
# fireid16 <- left_join(fireid16, select(fabio_burns_biomass, -fabio_id))

fireid17 <- rbind(
      filter(fabio2_0cm_old, between(time, 54120,55300)),
      filter(fabio2_25cm_old, between(time, 54120,55300)),
      filter(fabio2_50cm_old, between(time, 54120,55300))
      ) %>%
      mutate(fire_id = 17)
# fireid17 <- left_join(fireid17, select(fabio_burns_biomass, -fabio_id))


#+ Experimental Burns July 14, 2017: Fire IDs 18-24 ####
fabio2_0cm_old <- read_csv("data/fire_temperatures/fabio2-0cm-old-20170714.csv")
names(fabio2_0cm_old) <- c("date","time","tempC")
fabio2_0cm_old <- mutate(fabio2_0cm_old, location = "0cm", probe_type = "old")

fabio2_25cm_old <- read_csv("data/fire_temperatures/fabio2-25cm-old-20170714.csv")
names(fabio2_25cm_old) <- c("date","time","tempC")
fabio2_25cm_old <- mutate(fabio2_25cm_old, location = "25cm", probe_type = "old")

fabio2_50cm_old <- read_csv("data/fire_temperatures/fabio2-50cm-old-20170714.csv")
names(fabio2_50cm_old) <- c("date","time","tempC")
fabio2_50cm_old <- mutate(fabio2_50cm_old, location = "50cm", probe_type = "old")

fabio2_0cm_new <- read_csv("data/fire_temperatures/fabio2-0cm-new-20170714.csv")
names(fabio2_0cm_new) <- c("date","time","tempC")
fabio2_0cm_new <- mutate(fabio2_0cm_new, location = "0cm", probe_type = "new")

fabio2_25cm_new <- read_csv("data/fire_temperatures/fabio2-25cm-new-20170714.csv")
names(fabio2_25cm_new) <- c("date","time","tempC")
fabio2_25cm_new <- mutate(fabio2_25cm_new, location = "25cm", probe_type = "new")

fabio2_50cm_new <- read_csv("data/fire_temperatures/fabio2-50cm-new-20170714.csv")
names(fabio2_50cm_new) <- c("date","time","tempC")
fabio2_50cm_new <- mutate(fabio2_50cm_new, location = "50cm", probe_type = "new")

#+ Slice Experimental Burn Temperatures: Fire IDs 18-24 ####
d1 <- rbind(fabio2_0cm_new,fabio2_0cm_old,fabio2_25cm_new,fabio2_25cm_old,fabio2_50cm_new,fabio2_50cm_old)
# ggplot(filter(d1, probe_type=="old"), aes(time, tempC)) +
#       geom_line() +
#       # geom_point() +
#       facet_grid(location~.) +
#       theme_bw() +
#       ggtitle("Wiregrass Burns 7/14/2017")
# ggsave("figures/wiregrassburns20170714.png", dpi = 300)

fireid18 <- filter(d1, probe_type == "old", between(time, 56120,56180)) %>%
      mutate(fire_id = 18)
# fireid18 <- left_join(fireid18, select(fabio_burns_biomass, -fabio_id))

fireid19 <- filter(d1, probe_type == "old", between(time, 57240,57600)) %>%
      mutate(fire_id = 19)
# fireid19 <- left_join(fireid19, select(fabio_burns_biomass, -fabio_id))

fireid20 <- filter(d1, probe_type == "old", between(time, 58830,58920)) %>%
      mutate(fire_id = 20)
# fireid20 <- left_join(fireid20, select(fabio_burns_biomass, -fabio_id))

fireid21 <- filter(d1, probe_type == "old", between(time, 61000,61110)) %>%
      mutate(fire_id = 21)
# fireid21 <- left_join(fireid21, select(fabio_burns_biomass, -fabio_id))

fireid22 <- filter(d1, probe_type == "old", between(time, 62140,62210))%>%
      mutate(fire_id = 22)
# fireid22 <- left_join(fireid22, select(fabio_burns_biomass, -fabio_id))

fireid23 <- filter(d1, probe_type == "old", between(time, 63310,63400))%>%
      mutate(fire_id = 23)
# fireid23 <- left_join(fireid23, select(fabio_burns_biomass, -fabio_id))

fireid24 <- filter(d1, probe_type == "old", between(time, 64320,64400))%>%
      mutate(fire_id = 24)
# fireid24 <- left_join(fireid24, select(fabio_burns_biomass, -fabio_id))


#+ Experimental Burns September 7, 2017: Fire IDs 25-37 ####
# list.files("data/fire_temperatures/")

fabio2_0cm_old <- read_csv("data/fire_temperatures/fabio2-0cm-old-20170907.csv")
fabio2_0cm_new <- read_csv("data/fire_temperatures/fabio2-0cm-new-20170907.csv")
names(fabio2_0cm_old) <- c("date","time","tempC")
fabio2_0cm_old <- mutate(fabio2_0cm_old, location = "0cm", probe_type = "old")
names(fabio2_0cm_new) <- c("date","time","tempC")
fabio2_0cm_new <- mutate(fabio2_0cm_new, location = "0cm", probe_type = "new")

fabio2_25cm_old <- read_csv("data/fire_temperatures/fabio2-25cm-old-20170907.csv")
fabio2_25cm_new <- read_csv("data/fire_temperatures/fabio2-25cm-new-20170907.csv")
names(fabio2_25cm_old) <- c("date","time","tempC")
fabio2_25cm_old <- mutate(fabio2_25cm_old, location = "25cm", probe_type = "old")
names(fabio2_25cm_new) <- c("date","time","tempC")
fabio2_25cm_new <- mutate(fabio2_25cm_new, location = "25cm", probe_type = "new")

fabio2_50cm_old <- read_csv("data/fire_temperatures/fabio2-50cm-old-20170907.csv")
fabio2_50cm_new <- read_csv("data/fire_temperatures/fabio-50cm-new-20170907.csv")
names(fabio2_50cm_old) <- c("date","time","tempC")
fabio2_50cm_old <- mutate(fabio2_50cm_old, location = "50cm", probe_type = "old")
names(fabio2_50cm_new) <- c("date","time","tempC")
fabio2_50cm_new <- mutate(fabio2_50cm_new, location = "50cm", probe_type = "new")

#+ Slice Experimental Burn Temperatures: Fire IDs 25-37 ####
d1 <- rbind(fabio2_0cm_new,fabio2_0cm_old,fabio2_25cm_new,fabio2_25cm_old,fabio2_50cm_new,fabio2_50cm_old)

# ggplot(filter(d1, probe_type=="old"), aes(time, tempC)) +
#       geom_line() +
#       # geom_point() +
#       facet_grid(location~.) +
#       theme_bw()
# ggsave("fire-temps.png", width = 10, height = 8, dpi = 150)
# ggplot(filter(d1, probe_type == "old", between(time, 37780,37970)),
#        aes(time, tempC)) +
#       geom_line() +
#       facet_grid((location~.))

fireid25 <- filter(d1, probe_type == "old", between(time, 36060,36200)) %>%
      mutate(fire_id = 25)
fireid26 <- filter(d1, probe_type == "old", between(time, 37780,37970)) %>%
      mutate(fire_id = 26)
fireid27 <- filter(d1, probe_type == "old", between(time, 39750,39940)) %>%
      mutate(fire_id = 27)
fireid28 <- filter(d1, probe_type == "old", between(time, 41360,41640)) %>%
      mutate(fire_id = 28)
fireid29 <- filter(d1, probe_type == "old", between(time, 43160,43400)) %>%
      mutate(fire_id = 29)
fireid30 <- filter(d1, probe_type == "old", between(time, 46030,46200)) %>%
      mutate(fire_id = 30)
fireid31 <- filter(d1, probe_type == "old", between(time, 47210,47340)) %>%
      mutate(fire_id = 31)
fireid32 <- filter(d1, probe_type == "old", between(time, 49240,49500)) %>%
      mutate(fire_id = 32)
fireid33 <- filter(d1, probe_type == "old", between(time, 51240,51440)) %>%
      mutate(fire_id = 33)
fireid34 <- filter(d1, probe_type == "old", between(time, 52830,53000)) %>%
      mutate(fire_id = 34)
fireid35 <- filter(d1, probe_type == "old", between(time, 56270,56415)) %>%
      mutate(fire_id = 35)
fireid36 <- filter(d1, probe_type == "old", between(time, 58030,58210)) %>%
      mutate(fire_id = 36)
fireid37 <- filter(d1, probe_type == "old", between(time, 59460,59840)) %>%
      mutate(fire_id = 37)


#+ Experimental Burns September 8, 2017: Fire IDs 38-42 ####
fabio2_0cm_old <- read_csv("data/fire_temperatures/fabio2-0cm-old-20170908.csv")
fabio2_0cm_new <- read_csv("data/fire_temperatures/fabio2-0cm-new-20170908.csv")
names(fabio2_0cm_old) <- c("date","time","tempC")
fabio2_0cm_old <- mutate(fabio2_0cm_old, location = "0cm", probe_type = "old")
names(fabio2_0cm_new) <- c("date","time","tempC")
fabio2_0cm_new <- mutate(fabio2_0cm_new, location = "0cm", probe_type = "new")

fabio2_25cm_old <- read_csv("data/fire_temperatures/fabio2-25cm-old-20170908.csv")
fabio2_25cm_new <- read_csv("data/fire_temperatures/fabio2-25cm-new-20170908.csv")
names(fabio2_25cm_old) <- c("date","time","tempC")
fabio2_25cm_old <- mutate(fabio2_25cm_old, location = "25cm", probe_type = "old")
names(fabio2_25cm_new) <- c("date","time","tempC")
fabio2_25cm_new <- mutate(fabio2_25cm_new, location = "25cm", probe_type = "new")

fabio2_50cm_old <- read_csv("data/fire_temperatures/fabio2-50cm-old-20170908.csv")
fabio2_50cm_new <- read_csv("data/fire_temperatures/fabio2-50cm-new-20170908.csv")
names(fabio2_50cm_old) <- c("date","time","tempC")
fabio2_50cm_old <- mutate(fabio2_50cm_old, location = "50cm", probe_type = "old")
names(fabio2_50cm_new) <- c("date","time","tempC")
fabio2_50cm_new <- mutate(fabio2_50cm_new, location = "50cm", probe_type = "new")

#+ Slice Experimental Burn Temperatures: Fire IDs 38-42 ####
d1 <- rbind(fabio2_0cm_new,fabio2_0cm_old,fabio2_25cm_new,fabio2_25cm_old,fabio2_50cm_new,fabio2_50cm_old)

# ggplot(filter(d1, probe_type=="old"), aes(time, tempC)) +
#       geom_line() +
#       # geom_point() +
#       facet_grid(location~.) +
#       theme_bw()
# ggsave("fire-temps.png", width = 10, height = 8, dpi = 150)
# ggplot(filter(d1, probe_type == "old", between(time, 41830,41940)),
#        aes(time, tempC)) +
#       geom_line() +
#       facet_grid((location~.))

fireid38 <- filter(d1, probe_type == "old", between(time, 41830,41940)) %>%
      mutate(fire_id = 38)
fireid39 <- filter(d1, probe_type == "old", between(time, 44690,44810)) %>%
      mutate(fire_id = 39)
fireid40 <- filter(d1, probe_type == "old", between(time, 46180,46340)) %>%
      mutate(fire_id = 40)
fireid41 <- filter(d1, probe_type == "old", between(time, 47570,47700)) %>%
      mutate(fire_id = 41)
fireid42 <- filter(d1, probe_type == "old", between(time, 48360,48700)) %>%
      mutate(fire_id = 42)

d1 <- rbind(fireid1,fireid2,fireid3,fireid4,fireid5,fireid6,fireid7,fireid8,fireid9,fireid10,fireid11,fireid12,fireid13,fireid14,fireid15,fireid16,fireid17,fireid18,fireid19,fireid20,fireid21,fireid22,fireid23,fireid24,fireid25,fireid26,fireid27,fireid28,fireid29,fireid30,fireid31,fireid32,fireid33,fireid34,fireid35,fireid36,fireid37,fireid38,fireid39,fireid40,fireid41,fireid42)

d1 <- d1 %>%
      mutate(position = "middle") %>%
      select(date:probe_type, position, fire_id)

#+ Experimental Burns December 1,4-5, 2017: Fire IDs 43-74, Piled vs. Standing ####

## Temperatures from piled fuels
flist <- list.files("data/fire_temperatures", pattern = "piled", full.names = T)
flist <- flist[grep(".csv", flist, fixed=T)]

piled_0cmL <- flist[grep("-0cmL", flist, fixed = T)]
piled_0cmM <- flist[grep("-0cmM", flist, fixed = T)]
piled_0cmR <- flist[grep("-0cmR", flist, fixed = T)]

piled_25cmL <- flist[grep("-25cmL", flist, fixed = T)]
piled_25cmM <- flist[grep("-25cmM", flist, fixed = T)]
piled_25cmR <- flist[grep("-25cmR", flist, fixed = T)]

piled_50cmL <- flist[grep("-50cmL", flist, fixed = T)]
piled_50cmM <- flist[grep("-50cmM", flist, fixed = T)]
piled_50cmR <- flist[grep("-50cmR", flist, fixed = T)]

cnames <- c("date", "time", "tempC")
piled_temps <- rbind(
      ldply(piled_0cmL, function(x){read_csv(x, col_names = cnames, skip = 1)}) %>%
            mutate(location = "0cm", probe_type = "new", position = "left"),
      ldply(piled_0cmM, function(x){read_csv(x, col_names = cnames, skip = 1)}) %>%
            mutate(location = "0cm", probe_type = "new", position = "middle"),
      ldply(piled_0cmR, function(x){read_csv(x, col_names = cnames, skip = 1)}) %>%
            mutate(location = "0cm", probe_type = "new", position = "right"),
      ldply(piled_25cmL, function(x){read_csv(x, col_names = cnames, skip = 1)}) %>%
            mutate(location = "25cm", probe_type = "new", position = "left"),
      ldply(piled_25cmM, function(x){read_csv(x, col_names = cnames, skip = 1)}) %>%
            mutate(location = "25cm", probe_type = "new", position = "middle"),
      ldply(piled_25cmR, function(x){read_csv(x, col_names = cnames, skip = 1)}) %>%
            mutate(location = "25cm", probe_type = "new", position = "right"),
      ldply(piled_50cmL, function(x){read_csv(x, col_names = cnames, skip = 1)}) %>%
            mutate(location = "50cm", probe_type = "new", position = "left"),
      ldply(piled_50cmM, function(x){read_csv(x, col_names = cnames, skip = 1)}) %>%
            mutate(location = "50cm", probe_type = "new", position = "middle"),
      ldply(piled_50cmR, function(x){read_csv(x, col_names = cnames, skip = 1)}) %>%
            mutate(location = "50cm", probe_type = "new", position = "right")
)
# summary(piled_temps)


t <- filter(piled_temps, date=="2017-12-01")
# ggplot(t, aes(time, tempC)) +
#       geom_line() +
#       facet_grid(location~position) +
#       theme_bw() +
#       ggtitle("Piled fuels temperatures")
# time is 1-hour off for some of the loggers

t1 <- filter(t, location=="50cm", position=="middle" | position=="right")
t2 <- filter(t, location=="25cm", position=="left"| position=="right")
t3 <- filter(t, location=="0cm", position=="left")
t4 <- rbind(t1,t2,t3)
# str(t4)

b <- anti_join(piled_temps, t4)

t4$time <- t4$time - 3600
t4$time <- hms::as.hms(t4$time)

# sum(c(length(b$date),length(t4$date)))# should = obs of piled_temps
piled_temps <- union(b,t4)


t <- filter(piled_temps, date=="2017-12-01")

# ggplot(fireid71, aes(as.numeric(time), tempC)) +
#       geom_line(aes(color = position, linetype = location)) +
#       # facet_grid(location~position) +
#       theme_bw() +
#       ggtitle("Piled fuels temperatures") +
#       geom_hline(yintercept = 50, color = "purple", linetype = "dashed") +
#       geom_hline(yintercept = 60, color = "brown", linetype = "dashed")

dec_5 <- filter(piled_temps, date=="2017-12-05")
fireid73 <- filter(dec_5, between(time, 53930, 55060)) %>%
      mutate(fire_id = 73)
fireid71 <- filter(dec_5, between(time, 51000, 52500))
maxtemp71 <- max(fireid71$tempC)
fireid71 <- filter(fireid71, tempC != maxtemp71) %>%
      mutate(fire_id = 71)
fireid69 <- filter(dec_5, between(time, 48800, 49600)) %>%
      mutate(fire_id = 69)
fireid67 <- filter(dec_5, between(time, 46920, 47520)) %>%
      mutate(fire_id = 67)
fireid65 <- filter(dec_5, between(time, 45400, 45850)) %>%
      mutate(fire_id = 65)
fireid63 <- filter(dec_5, between(time, 43750, 44200)) %>%
      mutate(fire_id = 63)

dec_4 <- filter(piled_temps, date=="2017-12-04")
# exclue inaccurate measures from 0cm-right sensor after fire id 51
fireid61 <- filter(dec_4, location!="0cm" | position!="right", between(time, 58600, 60100)) %>%
      mutate(fire_id = 61)
fireid59 <- filter(dec_4, location!="0cm" | position!="right", between(time, 55500, 56700)) %>%
      mutate(fire_id = 59)
fireid57 <- filter(dec_4, location!="0cm" | position!="right", between(time, 53350, 54600)) %>%
      mutate(fire_id = 57)
fireid55 <- filter(dec_4, location!="0cm" | position!="right", between(time, 49500, 50300)) %>%
      mutate(fire_id = 55)
fireid53 <- filter(dec_4, location!="0cm" | position!="right", between(time, 47350, 47850)) %>%
      mutate(fire_id = 53)
fireid51 <- filter(dec_4, between(time, 45500, 46830)) %>%
      mutate(fire_id = 51)

dec_1 <- filter(piled_temps, date=="2017-12-01")
fireid49 <- filter(dec_1, between(time, 61700, 62560)) %>%
      mutate(fire_id = 49)
fireid47 <- filter(dec_1, between(time, 60500, 61000)) %>%
      mutate(fire_id = 47)
fireid45 <- filter(dec_1, between(time, 53700, 54200)) %>%
      mutate(fire_id = 45)
fireid43 <- filter(dec_1, between(time, 51850, 52300)) %>%
      mutate(fire_id = 43)


## Temperatures from standing fuels
flist <- list.files("data/fire_temperatures", pattern = "standing", full.names = T)
flist <- flist[grep(".csv", flist, fixed=T)]

standing_0cmL <- flist[grep("-0cmL", flist, fixed = T)]
standing_0cmM <- flist[grep("-0cmM", flist, fixed = T)]
standing_0cmR <- flist[grep("-0cmR", flist, fixed = T)]

standing_25cmL <- flist[grep("-25cmL", flist, fixed = T)]
standing_25cmM <- flist[grep("-25cmM", flist, fixed = T)]
standing_25cmR <- flist[grep("-25cmR", flist, fixed = T)]

standing_50cmL <- flist[grep("-50cmL", flist, fixed = T)]
standing_50cmM <- flist[grep("-50cmM", flist, fixed = T)]
standing_50cmR <- flist[grep("-50cmR", flist, fixed = T)]

cnames <- c("date", "time", "tempC")
standing_temps <- rbind(
      ldply(standing_0cmL, function(x){read_csv(x, col_names = cnames, skip = 1)}) %>%
            mutate(location = "0cm", probe_type = "new", position = "left"),
      ldply(standing_0cmM, function(x){read_csv(x, col_names = cnames, skip = 1)}) %>%
            mutate(location = "0cm", probe_type = "new", position = "middle"),
      ldply(standing_0cmR, function(x){read_csv(x, col_names = cnames, skip = 1)}) %>%
            mutate(location = "0cm", probe_type = "new", position = "right"),
      ldply(standing_25cmL, function(x){read_csv(x, col_names = cnames, skip = 1)}) %>%
            mutate(location = "25cm", probe_type = "new", position = "left"),
      ldply(standing_25cmM, function(x){read_csv(x, col_names = cnames, skip = 1)}) %>%
            mutate(location = "25cm", probe_type = "new", position = "middle"),
      ldply(standing_25cmR, function(x){read_csv(x, col_names = cnames, skip = 1)}) %>%
            mutate(location = "25cm", probe_type = "new", position = "right"),
      ldply(standing_50cmL, function(x){read_csv(x, col_names = cnames, skip = 1)}) %>%
            mutate(location = "50cm", probe_type = "new", position = "left"),
      ldply(standing_50cmM, function(x){read_csv(x, col_names = cnames, skip = 1)}) %>%
            mutate(location = "50cm", probe_type = "new", position = "middle"),
      ldply(standing_50cmR, function(x){read_csv(x, col_names = cnames, skip = 1)}) %>%
            mutate(location = "50cm", probe_type = "new", position = "right")
)
# summary(standing_temps)


t <- filter(standing_temps, date=="2017-12-01")

t1 <- filter(t, location=="50cm", position=="left")
t2 <- filter(t, location=="25cm", position=="left" | position=="right")
t3 <- filter(t, location=="0cm", position=="right")
t4 <- rbind(t1,t2,t3)
# str(t4)

b <- anti_join(standing_temps, t4)

t4$time <- t4$time - 3600
t4$time <- hms::as.hms(t4$time)

# sum(c(length(b$date),length(t4$date)))# should = obs of piled_temps
standing_temps <- union(b,t4)

t <- filter(standing_temps, date=="2017-12-01")

# ggplot(fireid74, aes(as.numeric(time), tempC)) +
#       geom_line(aes(color = position, linetype = location)) +
#       # facet_grid(location~position) +
#       theme_bw() +
#       ggtitle("Standing fuels temperatures") +
#       geom_hline(yintercept = 50, color = "purple", linetype = "dashed") +
#       geom_hline(yintercept = 60, color = "brown", linetype = "dashed")

dec_5 <- filter(standing_temps, date=="2017-12-05")
fireid74 <- filter(dec_5, between(time, 54220, 54430)) %>%
      mutate(fire_id = 74)
fireid72 <- filter(dec_5, between(time, 51650, 52000)) %>%
      mutate(fire_id = 72)
fireid70 <- filter(dec_5, between(time, 49200, 49400)) %>%
      mutate(fire_id = 70)
fireid68 <- filter(dec_5, between(time, 47200, 47420)) %>%
      mutate(fire_id = 68)
fireid66 <- filter(dec_5, between(time, 45600, 45830)) %>%
      mutate(fire_id = 66)
fireid64 <- filter(dec_5, between(time, 44000, 44160)) %>%
      mutate(fire_id = 64)

dec_4 <- filter(standing_temps, date=="2017-12-04")
fireid62 <- filter(dec_4, between(time, 59150, 59450)) %>%
      mutate(fire_id = 62)
fireid60 <- filter(dec_4, between(time, 55750, 56030)) %>%
      mutate(fire_id = 60)
fireid58 <- filter(dec_4, between(time, 53570, 53900)) %>%
      mutate(fire_id = 58)
fireid56 <- filter(dec_4, between(time, 49400, 49700)) %>%
      mutate(fire_id = 56)
fireid54 <- filter(dec_4, between(time, 47780, 48000)) %>%
      mutate(fire_id = 54)
fireid52 <- filter(dec_4, between(time, 45850, 46100)) %>%
      mutate(fire_id = 52)

dec_1 <- filter(standing_temps, date=="2017-12-01")
fireid50 <- filter(dec_1, between(time, 60900, 61250)) %>%
      mutate(fire_id = 50)
fireid48 <- filter(dec_1, between(time, 56030, 56400)) %>%
      mutate(fire_id = 48)
fireid46 <- filter(dec_1, between(time, 53630, 54030)) %>%
      mutate(fire_id = 46)
fireid44 <- filter(dec_1, between(time, 51750, 52100)) %>%
      mutate(fire_id = 44)



#+ Join fire data into single file ####

n <- (seq(43,74,1))
n <- paste("fireid", n, sep="")
n <- noquote(paste(n,",", sep = ""))

d1 <- rbind(d1, fireid43, fireid44, fireid45, fireid46, fireid47, fireid48, fireid49, fireid50, fireid51, fireid52, fireid53, fireid54, fireid55, fireid56, fireid57, fireid58, fireid59, fireid60, fireid61, fireid62, fireid63, fireid64, fireid65, fireid66, fireid67, fireid68, fireid69, fireid70, fireid71, fireid72, fireid73, fireid74)


write_csv(d1,"data/fabio-fires-temperatures.csv")
d1 <- left_join(d1, fires_biomass, by = "fire_id")

#+ Plot Experimental FABIO Burns ####
time_abv150 <- d1 %>% filter(probe_type == "old") %>%
             group_by(location,fire_id, pct_green, biomass, litter_biomass, biomass_type) %>%
             filter(tempC>150) %>%
             summarise(s_abv150 = length(tempC))

max_temp <- d1 %>%
      filter(probe_type == "old") %>%
      group_by(location,fire_id, pct_green, biomass, litter_biomass, biomass_type) %>%
      summarise(max_temp = max(tempC))


d2 <- left_join(max_temp,time_abv150)
d2$s_abv150[is.na(d2$s_abv150)] <- 0

ggplot(d2, aes(s_abv150, max_temp, color = factor(pct_green))) +
      geom_point(aes(size = biomass)) +
      # scale_fill_continuous(low = "yellow", high = "red") +
      # geom_bar(stat = "identity", position = "dodge", aes(fill = factor(pct_green))) +
      facet_grid(location~litter_biomass) +
      ylab("Maximum Temperature ºC") +
      xlab("Seconds >150 ºC") +
      theme_bw()

ggplot(d2 %>% filter(litter_biomass==0),
       aes(s_abv150, max_temp, color = factor(pct_green))) +
      geom_point(aes(size = biomass, shape = biomass_type)) +
      # scale_fill_continuous(low = "yellow", high = "red") +
      # geom_bar(stat = "identity", position = "dodge", aes(fill = factor(pct_green))) +
      facet_grid(location~.) +
      ylab("Maximum Temperature ºC") +
      xlab("Seconds >150 ºC") +
      theme_bw()

ggplot(d2, aes(biomass, max_temp)) +
      geom_point(aes(color = factor(pct_green), shape = biomass_type)) +
      # geom_line(aes(color = factor(pct_green))) +
      # geom_line(aes(color = factor(pct_green))) +
      # geom_bar(stat = "identity", position = "dodge", aes(fill = factor(pct_green))) +
      facet_grid(location~litter_biomass) +
      ylab("Maximum Temperature ºC") +
      xlab("Standing Biomass (g)") +
      theme_bw()

ggplot(d2, aes(biomass, s_abv150)) +
      geom_point(aes(color = factor(pct_green), shape = biomass_type)) +
      # geom_line(aes(color = factor(pct_green))) +
      # geom_line(aes(color = factor(pct_green))) +
      # geom_bar(stat = "identity", position = "dodge", aes(fill = factor(pct_green))) +
      facet_grid(location~litter_biomass) +
      ylab("Seconds >150 ºC") +
      xlab("Standing Biomass (g)") +
      theme_bw()

ggplot(d2 %>% filter(litter_biomass==0), aes(biomass, s_abv150)) +
      geom_point(aes(color = factor(pct_green))) +
      # geom_smooth(aes(color = factor(pct_green), linetype = biomass_type)) +
      geom_line(aes(color = factor(pct_green))) +
      # geom_bar(stat = "identity", position = "dodge", aes(fill = factor(pct_green))) +
      facet_grid(location~biomass_type) +
      ylab("Seconds >150 ºC") +
      xlab("Standing Biomass (g)") +
      theme_bw()

#+ Load Flame Height Temperature Sensor Data -------------------------------
flame1_0cm <- read_csv("data/fire_temperatures_raw/bland-c-f1-0cm-20170706.csv")
names(flame1_0cm) <- c("date","time","tempC")
flame1_0cm <- mutate(flame1_0cm, location = "0cm", probe_type = "old")
flame1_25cm <- read_csv("data/fire_temperatures_raw/bland-c-f1-25cm-20170706.csv")
names(flame1_25cm) <- c("date","time","tempC")
flame1_25cm <- mutate(flame1_25cm, location = "25cm", probe_type = "old")
flame1_50cm <- read_csv("data/fire_temperatures_raw/bland-c-f1-50cm-20170706.csv")
names(flame1_50cm) <- c("date","time","tempC")
flame1_50cm <- mutate(flame1_50cm, location = "50cm", probe_type = "old")

flame3_0cm <- read_csv("data/fire_temperatures_raw/bland-c-f3-0cm-20170706.csv")
names(flame3_0cm) <- c("date","time","tempC")
flame3_0cm <- mutate(flame3_0cm, location = "0cm", probe_type = "old")
flame3_25cm <- read_csv("data/fire_temperatures_raw/bland-c-f3-25cm-20170706.csv")
names(flame3_25cm) <- c("date","time","tempC")
flame3_25cm <- mutate(flame3_25cm, location = "25cm", probe_type = "old")
# flame3_50cm <- read_csv("data/fire_temperatures_raw/bland-c-f3-50cm-20170706.csv")
# names(flame3_50cm) <- c("date","time","tempC")
## This logger had failure in correctly recording temperature.

flame4_0cm <- read_csv("data/fire_temperatures_raw/bland-c-f4-0cm-20170706.csv")
names(flame4_0cm) <- c("date","time","tempC")
flame4_0cm <- mutate(flame4_0cm, location = "0cm", probe_type = "old")
flame4_25cm <- read_csv("data/fire_temperatures_raw/bland-c-f4-25cm-20170706.csv")
names(flame4_25cm) <- c("date","time","tempC")
flame4_25cm <- mutate(flame4_0cm, location = "25cm", probe_type = "old")
flame4_50cm <- read_csv("data/fire_temperatures_raw/bland-c-f4-50cm-20170706.csv")
names(flame4_50cm) <- c("date","time","tempC")
flame4_50cm <- mutate(flame4_50cm, location = "50cm", probe_type = "old")

flame5_0cm <- read_csv("data/fire_temperatures_raw/bland-c-f5-0cm-20170706.csv")
names(flame5_0cm) <- c("date","time","tempC")
flame5_0cm <- mutate(flame5_0cm, location = "0cm", probe_type = "old")
flame5_25cm <- read_csv("data/fire_temperatures_raw/bland-c-f5-25cm-20170706.csv")
names(flame5_25cm) <- c("date","time","tempC")
flame5_25cm <- mutate(flame5_25cm, location = "25cm", probe_type = "old")
flame5_50cm <- read_csv("data/fire_temperatures_raw/bland-c-f5-50cm-20170706.csv")
names(flame5_50cm) <- c("date","time","tempC")
flame5_50cm <- mutate(flame5_50cm, location = "50cm", probe_type = "old")

flame6_0cm <- read_csv("data/fire_temperatures_raw/bland-c-f6-0cm-20170706.csv")
names(flame6_0cm) <- c("date","time","tempC")
flame6_0cm <- mutate(flame6_0cm, location = "0cm", probe_type = "old")
flame6_25cm <- read_csv("data/fire_temperatures_raw/bland-c-f6-25cm-20170706.csv")
names(flame6_25cm) <- c("date","time","tempC")
flame6_25cm <- mutate(flame6_25cm, location = "25cm", probe_type = "old")
flame6_50cm <- read_csv("data/fire_temperatures_raw/bland-c-f6-50cm-20170706.csv")
names(flame6_50cm) <- c("date","time","tempC")
flame6_50cm <- mutate(flame6_50cm, location = "50cm", probe_type = "old")

flame7_0cm <- read_csv("data/fire_temperatures_raw/bland-c-f7-0cm-20170706.csv")
names(flame7_0cm) <- c("date","time","tempC")
flame7_0cm <- mutate(flame7_0cm, location = "0cm", probe_type = "old")
flame7_25cm <- read_csv("data/fire_temperatures_raw/bland-c-f7-25cm-20170706.csv")
names(flame7_25cm) <- c("date","time","tempC")
flame7_25cm <- mutate(flame7_25cm, location = "25cm", probe_type = "old")
flame7_50cm <- read_csv("data/fire_temperatures_raw/bland-c-f7-50cm-20170706.csv")
names(flame7_50cm) <- c("date","time","tempC")
flame7_50cm <- mutate(flame7_50cm, location = "50cm", probe_type = "old")

flame8_0cm <- read_csv("data/fire_temperatures_raw/bland-c-f8-0cm-20170706.csv")
names(flame8_0cm) <- c("date","time","tempC")
flame8_0cm <- mutate(flame8_0cm, location = "0cm", probe_type = "old")
flame8_25cm <- read_csv("data/fire_temperatures_raw/bland-c-f8-25cm-20170706.csv")
names(flame8_25cm) <- c("date","time","tempC")
flame8_25cm <- mutate(flame8_25cm, location = "25cm", probe_type = "old")
flame8_50cm <- read_csv("data/fire_temperatures_raw/bland-c-f8-50cm-20170706.csv")
names(flame8_50cm) <- c("date","time","tempC")
flame8_50cm <- mutate(flame8_50cm, location = "50cm", probe_type = "old")

flame1 <- rbind(flame1_0cm,flame1_25cm,flame1_50cm) %>% mutate(id="flame1")
flame3 <- rbind(flame3_0cm,flame3_25cm) %>% mutate(id="flame3")
flame4 <- rbind(flame4_0cm,flame4_25cm,flame4_50cm) %>% mutate(id="flame4")
flame5 <- rbind(flame5_0cm,flame5_25cm,flame5_50cm) %>% mutate(id="flame5")
flame6 <- rbind(flame6_0cm,flame6_25cm,flame6_50cm) %>% mutate(id="flame6")
flame7 <- rbind(flame7_0cm,flame7_25cm,flame7_50cm) %>% mutate(id="flame7")
flame8 <- rbind(flame8_0cm,flame8_25cm,flame8_50cm) %>% mutate(id="flame8")

#+ Slice Flame Sensor Temperature Data ####

# qplot(time,tempC,data=filter(flame1, between(time, 43240,43340)),facets = location~.)
flame1 <- filter(flame1, between(time, 43240,43340))
# flame1 <- left_join(flame1, flame_veg_data %>%
#                           filter(date > "2017-06-01") %>%
#                           select(-date),
#                     by = "id")

# qplot(time,tempC,data=filter(flame3, between(time, 42800,42990)),facets = location~.)
flame3 <- filter(flame3, between(time, 42800,42990))
# flame3 <- left_join(flame3, flame_veg_data %>%
#                           filter(date > "2017-06-01") %>%
#                           select(-date),
#                     by = "id")

# qplot(time,tempC,data=flame4,facets = location~.)
# qplot(time,tempC,data=filter(flame4, between(time, 44680,44810)),facets = location~.)
flame4 <- filter(flame4, between(time, 44680,44810))
# flame4 <- left_join(flame4, flame_veg_data %>%
#                           filter(date > "2017-06-01") %>%
#                           select(-date),
#                     by = "id")

# qplot(time,tempC,data=flame5,facets = location~.)
# qplot(time,tempC,data=filter(flame5, between(time, 44680,44810)),facets = location~.)
flame5 <- filter(flame5, between(time, 44680,44810))
# flame5 <- left_join(flame5, flame_veg_data %>%
#                           filter(date > "2017-06-01") %>%
#                           select(-date),
#                     by = "id")

# qplot(time,tempC,data=filter(flame6, between(time, 43310,43550)),facets = location~.)
flame6 <- filter(flame6, between(time, 43310,43550))
# flame6 <- left_join(flame6, flame_veg_data %>%
#                           filter(date > "2017-06-01") %>%
#                           select(-date),
#                     by = "id")

# qplot(time,tempC,data=flame7,facets = location~.)
# qplot(time,tempC,data=filter(flame7, between(time, 41300,41600)),facets = location~.)
flame7 <- filter(flame7, between(time, 41300,41600))
# flame7 <- left_join(flame7, flame_veg_data %>%
#                           filter(date > "2017-06-01") %>%
#                           select(-date),
#                     by = "id")

# qplot(time,tempC,data=flame8,facets = location~.)
# qplot(time,tempC,data=filter(flame8, between(time, 40800,41060)),facets = location~.)
flame8 <- filter(flame8, between(time, 40800,41060))
# flame8 <- left_join(flame8, flame_veg_data %>%
#                           filter(date > "2017-06-01") %>%
#                           select(-date),
#                     by = "id")

prescribed_fire_temps <- rbind(flame1,flame3,flame4,flame5,flame6,flame7,flame8)

unique(filter(prescribed_fire_temps, id=="flame5")$location)


#+ Slice FABIO Prescribed Fire Temperature Data ####
fabio_blanding_prescribe <- rbind(
      filter(fabio_blanding_0cm, between(time, 45000,45150)) %>%
            mutate(location = "0cm"),
      filter(fabio_blanding_25cm, between(time, 45000,45150)) %>%
            mutate(location = "25cm"),
      filter(fabio_blanding_50cm, between(time, 45000,45150)) %>%
            mutate(location = "50cm")
)

fabio_blanding_prescribe <- mutate(
      fabio_blanding_prescribe, id = "fabio naturale")

prescribed_fire_temps <- rbind(prescribed_fire_temps, fabio_blanding_prescribe)

ggplot(prescribed_fire_temps, aes(time,tempC, color = id)) +
      scale_color_viridis_d() +
      geom_line() +
      theme_bw() +
      ggtitle("Temperatures from Camp Blanding Prescribed Fire")
# flame 5 temperature sensors did not record anyting that looks like fire
prescribed_fire_temps <- filter(prescribed_fire_temps, id!="flame5")

write_csv(prescribed_fire_temps, "data/prescribed-fire-temps.csv")

#+ Plot Test Burn Fire Temperatures -------------------------------------
ggplot(rbind(test_burn_standing,test_burn_thatch), aes(time, tempC, col = location)) +
      geom_line() +
      theme_bw() +
      facet_grid(.~ fuel_type, scales = "free_x") +
      ylab("Temperature ºC") +
      xlab("Time of Day (24 hour)") +
      ggtitle("Test Burn")
# ggsave("figures/bivens-test-burn.png", dpi = 600)

filter(fireid1, tempC > 300) %>% group_by(location) %>%
      summarize(max_temp = max(tempC), time_above_300C = length(tempC))
filter(fireid2, tempC > 300) %>% group_by(location) %>%
      summarize(max_temp = max(tempC), time_above_300C = length(tempC))
filter(fireid3, tempC > 300) %>% group_by(location) %>%
      summarize(max_temp = max(tempC), time_above_300C = length(tempC))
fireid4 %>% group_by(location) %>%
      summarize(max_temp = max(tempC))
fireid5 %>% group_by(location) %>%
      summarize(max_temp = max(tempC))
fireid6 %>% group_by(location) %>%
      summarize(max_temp = max(tempC))
filter(fireid7, tempC > 300) %>% group_by(location) %>%
      summarize(max_temp = max(tempC), time_above_300C = length(tempC))
filter(fireid8, tempC > 300) %>% group_by(location) %>%
      summarize(max_temp = max(tempC), time_above_300C = length(tempC))




#+ Plot Prescribed Fire Temperatures ---------------------------------------
ggplot(fabio_blanding_prescribe, aes(time, tempC, col = location)) +
      geom_line() +
      theme_bw() +
      # facet_grid(.~ fuel_type, scales = "free_x") +
      ylab("Temperature ºC") +
      xlab("Time of Day (24 hour)") +
      ggtitle("FABIO Prescribed Fire Temperatures 7/6/2017")

filter(fabio_blanding_prescribe, tempC > 300) %>% group_by(location) %>%
      summarize(max_temp = max(tempC), time_above_300C = length(tempC))
filter(flame1, tempC > 300) %>% group_by(location) %>%
      summarize(max_temp = max(tempC), time_above_300C = length(tempC))
filter(flame6, tempC > 300) %>% group_by(location) %>%
      summarize(max_temp = max(tempC), time_above_300C = length(tempC))
filter(flame8, tempC > 300) %>% group_by(location) %>%
      summarize(max_temp = max(tempC), time_above_300C = length(tempC))
