library(readr);library(ggplot2);library(dplyr)

loggers <- read_csv("data/processed_data/2019 serdp processed data/2019-final-round-loggers-serdp.csv")

summary(loggers)
loggers <- loggers[c(2:5)]

loggers <- loggers %>% 
  mutate(date = substr(time, 1,8),
         time_2 = substr(time, 9,20),
         date_time = lubridate::mdy_hms(paste(date,time_2)))



loggers <- loggers[-c(1:1)]
loggers <- loggers[-c(5:5)]

loggers <- loggers %>% 
  filter(date_time < "2019-08-01 20:00:00") %>% 
  filter(date_time > "2019-06-12 20:00:00") %>% 
  filter(tempC > 10) %>% 
  filter(RH > 20) %>% 
  mutate(installation = "test",
         plot_id = "test",
         status = "n")

loggers <- loggers %>% 
  mutate(date = as.Date(date, "%m/%d/%y"),
         days = julian(date, origin = as.Date("2019-06-12")))

#adding all of installation and status and plot id text ##

loggers$installation[loggers$logger_id=="01"] <- "hancock"
loggers$installation[loggers$logger_id=="02"] <- "riversedge"
loggers$installation[loggers$logger_id=="03"] <- "riversedge"
loggers$installation[loggers$logger_id=="04"] <- "brown"
loggers$installation[loggers$logger_id=="05"] <- "silversprings"
loggers$installation[loggers$logger_id=="06"] <- "munson"
loggers$installation[loggers$logger_id=="07"] <- "silversprings"
loggers$installation[loggers$logger_id=="08"] <- "hancock"
loggers$installation[loggers$logger_id=="10"] <- "hancock"
loggers$installation[loggers$logger_id=="11"] <- "brown"
loggers$installation[loggers$logger_id=="12"] <- "munson"
loggers$installation[loggers$logger_id=="15"] <- "hancock"
loggers$installation[loggers$logger_id=="16"] <- "peace"
loggers$installation[loggers$logger_id=="17"] <- "wes"
loggers$installation[loggers$logger_id=="19"] <- "wes"
loggers$installation[loggers$logger_id=="20"] <- "peace"
loggers$installation[loggers$logger_id=="21"] <- "peace"
loggers$installation[loggers$logger_id=="23"] <- "wes"
loggers$installation[loggers$logger_id=="24"] <- "wes"

loggers$plot_id[loggers$logger_id=="01"] <- "n2"
loggers$plot_id[loggers$logger_id=="02"] <- "n1"
loggers$plot_id[loggers$logger_id=="03"] <- "i2"
loggers$plot_id[loggers$logger_id=="04"] <- "n2"
loggers$plot_id[loggers$logger_id=="05"] <- "n1"
loggers$plot_id[loggers$logger_id=="06"] <- "i1"
loggers$plot_id[loggers$logger_id=="07"] <- "i1"
loggers$plot_id[loggers$logger_id=="08"] <- "i3"
loggers$plot_id[loggers$logger_id=="10"] <- "n1"
loggers$plot_id[loggers$logger_id=="11"] <- "i2"
loggers$plot_id[loggers$logger_id=="12"] <- "n1"
loggers$plot_id[loggers$logger_id=="15"] <- "i4"
loggers$plot_id[loggers$logger_id=="16"] <- "i2"
loggers$plot_id[loggers$logger_id=="17"] <- "n1"
loggers$plot_id[loggers$logger_id=="19"] <- "i3"
loggers$plot_id[loggers$logger_id=="20"] <- "i1"
loggers$plot_id[loggers$logger_id=="21"] <- "n1"
loggers$plot_id[loggers$logger_id=="23"] <- "i3"
loggers$plot_id[loggers$logger_id=="24"] <- "n1"

loggers$status[loggers$logger_id=="01"] <- "native"
loggers$status[loggers$logger_id=="02"] <- "native"
loggers$status[loggers$logger_id=="03"] <- "invaded"
loggers$status[loggers$logger_id=="04"] <- "native"
loggers$status[loggers$logger_id=="05"] <- "native"
loggers$status[loggers$logger_id=="06"] <- "invaded"
loggers$status[loggers$logger_id=="07"] <- "invaded"
loggers$status[loggers$logger_id=="08"] <- "invaded"
loggers$status[loggers$logger_id=="10"] <- "native"
loggers$status[loggers$logger_id=="11"] <- "invaded"
loggers$status[loggers$logger_id=="12"] <- "native"
loggers$status[loggers$logger_id=="15"] <- "invaded"
loggers$status[loggers$logger_id=="16"] <- "invaded"
loggers$status[loggers$logger_id=="17"] <- "native"
loggers$status[loggers$logger_id=="19"] <- "invaded"
loggers$status[loggers$logger_id=="20"] <- "invaded"
loggers$status[loggers$logger_id=="21"] <- "native"
loggers$status[loggers$logger_id=="23"] <- "invaded"
loggers$status[loggers$logger_id=="24"] <- "native"

#fixing error because ggplot wouldnt plot date as character
loggers <- loggers %>% 
  mutate(date = as.Date(substr(date_time, 1,10))) 

loggers_hancock <- loggers %>% 
  filter(installation=="hancock")
loggers_riversedge <- loggers %>% 
  filter(installation=="riversedge")
loggers_brown <- loggers %>% 
  filter(installation=="brown")
loggers_silversprings <- loggers %>% 
  filter(installation=="silversprings")
loggers_munson <- loggers %>% 
  filter(installation=="munson")
loggers_peace <- loggers %>% 
  filter(installation=="peace")
loggers_wes <- loggers %>% 
  filter(installation=="wes")

####hancock
loggers_hancock_min <- loggers_hancock %>% 
  group_by(date, installation, plot_id, status, logger_id, days) %>% 
  summarise(daily_min_rh = min(RH))

loggers_hancock_below <- loggers_hancock %>% 
  filter(between(RH, 20, 79.999)) %>%
  group_by(date, installation, plot_id, status, logger_id) %>%
  summarise(obs_below_80 = n(),
            minutes_below_80 = obs_below_80*5,
            hours_below_80 = minutes_below_80/60) %>%
  select(date, installation, status, plot_id, logger_id, hours_below_80)

loggers_hancock_above <- loggers_hancock %>%
  filter(RH >80) %>%
  group_by(date, installation, plot_id, status, logger_id) %>%
  summarise(obs_above_80 = n(),
            minutes_above_80 = obs_above_80*5,
            hours_above_80 = minutes_above_80/60) %>%
  select(date, installation, status, plot_id, logger_id, hours_above_80)

hancock_thresholds_1 <- left_join(loggers_hancock_above, loggers_hancock_below)
hancock_thresholds <- left_join(hancock_thresholds_1, loggers_hancock_min)
hancock_thresholds[is.na(hancock_thresholds)] <- 0

write_csv(hancock_thresholds, "data/processed_data/2019 serdp processed data/logger-sampling/hancock_thresholds.csv")

####riversedge
loggers_riversedge_min <- loggers_riversedge %>% 
  group_by(date, installation, plot_id, status, logger_id, days) %>% 
  summarise(daily_min_rh = min(RH))

loggers_riversedge_below <- loggers_riversedge %>% 
  filter(between(RH, 20, 79.999)) %>%
  group_by(date, installation, plot_id, status, logger_id) %>%
  summarise(obs_below_80 = n(),
            minutes_below_80 = obs_below_80*5,
            hours_below_80 = minutes_below_80/60) %>%
  select(date, installation, status, plot_id, logger_id, hours_below_80)

loggers_riversedge_above <- loggers_riversedge %>%
  filter(RH >80) %>%
  group_by(date, installation, plot_id, status, logger_id) %>%
  summarise(obs_above_80 = n(),
            minutes_above_80 = obs_above_80*5,
            hours_above_80 = minutes_above_80/60) %>%
  select(date, installation, status, plot_id, logger_id, hours_above_80)

riversedge_thresholds_1 <- left_join(loggers_riversedge_above, loggers_riversedge_below)
riversedge_thresholds <- left_join(riversedge_thresholds_1, loggers_riversedge_min)
riversedge_thresholds[is.na(riversedge_thresholds)] <- 0


write_csv(riversedge_thresholds, "data/processed_data/2019 serdp processed data/logger-sampling/riversedge_thresholds.csv")

####brown

loggers_brown_min <- loggers_brown %>% 
  group_by(date, installation, plot_id, status, logger_id, days) %>% 
  summarise(daily_min_rh = min(RH))

loggers_brown_below <- loggers_brown %>% 
  filter(between(RH, 20, 79.999)) %>%
  group_by(date, installation, plot_id, status, logger_id) %>%
  summarise(obs_below_80 = n(),
            minutes_below_80 = obs_below_80*5,
            hours_below_80 = minutes_below_80/60) %>%
  select(date, installation, status, plot_id, logger_id, hours_below_80)

loggers_brown_above <- loggers_brown %>%
  filter(RH >80) %>%
  group_by(date, installation, plot_id, status, logger_id) %>%
  summarise(obs_above_80 = n(),
            minutes_above_80 = obs_above_80*5,
            hours_above_80 = minutes_above_80/60) %>%
  select(date, installation, status, plot_id, logger_id, hours_above_80)

brown_thresholds_1 <- left_join(loggers_brown_above, loggers_brown_below)
brown_thresholds <- left_join(brown_thresholds_1, loggers_brown_min)
brown_thresholds[is.na(brown_thresholds)] <- 0


write_csv(brown_thresholds, "data/processed_data/2019 serdp processed data/logger-sampling/brown_thresholds.csv")

####silversprings

loggers_silversprings_min <- loggers_silversprings %>% 
  group_by(date, installation, plot_id, status, logger_id, days) %>% 
  summarise(daily_min_rh = min(RH))

loggers_silversprings_below <- loggers_silversprings %>% 
  filter(between(RH, 20, 79.999)) %>%
  group_by(date, installation, plot_id, status, logger_id) %>%
  summarise(obs_below_80 = n(),
            minutes_below_80 = obs_below_80*5,
            hours_below_80 = minutes_below_80/60) %>%
  select(date, installation, status, plot_id, logger_id, hours_below_80)

loggers_silversprings_above <- loggers_silversprings %>%
  filter(RH >80) %>%
  group_by(date, installation, plot_id, status, logger_id) %>%
  summarise(obs_above_80 = n(),
            minutes_above_80 = obs_above_80*5,
            hours_above_80 = minutes_above_80/60) %>%
  select(date, installation, status, plot_id, logger_id, hours_above_80)

silversprings_thresholds_1 <- left_join(loggers_silversprings_above, loggers_silversprings_below)
silversprings_thresholds <- left_join(silversprings_thresholds_1, loggers_silversprings_min)
silversprings_thresholds[is.na(silversprings_thresholds)] <- 0


write_csv(silversprings_thresholds, "data/processed_data/2019 serdp processed data/logger-sampling/silversprings_thresholds.csv")

####munson

loggers_munson_min <- loggers_munson %>% 
  group_by(date, installation, plot_id, status, logger_id, days) %>% 
  summarise(daily_min_rh = min(RH))

loggers_munson_below <- loggers_munson %>% 
  filter(between(RH, 20, 79.999)) %>%
  group_by(date, installation, plot_id, status, logger_id) %>%
  summarise(obs_below_80 = n(),
            minutes_below_80 = obs_below_80*5,
            hours_below_80 = minutes_below_80/60) %>%
  select(date, installation, status, plot_id, logger_id, hours_below_80)

loggers_munson_above <- loggers_munson %>%
  filter(RH >80) %>%
  group_by(date, installation, plot_id, status, logger_id) %>%
  summarise(obs_above_80 = n(),
            minutes_above_80 = obs_above_80*5,
            hours_above_80 = minutes_above_80/60) %>%
  select(date, installation, status, plot_id, logger_id, hours_above_80)

munson_thresholds_1 <- left_join(loggers_munson_above, loggers_munson_below)
munson_thresholds <- left_join(munson_thresholds_1, loggers_munson_min)
munson_thresholds[is.na(munson_thresholds)] <- 0


write_csv(munson_thresholds, "data/processed_data/2019 serdp processed data/logger-sampling/munson_thresholds.csv")

####peace

loggers_peace_min <- loggers_peace %>% 
  group_by(date, installation, plot_id, status, logger_id, days) %>% 
  summarise(daily_min_rh = min(RH))

loggers_peace_below <- loggers_peace %>% 
  filter(between(RH, 20, 79.999)) %>%
  group_by(date, installation, plot_id, status, logger_id) %>%
  summarise(obs_below_80 = n(),
            minutes_below_80 = obs_below_80*5,
            hours_below_80 = minutes_below_80/60) %>%
  select(date, installation, status, plot_id, logger_id, hours_below_80)

loggers_peace_above <- loggers_peace %>%
  filter(RH >80) %>%
  group_by(date, installation, plot_id, status, logger_id) %>%
  summarise(obs_above_80 = n(),
            minutes_above_80 = obs_above_80*5,
            hours_above_80 = minutes_above_80/60) %>%
  select(date, installation, status, plot_id, logger_id, hours_above_80)

peace_thresholds_1 <- left_join(loggers_peace_above, loggers_peace_below)
peace_thresholds <- left_join(peace_thresholds_1, loggers_peace_min)
peace_thresholds[is.na(peace_thresholds)] <- 0


write_csv(peace_thresholds, "data/processed_data/2019 serdp processed data/logger-sampling/peace_thresholds.csv")

###wes

loggers_wes_min <- loggers_wes %>% 
  group_by(date, installation, plot_id, status, logger_id, days) %>% 
  summarise(daily_min_rh = min(RH))

loggers_wes_below <- loggers_wes %>% 
  filter(between(RH, 20, 79.999)) %>%
  group_by(date, installation, plot_id, status, logger_id) %>%
  summarise(obs_below_80 = n(),
            minutes_below_80 = obs_below_80*5,
            hours_below_80 = minutes_below_80/60) %>%
  select(date, installation, status, plot_id, logger_id, hours_below_80)

loggers_wes_above <- loggers_wes %>%
  filter(RH >80) %>%
  group_by(date, installation, plot_id, status, logger_id) %>%
  summarise(obs_above_80 = n(),
            minutes_above_80 = obs_above_80*5,
            hours_above_80 = minutes_above_80/60) %>%
  select(date, installation, status, plot_id, logger_id, hours_above_80)

wes_thresholds_1 <- left_join(loggers_wes_above, loggers_wes_below)
wes_thresholds <- left_join(wes_thresholds_1, loggers_wes_min)
wes_thresholds[is.na(wes_thresholds)] <- 0


write_csv(wes_thresholds, "data/processed_data/2019 serdp processed data/logger-sampling/wes_thresholds.csv")

####end processing each indiviudal installation for sep csv's for drew analysis 

