# Weather Data from Experimental Fires

library(readr)
library(plyr)
library(dplyr)
library(ggplot2)

column_names <- c("date_time","air_tempC","wet_bulb_tempC","rel_humidity","barometer_mb","altitude_m","station_pressure_mb","wind_speed_ms","heat_index_C","dew_point_C","dens_altitude_m","crosswind_ms","headwind_ms","magnetic_direction_deg","true_direction_deg","wind_chill_C","ignition_probability")
# piled vs. standing ####

# December 1st weather
wx_dec1 <- read_csv("data/experimental-fire-weather-20171201.csv", skip = 2,
                    col_names = column_names)

wx_dec1$date_time <- wx_dec1$date_time-3600 #correct for time change

wx_dec1 <- select(wx_dec1, -dens_altitude_m, -altitude_m)

wx_dec1$time <- hms::as.hms(sapply(strsplit(as.character(wx_dec1$date_time), " "), "[", 2))

wx_dec1$date <- as.Date(wx_dec1$date_time)

ggplot(wx_dec1, aes(time, air_tempC)) +
      geom_point()

wx_dec1 <- select(wx_dec1, date, time, air_tempC, rel_humidity, wind_speed_ms)

wx_fire43 <- wx_dec1 %>%
      filter(between(time, 51850, 52300)) %>%
      mutate(fire_id = 43)
wx_fire45 <- wx_dec1 %>%
      filter(between(time, 53700, 54200)) %>%
      mutate(fire_id = 45)
wx_fire47 <- wx_dec1 %>%
      filter(between(time, 60500, 61000)) %>%
      mutate(fire_id = 47)
wx_fire49 <- wx_dec1 %>%
      filter(between(time, 61700, 62560)) %>%
      mutate(fire_id = 49)
wx_fire50 <- wx_dec1 %>%
      filter(between(time, 60900, 61250)) %>%
      mutate(fire_id = 50)
wx_fire48 <- wx_dec1 %>%
      filter(between(time, 56030, 56400)) %>%
      mutate(fire_id = 48)
wx_fire46 <- wx_dec1 %>%
      filter(between(time, 53630, 54030)) %>%
      mutate(fire_id = 46)
wx_fire44 <- wx_dec1 %>%
      filter(between(time, 51750, 52100)) %>%
      mutate(fire_id = 44)

# December 4th weather
wx_dec4 <- read_csv("data/experimental-fire-weather-20171204.csv", skip = 11,
         col_names = column_names)

wx_dec4$date_time <- wx_dec4$date_time-3600 #correct for time change

wx_dec4 <- select(wx_dec4, -dens_altitude_m, -altitude_m)

wx_dec4$time <- hms::as.hms(sapply(strsplit(as.character(wx_dec4$date_time), " "), "[", 2))

wx_dec4$date <- as.Date(wx_dec4$date_time)

wx_dec4 <- select(wx_dec4, date, time, air_tempC, rel_humidity, wind_speed_ms)

wx_fire62 <- wx_dec4 %>%
      filter(between(time, 59150, 59450)) %>%
      mutate(fire_id = 62)
wx_fire60 <- wx_dec4 %>%
      filter(between(time, 55750, 56030)) %>%
      mutate(fire_id = 60)
wx_fire58 <- wx_dec4 %>%
      filter(between(time, 53570, 53900)) %>%
      mutate(fire_id = 58)
wx_fire56 <- wx_dec4 %>%
      filter(between(time, 49400, 49700)) %>%
      mutate(fire_id = 56) # 0 observations, could assign fire ID 55 values
wx_fire54 <- wx_dec4 %>%
      filter(between(time, 47780, 48000)) %>%
      mutate(fire_id = 54) # 0 observations, could assign fire ID 53 values
wx_fire52 <- wx_dec4 %>%
      filter(between(time, 45850, 46100)) %>%
      mutate(fire_id = 52)
wx_fire61 <- wx_dec4 %>%
      filter(between(time, 58600, 60100)) %>%
      mutate(fire_id = 61)
wx_fire59 <- wx_dec4 %>%
      filter(between(time, 55500, 56700)) %>%
      mutate(fire_id = 59)
wx_fire57 <- wx_dec4 %>%
      filter(between(time, 53350, 54600)) %>%
      mutate(fire_id = 57)
wx_fire55 <- wx_dec4 %>%
      filter(between(time, 49500, 50300)) %>%
      mutate(fire_id = 55)
wx_fire53 <- wx_dec4 %>%
      filter(between(time, 47350, 47850)) %>%
      mutate(fire_id = 53)
wx_fire51 <- wx_dec4 %>%
      filter(between(time, 45500, 46830)) %>%
      mutate(fire_id = 51)


# December 5th weather
wx_dec5 <- read_csv("data/experimental-fire-weather-20171205.csv", skip = 11,
                    col_names = column_names)

wx_dec5$date_time <- wx_dec5$date_time-3600 #correct for time change

wx_dec5 <- select(wx_dec5, -dens_altitude_m, -altitude_m)

wx_dec5$time <- hms::as.hms(sapply(strsplit(as.character(wx_dec5$date_time), " "), "[", 2))
wx_dec5$date <- as.Date(wx_dec5$date_time)

wx_dec5 <- select(wx_dec5, date, time, air_tempC, rel_humidity, wind_speed_ms)

wx_fire73 <- wx_dec5 %>%
      filter(between(time, 53930, 55060)) %>%
      mutate(fire_id = 73)
wx_fire71 <- wx_dec5 %>%
      filter(between(time, 51000, 52500)) %>%
      mutate(fire_id = 71)
wx_fire69 <- wx_dec5 %>%
      filter(between(time, 48800, 49600)) %>%
      mutate(fire_id = 69)
wx_fire67 <- wx_dec5 %>%
      filter(between(time, 46920, 47520)) %>%
      mutate(fire_id = 67)
wx_fire65 <- wx_dec5 %>%
      filter(between(time, 45400, 45850)) %>%
      mutate(fire_id = 65)
wx_fire63 <- wx_dec5 %>%
      filter(between(time, 43750, 44200)) %>%
      mutate(fire_id = 63)
wx_fire74 <- wx_dec5 %>%
      filter(between(time, 54220, 54430)) %>%
      mutate(fire_id = 74) # 0 observations, could assign fire ID 73 values
wx_fire72 <- wx_dec5 %>%
      filter(between(time, 51650, 52000)) %>%
      mutate(fire_id = 72)
wx_fire70 <- wx_dec5 %>%
      filter(between(time, 49200, 49400)) %>%
      mutate(fire_id = 70) # 0 observations, could assign fire ID 69 values
wx_fire68 <- wx_dec5 %>%
      filter(between(time, 47200, 47420)) %>%
      mutate(fire_id = 68)
wx_fire66 <- wx_dec5 %>%
      filter(between(time, 45600, 45830)) %>%
      mutate(fire_id = 66)
wx_fire64 <- wx_dec5 %>%
      filter(between(time, 44000, 44160)) %>%
      mutate(fire_id = 64)


fire_weather <- rbind(
      wx_fire43, wx_fire44, wx_fire45, wx_fire46, wx_fire47, wx_fire48, wx_fire49, wx_fire50, wx_fire51, wx_fire52, wx_fire53, wx_fire54, wx_fire55, wx_fire56, wx_fire57, wx_fire58, wx_fire59, wx_fire60, wx_fire61, wx_fire62, wx_fire63, wx_fire64, wx_fire65, wx_fire66, wx_fire67, wx_fire68, wx_fire69, wx_fire70, wx_fire71, wx_fire72, wx_fire73, wx_fire74
)

# fire_weather[is.na(fire_weather),]

fire_weather_summary <- fire_weather %>%
      group_by(date, fire_id) %>%
      summarise(n_obs = length(air_tempC),
                avg_air_temp = mean(air_tempC),
                se_air_temp = sd(air_tempC)/sqrt(n_obs),
                avg_rh = mean(rel_humidity),
                se_rh = sd(rel_humidity)/sqrt(n_obs),
                avg_wind_speed = mean(wind_speed_ms),
                se_wind_speed = sd(air_tempC)/sqrt(n_obs))

fire_weather_summary <- rbind(
      fire_weather_summary %>% filter(fire_id==53) %>% mutate(fire_id = 54),
      fire_weather_summary %>% filter(fire_id==55) %>% mutate(fire_id = 56),
      fire_weather_summary %>% filter(fire_id==73) %>% mutate(fire_id = 74),
      fire_weather_summary %>% filter(fire_id==69) %>% mutate(fire_id = 70),
      fire_weather_summary
)
write_csv(fire_weather_summary, "data/piled-vs-standing-fire-wx-summary.csv")

knitr::kable(fire_weather_summary %>% select(-n_obs) %>% arrange(fire_id))

# tail(add_row(ungroup(fire_weather_summary), ))

fw_summ2 <- fire_weather %>%
      group_by(date) %>%
      summarise(n_obs = length(air_tempC),
                avg_temp = mean(air_tempC),
                se_avg_temp = sd(air_tempC)/sqrt(n_obs),
                avg_rh = mean(rel_humidity),
                se_rh = sd(air_tempC)/sqrt(n_obs),
                avg_wind_speed = mean(wind_speed_ms),
                se_wind_speed = sd(air_tempC)/sqrt(n_obs))


ggplot(fire_weather, aes(date, rel_humidity, color = fire_id)) +
      geom_point(position = "jitter") +
      geom_point(data=fw_summ2, aes(date, avg_rh), color = "orange") +
      theme_bw()
