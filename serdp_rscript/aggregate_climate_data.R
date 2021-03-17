## Aggregate Climate Data

# Seven Daymet parameters:
## https://daac.ornl.gov/DAYMET/guides/Daymet_V3_CFMosaics.html

## tmin & tmax (ºC) - Daily min/max 2-m air temperature in Celsius.

## prcp (mm/day) - Daily total precipitation in mm per day, sum of all forms converted to water-equivalent. Precipitation occurrence on any given day may be ascertained.

## srad (W/m2) - Incident shortwave radiation flux density in watts per square meter, taken as an average over the daylight period of the day. Note: Daily total radiation (MJ/m2/day) can be calculated as follows: ((srad (W/m2) * dayl (s/day)) / l,000,000)

## vp (Pa) - Water vapor pressure in pascals. Daily average partial pressure of water vapor.

## swe (kg/m2) - snow water equivalent ## probably not applicable for us

## dayl (s/day) - Duration of the daylight period in seconds per day. This calculation is based on the period of the day during which the sun is above a hypothetical flat horizon

library(tidyverse)

pvisit_locations <- read_csv("data/processed_data/plot_visit_data.csv")
daymet_data <- read_csv("data/raw_data/daymet_data_2017_2018_plots.csv")

daymet_data <- left_join(daymet_data, select(pvisit_locations, inst_name, plot_id, visit_date),
                      by = c("site"="plot_id")) %>%
      mutate(date = as.Date(paste(year, yday, sep = "-"), "%Y-%j"))

daymet_data_2016_2018 <- filter(daymet_data, year >= 2016)
write_csv(daymet_data_2016_2018, "data/raw_data/plot_locs_daymet_daily_2016_2018.csv")

CV <- function(mean, sd){
      (sd/mean)*100
}

## Aggregate 30-years of climate data ####
## This aggregates to annual average values (1980-2018) for each plot visit
## Total is also calculated to be used ONLY for precipitation
## Exclude day length and snow-water equivalent from aggregated data



y30 <- lubridate::as.period(30, unit = "year")
agg_30yr_annual_daymet <- daymet_data %>%
      filter(measurement!="swe..kg.m.2.", measurement!="dayl..s.") %>%
      rename(plot_id = site) %>%
      filter(date <= visit_date, date >= visit_date-y30) %>% # use only pre-visit data
      group_by(inst_name, plot_id, year, measurement, visit_date) %>%
      summarise(
            # max_value = max(value),## I don't think these mean anything
            # min_value = min(value),
            nobs = length(measurement),
            avg_value = mean(value),
            sd_value = sd(value),
            tot_value = sum(value),## only useful for prcp as total annual
            cv_value = CV(avg_value, sd_value),
      )

agg_30yr_fire_days <- daymet_data %>%
      filter(measurement=="prcp..mm.day.", value < 6.35,
             date <= visit_date, date >= visit_date-y30) %>%
      rename(plot_id = site) %>%
      group_by(inst_name, plot_id, year, visit_date) %>%
      filter(date <= visit_date, date >= visit_date-y30) %>% # use only pre-visit data
      summarise(fire_days = length(value)) %>%
      ungroup(.) %>%
      group_by(inst_name, plot_id, visit_date) %>%
      summarise(avg_30yr_fire_days = mean(fire_days),
                cv_30yr_fire_days = CV(mean(fire_days), sd(fire_days)))

# unique(agg_daymet$measurement)

## Plot 30 years of annual averages
agg_30yr_annual_daymet %>%
      filter(measurement!="prcp..mm.day.") %>%
      ggplot(aes(inst_name, avg_value)) +
      geom_boxplot(aes(color = inst_name), outlier.shape = NA) +
      geom_point(aes(color = inst_name), size = .5, alpha = .2, position = "jitter") +
      facet_wrap( ~ measurement, ncol = 2, scales = "free_y")

agg_30yr_annual_daymet %>%
      filter(measurement=="prcp..mm.day.") %>%
      ggplot(aes(inst_name, tot_value)) +
      geom_boxplot(aes(color = inst_name), outlier.shape = NA) +
      geom_point(aes(color = inst_name), size = .5, alpha = .2, position = "jitter") +
      facet_wrap( ~ measurement, ncol = 2, scales = "free_y") +
      ylab("Total annual precipitation 1980-2018 (mm)")

## Plot the 30-year averages of the data summarized to annual
## This plot shows that the 30-year averages of Daymet data are very similar within an installation but notably different between installations. Ft. Benning and Camp Blanding show the most variation within an installation.
prcp_30yr_plot <- agg_30yr_annual_daymet %>%
      filter(measurement == "prcp..mm.day.") %>%
      group_by(inst_name, plot_id, visit_date, measurement) %>%
      summarise(avg_30yr= mean(tot_value))

plot_labels <- data.frame(
      measurement = unique(agg_30yr_annual_daymet$measurement)
)

agg_30yr_annual_daymet %>%
      filter(measurement != "dayl..s.", measurement!="prcp..mm.day.") %>%
      group_by(inst_name, year, measurement) %>%
      summarise(avg_30yr= mean(avg_value)) %>%
      mutate(measurement = case_when(
            measurement=="srad..W.m.2." ~ "Solar~radiation~(W~m^{-2})",
            measurement=="tmax..deg.c." ~ "Maximum~temperature~(ºC)",
            measurement=="tmin..deg.c." ~ "Minimum~temperature~(ºC)",
            measurement=="vp..Pa." ~ "Vapor~pressure~(Pascals)"
                  )
            ) %>%
      ggplot(aes(inst_name, avg_30yr)) +
      geom_boxplot(outlier.shape = NA) +
      geom_point(position = "jitter", size = 1, alpha = .3) +
      geom_boxplot(data = prcp_30yr_plot %>%
                         mutate(measurement="Precipitation (mm)"),
                   aes(inst_name, avg_30yr),
                   outlier.shape = NA) +
      geom_point(data = prcp_30yr_plot %>%
                       mutate(measurement="Precipitation (mm)"),
                 aes(inst_name, avg_30yr),
                 position = "jitter", size = 1, alpha = .2) +
      facet_wrap( ~ measurement, ncol = 1, scales = "free_y", labeller = label_parsed) +
      theme_bw() +
      theme(strip.background = element_blank(),
            panel.grid = element_blank(),
            axis.title.y = element_text(size = 14),
            strip.text = element_text(size = 14),
            axis.text.x.bottom = element_text(angle = -75, size = 12)
            ) +
      ylab("Values 1980-2018") +
      xlab("") +
      # annotate("text", x = 1, y = 95, label = c("a","b","c","d","e"))
      # annotate("text", x = .8, y = 290, label = paste("R^2: ", round(fh_mod_summary$r.squared, 2)))
      NULL
ggsave("figures/daymet_installation_summary.png", width = 5,height = 8)


monthly_avg <- daymet_data %>%
      rename(plot_id = site) %>%
      # filter(measurement != "swe..kg.m.2.") %>%
      group_by(inst_name, visit_date, year, measurement, lubridate::month(date)) %>%
      filter(date <= visit_date) %>%
      summarise(avg_value = mean(value),
                tot_value = sum(value)) %>%
      rename(month = `lubridate::month(date)`) %>%
      mutate(month = ifelse(month>9, month, paste(0,month,sep = ""))) %>%
      mutate(date = lubridate::ymd(paste(year, month, "01", sep = "-"))) %>%
      select(inst_name:measurement, avg_value, date)

monthly_avg %>%
      filter(measurement != "swe..kg.m.2.", inst_name %in% c("Avon Park AFR", "Eglin AFB","Fort Jackson")) %>%
      ggplot(aes(date, avg_value)) +
      geom_line(aes(color = inst_name), alpha = .7) +
      facet_wrap(~ measurement, scales = "free_y") +
      scale_color_viridis_d() +
      theme_classic()

## Make dataframe of 30-year annual averages ####
agg_30yr_plot_daymet <- agg_30yr_annual_daymet %>%
      filter(measurement != "prcp..mm.day.") %>%
      group_by(inst_name, plot_id, visit_date, measurement) %>%
      summarise(avg_30yr = mean(avg_value),
                sd_30yr = sd(avg_value),
                cv_30yr = CV(avg_30yr, sd_30yr)
                ) %>%
      pivot_wider(names_from = measurement, values_from = c(avg_30yr, sd_30yr, cv_30yr))

agg_30yr_plot_daymet <- left_join(
      agg_30yr_plot_daymet,
      agg_30yr_annual_daymet %>%
      filter(measurement == "prcp..mm.day.") %>%
      group_by(inst_name, plot_id, visit_date, measurement) %>%
      summarise(avg_30yr = mean(tot_value),
                sd_30yr = sd(tot_value),
                cv_30yr = CV(avg_30yr, sd_30yr)) %>%
      pivot_wider(names_from = measurement, values_from = c(avg_30yr, sd_30yr, cv_30yr))
)

agg_30yr_plot_daymet <- left_join(
      agg_30yr_plot_daymet,
      agg_30yr_fire_days
)

ggplot(agg_30yr_plot_daymet, aes(inst_name, avg_30yr_tmax..deg.c.)) +
      geom_boxplot(outlier.shape = NA) +
      geom_point(position = "jitter", alpha = .5)


inst_avgs <- agg_30yr_annual_daymet %>%
      filter(measurement %in% c("tmax..deg.c.", "tmin..deg.c.", "vp..Pa.")) %>%
      group_by(inst_name, measurement) %>%
      summarise(avg_30yr = mean(avg_value),
                sd_30yr = sd(avg_value),
                cv_30yr = CV(avg_30yr, sd_30yr)
      ) %>%
      pivot_wider(names_from = measurement, values_from = c(avg_30yr, sd_30yr, cv_30yr)) %>%
      left_join(.,
                agg_30yr_annual_daymet %>%
      filter(measurement=="prcp..mm.day.") %>%
      group_by(inst_name, measurement) %>%
      summarise(avg_30yr = mean(avg_value),
                sd_30yr = sd(avg_value),
                cv_30yr = CV(avg_30yr, sd_30yr)
      ) %>%
      pivot_wider(names_from = measurement, values_from = c(avg_30yr, sd_30yr, cv_30yr))
      ) %>%
      select(inst_name, contains("tmax"),contains("tmin"),contains("prcp"),contains("vp"))

knitr::kable(inst_avgs)

write_csv(inst_avgs, "data/processed_data/daymet_30yr_avgs_installation_level.csv")

## 1-year of climate data ####

y1 <- lubridate::as.period(1, unit = "year")

## 1 year averages
agg_1yr_annual_daymet <- daymet_data_2016_2018 %>%
      filter(measurement!="swe..kg.m.2.", measurement!="dayl..s.") %>%
      # mutate(date = as.Date(paste(year, yday, sep = "-"), "%Y-%j")) %>%
      rename(plot_id = site) %>%
      group_by(inst_name, plot_id, measurement, visit_date) %>%
      filter(date >= visit_date-y1, date <= visit_date) %>% # use only 1-year pre-visit data
      summarise(
            # max_value = max(value),## I don't think these mean anything
            # min_value = min(value),
            # nobs = length(measurement),
            avg_1yr = mean(value),
            sd_1yr = sd(value),
            tot_1yr = sum(value),## only useful for prcp as total annual
            cv_1yr = CV(avg_1yr, sd_1yr)
      ) %>%
      ungroup(.)

agg_1yr_fire_days <- daymet_data_2016_2018 %>%
      filter(date >= visit_date-y1, date <= visit_date,
             measurement=="prcp..mm.day.", value < 6.35) %>%
      rename(plot_id = site) %>%
      group_by(inst_name, plot_id, visit_date) %>%
      filter(date > visit_date-y15) %>% # use only pre-visit data
      summarise(fire_days_1yr = length(value)) %>%
      ungroup(.)

agg_1yr_plot_daymet <- agg_1yr_annual_daymet %>%
      filter(measurement != "prcp..mm.day.") %>%
      select(-tot_1yr) %>%
      # ungroup(.) %>%
      # group_by(inst_name, plot_id, visit_date, measurement) %>%
      pivot_wider(names_from = measurement,
                  values_from = c(avg_1yr, sd_1yr, cv_1yr))

agg_1yr_plot_daymet <- left_join(
      agg_1yr_plot_daymet,
      agg_1yr_annual_daymet %>%
            filter(measurement == "prcp..mm.day.") %>%
            # select(tot_1yr) %>%
            pivot_wider(names_from = measurement,
                        values_from = c(tot_1yr, avg_1yr, sd_1yr, cv_1yr))
)

agg_1yr_plot_daymet <- left_join(
      agg_1yr_plot_daymet,
      agg_1yr_fire_days
)

write_csv(agg_1yr_plot_daymet, "data/processed_data/daymet_1yr_avgs_plots.csv")


## 15 year climate data ####
y15 <- lubridate::as.period(15, unit = "year")
agg_15yr_annual_daymet <- daymet_data %>%
      filter(measurement!="swe..kg.m.2.", measurement!="dayl..s.") %>%
      # mutate(date = as.Date(paste(year, yday, sep = "-"), "%Y-%j")) %>%
      rename(plot_id = site) %>%
      group_by(inst_name, plot_id, year, measurement, visit_date) %>%
      filter(date >= visit_date-y15, date <= visit_date) %>% # use only pre-visit data
      summarise(
            # max_value = max(value),## I don't think these mean anything
            # min_value = min(value),
            avg_value = mean(value),
            sd_value = sd(value),
            tot_value = sum(value),## only useful for prcp as total annual
            cv_value = CV(avg_value, sd_value)

      )

agg_15yr_fire_days <- daymet_data %>%
      filter(measurement=="prcp..mm.day.", value < 6.35) %>%
      rename(plot_id = site) %>%
      group_by(inst_name, plot_id, year, visit_date) %>%
      filter(date >= visit_date-y15, date <= visit_date) %>% # use only pre-visit data
      summarise(fire_days = length(value)) %>%
      ungroup(.) %>%
      group_by(inst_name, plot_id, visit_date) %>%
      summarise(avg_15yr_fire_days = mean(fire_days),
                cv_15yr_fire_days = CV(mean(fire_days), sd(fire_days)))

agg_15yr_plot_daymet <- agg_15yr_annual_daymet %>%
      filter(measurement != "prcp..mm.day.") %>%
      group_by(inst_name, plot_id, visit_date, measurement) %>%
      summarise(avg_15yr = mean(avg_value),
                sd_15yr = sd(avg_value),
                cv_15yr = CV(avg_15yr, sd_15yr)
      ) %>%
      pivot_wider(names_from = measurement, values_from = c(avg_15yr, sd_15yr, cv_15yr))

agg_15yr_plot_daymet <- left_join(
      agg_15yr_plot_daymet,
      agg_15yr_annual_daymet %>%
            filter(measurement == "prcp..mm.day.") %>%
            group_by(inst_name, plot_id, visit_date, measurement) %>%
            summarise(avg_15yr = mean(tot_value),
                      sd_15yr = sd(tot_value),
                      cv_15yr = CV(avg_15yr, sd_15yr)) %>%
            pivot_wider(names_from = measurement, values_from = c(avg_15yr, sd_15yr, cv_15yr))
)

agg_15yr_plot_daymet <- left_join(agg_15yr_plot_daymet, agg_15yr_fire_days)

write_csv(agg_15yr_plot_daymet, "data/processed_data/daymet_15yr_avgs_plots.csv")


## 6-month climate data ####
daymet_data <- read_csv("data/raw_data/plot_locs_daymet_daily_2016_2018.csv")

m6 <- lubridate::as.period(183, unit = "days")

agg_6mo_daymet <- daymet_data %>%
      filter(measurement!="swe..kg.m.2.", measurement!="dayl..s.") %>%
      # mutate(date = as.Date(paste(year, yday, sep = "-"), "%Y-%j")) %>%
      rename(plot_id = site) %>%
      group_by(inst_name, plot_id, measurement, visit_date) %>%
      filter(date >= visit_date-m6, date <= visit_date) %>% # use only pre-visit data
      summarise(
            # max_value = max(value),## I don't think these mean anything
            # min_value = min(value),
            avg_6mo = mean(value),
            sd_6mo = sd(value),
            tot_6mo = sum(value),## only useful for prcp as total annual
            cv_6mo = CV(avg_6mo, sd_6mo)

      ) %>%
      ungroup(.)

agg_6mo_fire_days <- daymet_data %>%
      filter(measurement=="prcp..mm.day.", value < 6.35) %>%
      rename(plot_id = site) %>%
      group_by(inst_name, plot_id, visit_date) %>%
      filter(date >= visit_date-m6, date <= visit_date) %>% # use only pre-visit data
      summarise(fire_days_6mo = length(value)) %>%
      ungroup(.)


agg_6mo_plot_daymet <- agg_6mo_daymet %>%
      filter(measurement != "prcp..mm.day.") %>%
      select(-tot_6mo) %>%
      # ungroup(.) %>%
      # group_by(inst_name, plot_id, visit_date, measurement) %>%
      pivot_wider(names_from = measurement,
                  values_from = c(avg_6mo, sd_6mo, cv_6mo))


agg_6mo_plot_daymet <- left_join(
      agg_6mo_plot_daymet,
      agg_6mo_daymet %>%
            filter(measurement == "prcp..mm.day.") %>%
            # select(tot_1yr) %>%
            pivot_wider(names_from = measurement,
                        values_from = c(tot_6mo, avg_6mo, sd_6mo, cv_6mo))
)

agg_6mo_plot_daymet <- left_join(agg_6mo_plot_daymet, agg_6mo_fire_days)
write_csv(agg_6mo_plot_daymet, "data/processed_data/aggregated_6mo_plot_daymet.csv")

## Join all climate data ####
agg_plot_daymet <- left_join(agg_15yr_plot_daymet, agg_30yr_plot_daymet) %>%
      left_join(., agg_1yr_plot_daymet) %>%
      left_join(., agg_6mo_plot_daymet)
agg_plot_daymet <- left_join(agg_plot_daymet, agg_15yr_fire_days)


## Export all plot-level aggregates of climate data ####
write_csv(agg_plot_daymet, "data/processed_data/aggregated_plot_daymet_data.csv")
