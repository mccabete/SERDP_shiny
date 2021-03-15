# Fetching Daymet Climate Data
# https://daymet.ornl.gov/

# install.packages("daymetr")

# Seven parameters:
## minimum and maximum temperature - tmin & tmax (ºC)
## precipitation - prcp (mm/day)
## shortwave radiation - srad (W/m2)
## vapor pressure - vp (Pa)
## snow water equivalent - swe (kg/m2) ## probably not applicable for our area
## day length - dayl (s/day)

library(daymetr)

## 2017 and 2018 plots
pvisit_locations <- readr::read_csv("data/processed_data/plot_visit_data.csv")

p_locations <- pvisit_locations %>%
      dplyr::select(plot_id, lat, lon) %>%
      dplyr::filter(!is.na(lon), !duplicated(plot_id)) %>%
      arrange(plot_id)
readr::write_csv(p_locations, "data/plot_locations_daymet.csv")

df_batch <- download_daymet_batch(
      file_location = "data/plot_locations_daymet.csv",
      start = 1980, end = 2018,
      internal = TRUE, simplify = TRUE
      )
unique(df_batch$measurement)

## Export download of raw Daymet data

write_csv(df_batch, "data/raw_data/daymet_data_2017_2018_plots.csv")


# https://cfss.uchicago.edu/notes/simplify-nested-lists/
# library(tidyverse)
# library(repurrrsive)
#
# str(df_batch, list.len = 3)
#
# map(df_batch[1:4],"site")# returns list
#
# map_chr(df_batch[1:4],"site")# returns vector, elements all of same type
#
# df_batch[[1]][c("site", "altitude", "latitude")]
#
# x <- map(df_batch, `[`, c("site", "altitude", "data"))# same as x2
# x2 <- map(df_batch, magrittr::extract, c("site","altitude","data"))
#
# x2 %>% map_chr(c(1))
# x2 %>% map_dbl(c("altitude"))
# x2 %>% map_df(c("data"))
#
# plot_names <- x2 %>% map_chr(c(1))
# x3 <- x2 %>%
#       set_names(plot_names) %>%
#       enframe("plot_id", "x2")
# # str(x3[1:3,])
# x3 %>%
#       mutate(dat = x2 %>%
#                    map(~ .x %>%
#                              map_df(extract, c("site","altitude","data")))
#              ) %>%
#       select(-x2) %>%
#       tidyr::unnest()
#
# unnest(x3)
#
# tidy_df <- df_batch %>%
#       lapply(FUN = unlist) %>%
#       as_tibble()
