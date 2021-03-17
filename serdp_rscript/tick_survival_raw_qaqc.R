#### Script for doing QA/QC on tick survival raw tick data numbers ####

library(plyr)
library(tidyverse)
# library(dplyr)
# library(readr)
library(stringi)
library(stringr)
library(ggplot2)
library(cowplot)
# install.packages("cowplot")

#+ load tick survival data ####

tick_survival <- read_csv("data/raw_data/tick_survival_assay/TickSurvivaldata_CleanedVersion.csv")

summary(tick_survival)

unique(tick_survival$Date)

names(tick_survival)

unique(tick_survival$Invaded)

tick_survival$Date <- as.Date.character(as.integer(tick_survival$Date), format = "%Y%m%d")

tick_survival <- tick_survival %>%
  mutate(days = julian(Date, origin = as.Date("2018-06-21")))

colnames(tick_survival) <- c("LocationID", "Tag", "Invaded", "Date", "JulianDay", "nymphs_alive", "nymphs_dead", "nymph_survival", "adult_F_alive", "adult_F_dead", "adult_F_survival", "adult_M_alive", "adult_M_dead", "adult_M_survival", "Avg CC", "NOTES", "days")

# range(tick_survival$days)

tick_survival_long <- tick_survival %>%
  select(LocationID:JulianDay, days, nymph_survival, adult_F_survival, adult_M_survival) %>%
  gather(., key = life_stage, value = survival, -LocationID:-days) %>%
  mutate(Invaded = ifelse(Invaded == "Yes", "invaded", "native"),
         sex = case_when(
               str_detect(life_stage, "adult_M") ~ "male",
               str_detect(life_stage, "adult_F") ~ "female",
               str_detect(life_stage, "nymph") ~ "nymph"
         ),
         life_stage = case_when(
               str_detect(life_stage, "adult") ~ "adult",
               str_detect(life_stage, "nymph") ~ "nymph"
         ))

alive_long <- tick_survival %>%
      select(LocationID:JulianDay, days, nymphs_alive, adult_F_alive, adult_M_alive) %>%
      gather(., key = life_stage, value = alive, -LocationID:-days) %>%
      mutate(Invaded = ifelse(Invaded == "Yes", "invaded", "native"),
             sex = case_when(
                   str_detect(life_stage, "adult_M") ~ "male",
                   str_detect(life_stage, "adult_F") ~ "female",
                   str_detect(life_stage, "nymph") ~ "nymph"
             ),
             life_stage = case_when(
                   str_detect(life_stage, "adult") ~ "adult",
                   str_detect(life_stage, "nymph") ~ "nymph"
             ))

dead_long <- tick_survival %>%
      select(LocationID:JulianDay, days, nymphs_dead, adult_F_dead, adult_M_dead) %>%
      gather(., key = life_stage, value = dead, -LocationID:-days) %>%
      mutate(Invaded = ifelse(Invaded == "Yes", "invaded", "native"),
             sex = case_when(
                   str_detect(life_stage, "adult_M") ~ "male",
                   str_detect(life_stage, "adult_F") ~ "female",
                   str_detect(life_stage, "nymph") ~ "nymph"
             ),
             life_stage = case_when(
                   str_detect(life_stage, "adult") ~ "adult",
                   str_detect(life_stage, "nymph") ~ "nymph"
             ))
tick_survival_long <- join_all(list(alive_long,dead_long,tick_survival_long))
summary(tick_survival_long)

filter(tick_survival_long, life_stage == "adult", dead >5)
# unique(tick_survival_long$life_stage)
# unique(tick_survival_long$sex)

id_update <- data.frame(LocationID = unique(tick_survival$LocationID), logger_id = c(seq(1,24,1)))

tick_survival_long <- left_join(tick_survival_long,
                                id_update)

tick_survival <- left_join(tick_survival, id_update)

#### Merging all adults to one ####

tick_survival <- tick_survival %>%
  mutate(total_adults_alive = adult_F_alive + adult_M_alive,
         total_adults_dead = adult_F_dead + adult_M_dead,
         total_adult_survival = total_adults_alive/(total_adults_alive + total_adults_dead))

tick_survival <- tick_survival %>%
  mutate(Invaded = ifelse(Invaded == "Yes", "invaded", "native"))

## adding visit number, ADD NEW VISITS EACH TIME

add_visit_number <- data.frame(days = unique(tick_survival$days), visit_number= as.double(seq(1,n_distinct(tick_survival$days),1)))

tick_survival <- left_join(tick_survival, add_visit_number)
tick_survival_long <- left_join(tick_survival_long, add_visit_number)

# head(tick_survival_long)

tick_survival_long <- tick_survival_long %>%
      select(LocationID:life_stage, sex, alive:visit_number)

write_csv(tick_survival_long, "data/processed_data/tick_survival_long.csv")

# View(tick_survival)

tick_survival_grouped <- tick_survival %>%
  group_by(visit_number, Invaded, days) %>%
  summarise(avg_nymph_survival = mean(nymph_survival),
            sd_nymph_survival = sd(nymph_survival),
            avg_adult_survival = mean(total_adult_survival),
            sd_adult_survival = sd(total_adult_survival))

# View(tick_survival_grouped)

# ggplot(tick_survival_grouped, aes(days, avg_nymph_survival, color = Invaded)) +
#   geom_smooth(aes(fill = Invaded), show.legend = F, alpha = .2) +
#   geom_point() +
#   geom_errorbar(aes(ymin = avg_nymph_survival - sd_nymph_survival, ymax = avg_nymph_survival + sd_nymph_survival, color = Invaded), width = .1) +
#   invasion_color +
#   invasion_fill +
#   theme_bw() +
#   def_theme +
#   NULL
#
# ggplot(tick_survival_grouped, aes(days, avg_adult_survival, color = Invaded)) +
#   geom_smooth(aes(fill = Invaded, color = Invaded), se = T, alpha = .2) +
#   geom_point() +
#   invasion_color +
#   invasion_fill +
#   def_theme +
#   NULL

# unique(tick_survival$days)
