library(plyr)
library(dplyr)
library(readr)
library(ggplot2)

secondary_loggers <- read_csv("data/processed_data/2019 serdp processed data/2019-secondary-logger-sampling.csv")

summary(secondary_loggers)

secondary_loggers <- secondary_loggers %>%
  mutate(date_time = lubridate::ymd_hms(paste(date,time)),
         id = as.character.numeric_version(stringr::str_sub(logger_id, start = 1, end = 2)),
         status = id,
         location = id,
         installation = id) %>%
  filter(date_time < "2019-05-30 06:16:00")

secondary_loggers$status[secondary_loggers$id=="01"] <- "invaded"
secondary_loggers$status[secondary_loggers$id=="12"] <- "invaded"
secondary_loggers$status[secondary_loggers$id=="02"] <- "invaded"
secondary_loggers$status[secondary_loggers$id=="10"] <- "invaded"
secondary_loggers$status[secondary_loggers$id=="03"] <- "invaded"
secondary_loggers$status[secondary_loggers$id=="14"] <- "invaded"
secondary_loggers$status[secondary_loggers$id=="08"] <- "invaded"
secondary_loggers$status[secondary_loggers$id=="15"] <- "invaded"
secondary_loggers$status[secondary_loggers$id=="19"] <- "invaded"
secondary_loggers$status[secondary_loggers$id=="24"] <- "invaded"
secondary_loggers$status[secondary_loggers$id=="20"] <- "invaded"
secondary_loggers$status[secondary_loggers$id=="21"] <- "invaded"

secondary_loggers$status[secondary_loggers$id=="04"] <- "native"
secondary_loggers$status[secondary_loggers$id=="13"] <- "native"
secondary_loggers$status[secondary_loggers$id=="05"] <- "native"
secondary_loggers$status[secondary_loggers$id=="07"] <- "native"
secondary_loggers$status[secondary_loggers$id=="16"] <- "native"
secondary_loggers$status[secondary_loggers$id=="22"] <- "native"
secondary_loggers$status[secondary_loggers$id=="17"] <- "native"
secondary_loggers$status[secondary_loggers$id=="23"] <- "native"
secondary_loggers$status[secondary_loggers$id=="06"] <- "native"
secondary_loggers$status[secondary_loggers$id=="11"] <- "native"

secondary_loggers$location[secondary_loggers$id=="12"] <- "tree"
secondary_loggers$location[secondary_loggers$id=="10"] <- "tree"
secondary_loggers$location[secondary_loggers$id=="14"] <- "tree"
secondary_loggers$location[secondary_loggers$id=="13"] <- "tree"
secondary_loggers$location[secondary_loggers$id=="07"] <- "tree"
secondary_loggers$location[secondary_loggers$id=="11"] <- "tree"
secondary_loggers$location[secondary_loggers$id=="15"] <- "tree"
secondary_loggers$location[secondary_loggers$id=="22"] <- "tree"
secondary_loggers$location[secondary_loggers$id=="17"] <- "tree"
secondary_loggers$location[secondary_loggers$id=="19"] <- "tree"
secondary_loggers$location[secondary_loggers$id=="21"] <- "tree"

secondary_loggers$location[secondary_loggers$id=="01"] <- "center"
secondary_loggers$location[secondary_loggers$id=="02"] <- "center"
secondary_loggers$location[secondary_loggers$id=="03"] <- "center"
secondary_loggers$location[secondary_loggers$id=="04"] <- "center"
secondary_loggers$location[secondary_loggers$id=="05"] <- "center"
secondary_loggers$location[secondary_loggers$id=="06"] <- "center"
secondary_loggers$location[secondary_loggers$id=="08"] <- "center"
secondary_loggers$location[secondary_loggers$id=="16"] <- "center"
secondary_loggers$location[secondary_loggers$id=="23"] <- "center"
secondary_loggers$location[secondary_loggers$id=="24"] <- "center"
secondary_loggers$location[secondary_loggers$id=="20"] <- "center"

secondary_loggers$installation[secondary_loggers$id=="17"] <- "silversprings"
secondary_loggers$installation[secondary_loggers$id=="23"] <- "silversprings"
secondary_loggers$installation[secondary_loggers$id=="19"] <- "silversprings"
secondary_loggers$installation[secondary_loggers$id=="24"] <- "silversprings"

secondary_loggers$installation[secondary_loggers$id=="16"] <- "brown"
secondary_loggers$installation[secondary_loggers$id=="22"] <- "brown"
secondary_loggers$installation[secondary_loggers$id=="20"] <- "brown"
secondary_loggers$installation[secondary_loggers$id=="21"] <- "brown"

secondary_loggers$installation[secondary_loggers$id=="02"] <- "peaceriver"
secondary_loggers$installation[secondary_loggers$id=="10"] <- "peaceriver"
secondary_loggers$installation[secondary_loggers$id=="03"] <- "peaceriver"
secondary_loggers$installation[secondary_loggers$id=="14"] <- "peaceriver"
secondary_loggers$installation[secondary_loggers$id=="05"] <- "peaceriver"
secondary_loggers$installation[secondary_loggers$id=="07"] <- "peaceriver"

secondary_loggers$installation[secondary_loggers$id=="01"] <- "hancock"
secondary_loggers$installation[secondary_loggers$id=="12"] <- "hancock"
secondary_loggers$installation[secondary_loggers$id=="04"] <- "hancock"
secondary_loggers$installation[secondary_loggers$id=="13"] <- "hancock"
secondary_loggers$installation[secondary_loggers$id=="06"] <- "hancock"
secondary_loggers$installation[secondary_loggers$id=="11"] <- "hancock"
secondary_loggers$installation[secondary_loggers$id=="08"] <- "hancock"
secondary_loggers$installation[secondary_loggers$id=="15"] <- "hancock"

#View(secondary_loggers)

secondary_loggers <- secondary_loggers %>%
  filter(RH >= 15 %>%
  group_by(date, status, id, location, installation) %>%
  summarise(avg_daily_tempC = mean(tempC),
            max_tempC = max(tempC),
            min_tempC = min(tempC),
            avg_daily_RH = mean(RH),
            avg_max_rh = max(RH),
            avg_min_rh = min(RH)
  ) %>%
  ungroup(.) %>%
  group_by(date, status, days) %>%
  summarise(avg_daily_tempC = mean(avg_daily_tempC),
            avg_dailymin_tempC = mean(min_tempC),
            avg_dailymax_tempC = mean(max_tempC),
            avg_daily_RH = mean(avg_daily_RH),
            avg_dailymax_rh = mean(avg_max_rh),
            avg_dailymin_rh = mean(avg_min_rh)
  ))


write_csv(secondary_loggers, "data/processed_data/2019-secondary-logger-trimmed.csv")

trimmed_loggers <- read_csv("data/processed_data/2019 serdp processed data/2019-secondary-logger-trimmed.csv")

# define consistent them
def_theme <- theme(legend.title = element_blank(),
                   legend.text = element_text(size = 12),
                   legend.position = "top",
                   axis.text = element_text(size = 12),
                   axis.title = element_text(size = 16),
                   plot.title = element_text(size = 28),
                   strip.background = element_blank(),
                   panel.grid = element_blank(),
                   panel.background = element_blank()
                   )
invasion_color <- scale_color_manual(values = c("red","deepskyblue"))
invasion_fill <- scale_color_manual(values = c("red","deepskyblue"))

max_temp_all_time <- ggplot(trimmed_loggers, aes(date_time, tempC, color = status)) +
  geom_smooth(aes(fill = status, color = status), se = T, method = "lm",
              alpha = .2) +
  geom_point(aes(color = status), alpha = .1) +
  #geom_hline(yintercept = 35, linetype = "solid", color = "gray") +
  #geom_vline(xintercept = vline_days, linetype = "dashed", alpha=.2) +
  # geom_point(data = trimmed_loggers, aes(y = tempC, color = status)) +
  invasion_color +
  invasion_fill +
  ylab("Average daily maximum temperature °C") +
  xlab("Days") +
  #scale_y_continuous(breaks = c(15,20,25,30,35,40,45)) +
  #scale_x_continuous(limits = c(0, 306)) +
  #scale_x_continuous(breaks = c(0, 50, 100, 150, 200, 250, 300)) +
  #expand_limits(x = 0, y = 0) +
  #scale_x_continuous(expand = c(0,0), limits = c(0,336), breaks = c(0, 50, 100, 150, 200, 250, 300, 350)) +
  #scale_y_continuous(expand = c(0,0), limits = c(5, 44), breaks = c(5, 10, 15, 20, 25, 30, 35, 40)) +
  #guides(fill=FALSE, color=FALSE) +
  theme_classic() +
  def_theme +
  NULL

min_rh_all_time <- ggplot(trimmed_loggers, aes(date_time, RH, color = status)) +
  geom_smooth(aes(fill = status, color = status), se = T, method = "lm",
              alpha = .2) +
  geom_point(aes(color = status), alpha = .1) +
  #geom_hline(yintercept = 80, linetype = "solid", color = "gray") +
  #geom_vline(xintercept = vline_days, linetype = "dashed", alpha=.2) +
  # geom_point(data = trimmed_loggers, aes(y = RH, color = status)) +
  invasion_color +
  invasion_fill +
  ylab("Average daily minimum %RH") +
  xlab("Days") +
  #expand_limits(x = 0, y = 0) +
  #scale_y_continuous(expand = c(0,0), limits = c(30, 102), breaks = c(30, 40, 50,60,70,80,90,100)) +
  #scale_x_continuous(expand = c(0,0), limits = c(0, 350), breaks = c(0, 50, 100, 150, 200, 250, 300, 350)) +
  #guides(fill=FALSE, color=FALSE) +
  def_theme +
  NULL

trimmed_loggers_peaceriver_tree <- trimmed_loggers %>%
  filter(installation == "peaceriver") %>%
  filter(location =="tree")
trimmed_loggers_peaceriver_center <- trimmed_loggers %>%
  filter(installation == "peaceriver") %>%
  filter(location =="center")

test1 <- ggplot(trimmed_loggers_peaceriver_tree, aes(date_time, RH, color = status)) +
  geom_smooth(aes(fill = status, color = status), se = T, method = "lm",
alpha = .2) +
  geom_point(data = trimmed_loggers_peaceriver_tree, aes(y = RH, color = status)) +
  invasion_color +
  ylab("tree") +
  invasion_fill +
  def_theme +
  NULL

test2 <- ggplot(trimmed_loggers_peaceriver_center, aes(date_time, RH, color = status)) +
  geom_smooth(aes(fill = status, color = status), se = T, method = "lm",
              alpha = .2) +
  geom_point(data = trimmed_loggers_peaceriver_center, aes(y = RH, color = status)) +
  ylab("center") +
  invasion_color +
  invasion_fill +
  def_theme +
  NULL


test3 <- cowplot::plot_grid(test1, test2, ncol = 2)



# brown testing

trimmed_loggers_brown_tree <- trimmed_loggers %>%
  filter(installation == "brown") %>%
  filter(location =="tree")
trimmed_loggers_brown_center <- trimmed_loggers %>%
  filter(installation == "brown") %>%
  filter(location =="center")


test4 <- ggplot(trimmed_loggers_brown_tree, aes(date_time, RH, color = status)) +
  geom_smooth(aes(fill = status, color = status), se = T, method = "lm",
              alpha = .2) +
  geom_point(data = trimmed_loggers_brown_tree, aes(y = RH, color = status)) +
  invasion_color +
  ylab("tree") +
  invasion_fill +
  def_theme +
  NULL

test5 <- ggplot(trimmed_loggers_brown_center, aes(date_time, RH, color = status)) +
  geom_smooth(aes(fill = status, color = status), se = T, method = "lm",
              alpha = .2) +
  geom_point(data = trimmed_loggers_brown_center, aes(y = RH, color = status)) +
  ylab("center") +
  invasion_color +
  invasion_fill +
  def_theme +
  NULL

test6 <- cowplot::plot_grid(test4, test5, ncol = 2)

## hancock

trimmed_loggers_hancock_tree <- trimmed_loggers %>%
  filter(installation == "hancock") %>%
  filter(location =="tree")
trimmed_loggers_hancock_center <- trimmed_loggers %>%
  filter(installation == "hancock") %>%
  filter(location =="center")


test7 <- ggplot(trimmed_loggers_hancock_tree, aes(date_time, RH, color = status)) +
  geom_smooth(aes(fill = status, color = status), se = T, method = "lm",
              alpha = .2) +
  geom_point(data = trimmed_loggers_hancock_tree, aes(y = RH, color = status)) +
  invasion_color +
  ylab("tree") +
  invasion_fill +
  def_theme +
  NULL

test8 <- ggplot(trimmed_loggers_hancock_center, aes(date_time, RH, color = status)) +
  geom_smooth(aes(fill = status, color = status), se = T, method = "lm",
              alpha = .2) +
  geom_point(data = trimmed_loggers_hancock_center, aes(y = RH, color = status)) +
  ylab("center") +
  invasion_color +
  invasion_fill +
  def_theme +
  NULL

test9 <- cowplot::plot_grid(test7, test8, ncol = 2)



