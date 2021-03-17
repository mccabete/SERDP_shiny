# Tick survival experiment figures

# source("R_scripts/tick_survival_raw_qaqc.R")
# source("R_scripts/temp-rh-qaqc.R")# This can take a long time (big dataframes)


# load packages ####
library(plyr)
library(tidyverse)
# library(dplyr)
# library(readr)
# library(stringi)
# library(stringr)
library(ggplot2)
library(cowplot)

# load data ####
temp_rh_data_grouped <- read_csv("data/processed_data/tick_temp_rh_grouped.csv")
tick_survival <- read_csv("data/processed_data/tick_survival_long.csv")
#tail(temp_rh_data_grouped)
#tail(tick_survival_long)

# define variables ####

# define consistent them
def_theme <- theme(legend.title = element_blank(),
                   legend.text = element_text(size = 12),
                   legend.position = "top",
                   axis.text = element_text(size = 12),
                   axis.title = element_text(size = 16),
                   plot.title = element_text(size = 28),
                   strip.background = element_blank(),
                   panel.grid = element_blank(),
                   panel.background = element_blank())
invasion_color <- scale_color_manual(values = c("red","deepskyblue"))
invasion_fill <- scale_color_manual(values = c("red","deepskyblue"))

# set vline variable for marking visit dates
vline_days <- c(unique(tick_survival$days))

temp_rh_data <- left_join(temp_rh_data_grouped, tick_survival)


# Figure for max temps all time all days ####

scaleFUN <- function(x) sprintf("%.1f", x)

max_temp_all_time <- ggplot(temp_rh_data_grouped, aes(days, avg_dailymax_tempC, color = status)) +
      geom_smooth(aes(fill = status, color = status), se = T, method = "lm",
                  alpha = .2) +
      geom_point(aes(color = status)) +
      #geom_hline(yintercept = 35, linetype = "solid", color = "gray") +
       #geom_vline(xintercept = vline_days, linetype = "dashed", alpha=.2) +
      geom_point(data = temp_rh_data_grouped, aes(y = avg_dailymax_tempC, color = status)) +
      invasion_color +
      invasion_fill +
      ylab("Average daily maximum temperature °C") +
      xlab("Days") +
      #scale_y_continuous(breaks = c(15,20,25,30,35,40,45)) +
      #scale_x_continuous(limits = c(0, 306)) +
  #scale_x_continuous(breaks = c(0, 50, 100, 150, 200, 250, 300)) +
  expand_limits(x = 0, y = 0) +
  scale_x_continuous(expand = c(0,0), limits = c(0,336), breaks = c(0, 50, 100, 150, 200, 250, 300, 350)) +
  scale_y_continuous(expand = c(0,0), limits = c(5, 44), breaks = c(5, 10, 15, 20, 25, 30, 35, 40)) +
      guides(fill=FALSE, color=FALSE) +
      def_theme +
      NULL

ggsave(plot = max_temp_all_time, height = 7, width = 7, "figures/tick-survival-assay/max_temp_all_time.png")
### changed for drew report, added more Y tick marks and removed scaleFUN, changed shape, removed tick visit vlines

# Figure for min rhs all time all days ####

min_rh_all_time <- ggplot(temp_rh_data_grouped, aes(days, avg_dailymin_rh, color = status)) +
      geom_smooth(aes(fill = status, color = status), se = T, method = "lm", formula = y ~ poly(x, 2), alpha = .2) +
      geom_point(aes(color = status)) +
      #geom_hline(yintercept = 80, linetype = "solid", color = "gray") +
      #geom_vline(xintercept = vline_days, linetype = "dashed", alpha=.2) +
      geom_point(data = temp_rh_data_grouped, aes(y = avg_dailymin_rh, color = status)) +
      invasion_color +
      invasion_fill +
      ylab("Average daily minimum %RH") +
      xlab("Days") +
  expand_limits(x = 0, y = 0) +
  scale_y_continuous(expand = c(0,0), limits = c(30, 102), breaks = c(30, 40, 50,60,70,80,90,100)) +
  scale_x_continuous(expand = c(0,0), limits = c(0, 350), breaks = c(0, 50, 100, 150, 200, 250, 300, 350)) +
      guides(fill=FALSE, color=FALSE) +
      def_theme +
      NULL

ggsave(plot = min_rh_all_time, height = 7, width = 7, "figures/tick-survival-assay/min_rh_all_time2.png")

##### blank for drew####
min_rh_all_time_blank <- ggplot(temp_rh_data_grouped, aes(days, avg_dailymin_rh, color = status)) +
  #geom_smooth(aes(fill = status, color = status), se = T, method = "lm", formula = y ~ poly(x, 2), alpha = .2) +
  #geom_point(aes(color = status)) +
  #geom_hline(yintercept = 80, linetype = "solid", color = "gray") +
  #geom_vline(xintercept = vline_days, linetype = "dashed", alpha=.2) +
  #geom_point(data = temp_rh_data_grouped, aes(y = avg_dailymin_rh, color = status)) +
  invasion_color +
  invasion_fill +
  ylab("Average daily minimum %RH") +
  xlab("Days") +
  scale_y_continuous(breaks = c(50,60,70,80,90,100)) +
  scale_x_continuous(limits = c(0, 264)) +
  scale_x_continuous(breaks = c(0, 50, 100, 150, 200, 250)) +
  guides(fill=FALSE, color=FALSE) +
  def_theme +
  NULL

ggsave(plot = min_rh_all_time_blank, height = 7, width = 7, "figures/tick-survival-assay/min_rh_all_time_blank.png")
#####

### changed for drew report, added more Y tick marks and removed scaleFUN, changed shape, removed tick visit vlines

# Separate tempRH data into chunks of days ####

temp_rh_survival1 <- temp_rh_data_grouped %>%
      filter(between(days, 1, 66))
temp_rh_survival2 <- temp_rh_data_grouped %>%
      filter(between(days, 67, 132))
temp_rh_survival3 <- temp_rh_data_grouped %>%
      filter(between(days, 133, 202))

# Figures for day 1-126 intervals for RH ####

surv1_rh <- ggplot(temp_rh_survival1, aes(days, avg_daily_RH, color = status)) +
      geom_smooth(aes(fill = status, color = status), se = T, method = "lm", alpha = .2) +
      geom_point(aes(color = status)) +
      geom_hline(yintercept = 80, linetype = "dashed") +
      geom_vline(xintercept = (11), linetype = "dashed") +
      geom_vline(xintercept = (15), linetype = "dashed") +
      geom_vline(xintercept = (25), linetype = "dashed") +
      geom_vline(xintercept = (32), linetype = "dashed") +
      geom_vline(xintercept = (39), linetype = "dashed") +
      geom_vline(xintercept = (55), linetype = "dashed") +
      geom_point(data = temp_rh_survival1, aes(y = avg_min_rh, color = status), shape = 25) +
      #geom_point(data = temp_rh_survival1, aes(y = avg_max_rh, color = status), shape = 24) +
      invasion_color +
      invasion_fill +
      ylab("Average daily RH") +
      xlab("Days") +
      theme(legend.position = "none") +
      def_theme +
      ylim(50,100) +
      xlim(0, 66) +
      NULL

surv2_rh <- ggplot(temp_rh_survival2, aes(days, avg_daily_RH, color = status)) +
      geom_smooth(aes(fill = status, color = status), se = T, method = "lm", alpha = .2) +
      geom_point(aes(color = status)) +
      geom_hline(yintercept = 80, linetype = "dashed") +
      geom_vline(xintercept = (67), linetype = "dashed") +
      geom_vline(xintercept = (77), linetype = "dashed") +
      geom_vline(xintercept = (89), linetype = "dashed") +
      geom_vline(xintercept = (98), linetype = "dashed") +
      geom_vline(xintercept = (110), linetype = "dashed") +
      geom_vline(xintercept = (126), linetype = "dashed") +
      geom_point(data = temp_rh_survival2, aes(y = avg_min_rh, color = status), shape = 25) +
      #geom_point(data = temp_rh_survival2, aes(y = avg_max_rh, color = status), shape = 24) +
      invasion_color +
      invasion_fill +
      xlab("Days") +
      theme(axis.title.y = element_blank()) +
      theme(legend.position = "none") +
      def_theme +
      ylim(50,100) +
      xlim(67, 132) +
      NULL

surv3_rh <- ggplot(temp_rh_survival3, aes(days, avg_daily_RH, color = status)) +
      geom_smooth(aes(fill = status, color = status), se = T, method = "lm", alpha = .2) +
      geom_point(aes(color = status)) +
      geom_hline(yintercept = 80, linetype = "dashed") +
      geom_vline(xintercept = (146), linetype = "dashed") +
      geom_vline(xintercept = (159), linetype = "dashed") +
      geom_vline(xintercept = (175), linetype = "dashed") +
      geom_vline(xintercept = (202), linetype = "dashed") +
      geom_point(data = temp_rh_survival3, aes(y = avg_min_rh, color = status), shape = 25) +
      #geom_point(data = temp_rh_survival3, aes(y = avg_max_rh, color = status), shape = 24) +
      invasion_color +
      invasion_fill +
      xlab("Days") +
      theme(axis.title.y = element_blank()) +
      theme(legend.position = "none") +
      def_theme +
      ylim(50,100) +
      xlim(133, 202) +
      NULL

# Figures for day 1-126 intervals for temperature ####

surv1_temp <- ggplot(temp_rh_survival1, aes(days, avg_daily_tempC, color = status)) +
      geom_smooth(aes(fill = status, color = status), se = T, method = "lm", alpha = .2) +
      geom_point(aes(color = status)) +
      geom_point(data = temp_rh_survival1, aes(y = avg_min_tempC, color = status), shape = 25) +
      geom_point(data = temp_rh_survival1, aes(y = avg_max_tempC, color = status), shape = 24) +
      invasion_color +
      invasion_fill +
      geom_hline(yintercept = 35, linetype = "dashed") +
      geom_vline(xintercept = (11), linetype = "dashed") +
      geom_vline(xintercept = (15), linetype = "dashed") +
      geom_vline(xintercept = (25), linetype = "dashed") +
      geom_vline(xintercept = (32), linetype = "dashed") +
      geom_vline(xintercept = (39), linetype = "dashed") +
      geom_vline(xintercept = (55), linetype = "dashed") +
      xlab("Days") +
      ylab("Average daily temp C") +
      theme(legend.position = "none") +
      def_theme +
      ylim(5,45) +
      xlim(0, 66) +
      NULL

surv2_temp <- ggplot(temp_rh_survival2, aes(days, avg_daily_tempC, color = status)) +
      geom_smooth(aes(fill = status, color = status), se = T, method = "lm", alpha = .2) +
      geom_point(aes(color = status)) +
      geom_point(data = temp_rh_survival2, aes(y = avg_min_tempC, color = status), shape = 25) +
      geom_point(data = temp_rh_survival2, aes(y = avg_max_tempC, color = status), shape = 24) +
      invasion_color +
      invasion_fill +
      geom_hline(yintercept = 35, linetype = "dashed") +
      geom_vline(xintercept = (67), linetype = "dashed") +
      geom_vline(xintercept = (77), linetype = "dashed") +
      geom_vline(xintercept = (89), linetype = "dashed") +
      geom_vline(xintercept = (98), linetype = "dashed") +
      geom_vline(xintercept = (110), linetype = "dashed") +
      geom_vline(xintercept = (126), linetype = "dashed") +
      xlab("Days") +
      theme(axis.title.y = element_blank()) +
      theme(legend.position = "none") +
      def_theme +
      ylim(5,45) +
      xlim(67, 132) +
      NULL

surv3_temp <- ggplot(temp_rh_survival3, aes(days, avg_daily_tempC, color = status)) +
      geom_smooth(aes(fill = status, color = status), se = T, method = "lm", alpha = .2) +
      geom_point(aes(color = status)) +
      geom_point(data = temp_rh_survival3, aes(y = avg_min_tempC, color = status), shape = 25) +
      geom_point(data = temp_rh_survival3, aes(y = avg_max_tempC, color = status), shape = 24) +
      invasion_color +
      invasion_fill +
      geom_hline(yintercept = 35, linetype = "dashed") +
      geom_vline(xintercept = (146), linetype = "dashed") +
      geom_vline(xintercept = (159), linetype = "dashed") +
      geom_vline(xintercept = (175), linetype = "dashed") +
      geom_vline(xintercept = (202), linetype = "dashed") +
      xlab("Days") +
      theme(axis.title.y = element_blank()) +
      theme(legend.position = "none") +
      def_theme +
      ylim(5,45) +
      xlim(133, 202) +
      NULL

#surv1_temp_rh <- ggplot(temp_rh_survival1, aes(days, avg_min_rh, color = status)) +
#geom_smooth(aes(fill = status, color = status), se = T, method = "lm", alpha = .2) +
#geom_point(aes(color = status)) +
#scale_y_continuous("avg_min_rh", sec.axis = sec_axis(temp_rh_survival1, name = "avg_max_tempC")) +
#invasion_color +
#invasion_fill +
#def_theme +
#NULL

avg_min_rhs_days_intervals <- cowplot::plot_grid(surv1_rh, surv2_rh, surv3_rh, ncol = 3)
#ggsave(plot = avg_min_rhs_days_intervals, "figures/tick-survival-assay/avg_min_rhs_days_intervals.png", height = 10, width = 7)

avg_max_temps_days_intervals <- cowplot::plot_grid(surv1_temp, surv2_temp, surv3_temp, ncol = 3)
#ggsave(plot = avg_max_temps_days_intervals, "figures/tick-survival-assay/avg_max_temps_days_intervals.png", height = 10, width =7)

# Plots for first check up date 14 days ####

temp_rh_surv_first_check <- temp_rh_data_grouped %>%
      filter(days <= 14)

surv_first_check_rh <- ggplot(temp_rh_surv_first_check, aes(days, avg_min_rh, color = status)) +
      geom_smooth(aes(fill = status, color = status), se = T, method = "lm", alpha = 0) +
      geom_point(aes(color = status)) +
      invasion_color +
      invasion_fill +
      def_theme +
      NULL

surv_first_check_temp <- ggplot(temp_rh_surv_first_check, aes(days, avg_max_tempC, color = status)) +
      geom_smooth(aes(fill = status, color = status), se = T, method = "lm", alpha = 0) +
      geom_point(aes(color = status)) +
      invasion_color +
      invasion_fill +
      def_theme +
      NULL

max_temp_min_rh_first_check <- cowplot::plot_grid(surv_first_check_rh, surv_first_check_temp, ncol = 2)
#ggsave(plot = max_temp_min_rh_first_check, "figures/tick-survival-assay/max_temp_min_rh_first_check.png")

## grouping by individual date time points attempt QUESTION ####

# only one temp/rh measurement taken per timepoint, no min/max unless you take min/max between 12 loggers of same treatment?

# #temp_rh_data_timepoints <- temp_rh_data %>%
#   group_by(time, status, logger_id, days) %>%
#   summarise(avg_daily_tempC = mean(tempC),
#             avg_daily_RH = mean(RH)
#             ) %>%
#   ungroup(.) %>%
#   group_by(time, status, days, logger_id) %>%
#   summarise(avg_daily_tempC = mean(avg_daily_tempC),
#             avg_daily_RH = mean(avg_daily_RH)
#   )


# Begin processing for time above/below data ####

rh_below_80 <- temp_rh_data %>%
      filter(between(RH, 20, 80)) %>%
      #filter(RH <= 80) %>%
      group_by(status, logger_id) %>%
      summarise(obs = n(),
                hrs = obs/12,
                days = hrs/24)

rh_below_80_Jun_Sep <- temp_rh_data %>%
      filter(between(RH, 20, 80), date < "2018-10-01") %>%
      #filter(RH <= 80) %>%
      group_by(status, logger_id) %>%
      summarise(obs = n(),
                hrs = obs/12,
                days = hrs/24)

#View(rh_below_80)

# ggplot(filter(temp_rh_data, between(RH,20,80), logger_id==13), aes(date_time, 1/RH)) +
      # geom_path()

rh_below_80_avg <- temp_rh_data %>%
      filter(between(RH, 20, 80)) %>%
      #filter(RH <= 80) %>%
      group_by(status, logger_id) %>%
      summarise(obs = n(),
                hrs = obs/12,
                days = hrs/24) %>%
      ungroup(.) %>%
      group_by(status) %>%
      summarise(avg_days = mean(days),
                sd = sd(days))

temp_above_35 <- temp_rh_data %>%
      filter(tempC >= 35) %>%
      group_by(status, logger_id) %>%
      summarise(obs = n(),
                hrs = obs/12,
                days = hrs/24)

temp_above_35_avg <- temp_rh_data %>%
      filter(tempC >= 35) %>%
      group_by(status, logger_id) %>%
      summarise(obs = n(),
                hrs = obs/12,
                days = hrs/24) %>%
      ungroup(.) %>%
      group_by(status) %>%
      summarise(avg_days = mean(days),
                sd = sd(days))

#View(temp_above_35)

rh_threshold <- ggplot(rh_below_80, aes(status, days)) +
      geom_boxplot(outlier.shape = 1) +
      geom_point(aes(color = status), position = position_jitter(width = .1)) +
      invasion_color +
      invasion_fill +
      #xlab("Invasion status") +
      ylab("Time below 80% RH (days)") +
      # def_theme +
      theme(legend.position = "none") +
      NULL

days_above_35_tempC <- ggplot(temp_above_35, aes(status, days)) +
      geom_boxplot(outlier.shape = 1) +
      geom_point(aes(color = status), position = position_jitter(width = .1)) +
      # geom_bar(stat = "identity") +
      #geom_errorbar(data = temp_above_35_avg,
      #aes(ymin=avg_days_above_35_tempC - sd,
      #ymax=avg_days_above_35_tempC + sd)) +
      invasion_color +
      invasion_fill +
      #xlab("Invasion status") +
      ylab("Average time above 35 C (days)") +
      def_theme +
      theme(legend.position = "none") +
      NULL

avg_time_days_temp_rh_barchart <- cowplot::plot_grid(rh_threshold, days_above_35_tempC, ncol = 2)

#ggsave(plot = avg_time_days_temp_rh_barchart, "figures/tick-survival-assay/avg_time_days_temp_rh_barchart.png")

avg_days_rh_boxplot <- ggplot(
      filter(rh_below_80, !(logger_id %in% c(01,19))),
      aes(status, days, color = status)) +
      geom_boxplot() +
      geom_point() +
      invasion_color +
      invasion_fill +
      def_theme +
      ylab("time below 80 rh (days)") +
      theme(legend.position = "none") +
      NULL


ggplot(rh_below_80_Jun_Sep,
       aes(status, days, color = status)) +
      geom_boxplot(notch = F) +
      invasion_color +
      invasion_fill +
      def_theme +
      ylab("time below 80 rh (days)") +
      theme(legend.position = "none") +
      NULL
#View(rh_below_80)

#ggsave(plot = avg_days_rh_boxplot, "figures/tick-survival-assay/avg_days_rh_boxplot.png")

avg_days_temp_boxplot <- ggplot(temp_above_35, aes(status, days, color = status)) +
      geom_boxplot() +
      invasion_color +
      invasion_fill +
      def_theme +
      ylab("time above 35 C (days)") +
      theme(legend.position = "none") +
      NULL

#ggsave(plot = avg_days_temp_boxplot, "figures/tick-survival-assay/avg_days_temp_boxplot.png")

avg_days_temp_rh_boxplot <- cowplot::plot_grid(avg_days_rh_boxplot, avg_days_temp_boxplot, ncol = 2)

ggsave(plot = avg_days_temp_rh_boxplot, "figures/tick-survival-assay/avg_days_temp_rh_boxplot.png", height = 7, width = 7)



# Drews graphs ####

nymph_survival_all_time <- ggplot(data = filter(tick_survival, life_stage=="nymph")) +
      geom_vline(xintercept = vline_days, linetype = "dashed", alpha=0.20) +

      stat_summary(aes(days, survival, fill = Invaded, color = Invaded),fun.data = mean_se, geom = "pointrange") +
      stat_smooth(aes(days, survival, fill = Invaded, color = Invaded), se = T, alpha = .2) +
      invasion_color +
      invasion_fill +
      def_theme +
      #theme_classic() +
      ggtitle(label = "Nymphs") +
      theme(plot.title = element_text(hjust = 0.5)) +
      xlab(" ") +
      ylab("Survival") +
      guides(fill=FALSE, color=FALSE) +
      scale_y_continuous(limits = c(0, 1 )) +
      scale_x_continuous(limits = c(0, 216)) +
      #theme(axis.title.x = element_blank()) +
      NULL

f_adult_survival_all_time <- ggplot(data = tick_survival) +
      stat_summary(aes(days, adult_F_survival, fill = Invaded, color = Invaded),
                   fun.data = mean_se, geom = "pointrange") +
      stat_smooth(aes(days, adult_F_survival, color = Invaded, fill = Invaded),
                  se = T, alpha = .2) +
      invasion_color +
      invasion_fill +
      def_theme +
      ggtitle(label = "Female Adults") +
      theme(plot.title = element_text(hjust = 0.5)) +
      guides(fill=FALSE, color=FALSE) +
      #theme(legend.position = c(0, 0), legend.justification = c(0, 0)) +
      #theme(axis.title.y = element_blank()) +
      scale_y_continuous(limits = c(0, 1 )) +
      scale_x_continuous(limits = c(0, 216)) +
      ylab("Survival") +
      xlab(" ") +
      geom_vline(xintercept = vline_days, linetype = "dashed", alpha=0.20) +
      NULL

m_adult_survival_all_time <- ggplot(data = tick_survival) +
      stat_summary(aes(days, adult_M_survival, fill = Invaded, color = Invaded),
                   fun.data = mean_se, geom = "pointrange") +
      stat_smooth(aes(days, adult_M_survival, color = Invaded, fill = Invaded),
                  se = T, alpha = .2) +
      invasion_color +
      invasion_fill +
      def_theme +
      ggtitle(label = "Male Adults") +
      theme(plot.title = element_text(hjust = 0.5)) +
      guides(fill=FALSE, color=FALSE) +
      scale_y_continuous(limits = c(0, 1 )) +
      scale_x_continuous(limits = c(0, 216)) +
      xlab(" ") +
      ylab("Survival") +
      #theme(axis.title.x = element_blank()) +
      #theme(axis.title.y = element_blank()) +
      geom_vline(xintercept = vline_days, linetype = "dashed", alpha=0.20) +
      NULL

tick_survival_all_time <- cowplot::plot_grid(nymph_survival_all_time,
                                             f_adult_survival_all_time, m_adult_survival_all_time, ncol = 3)

# Stack all life stages ####

avg_survival <- tick_survival %>%
      group_by(days, Invaded, life_stage) %>%
      summarise(avg_surv = mean(survival))

all_stages_stacked <- ggplot(
      data = tick_survival,
      aes(days, survival*100, shape = life_stage, color = Invaded)
      ) +

      # geom_vline(xintercept = vline_days, linetype = "dashed", alpha=0.20) +
      geom_hline(yintercept = 50, linetype = "solid", color = "gray") +

      stat_summary(fun.data = mean_se, geom = "pointrange", alpha=0.80, show.legend = F) +
      stat_summary(fun.y = mean, geom = "point") +

      geom_step(data = avg_survival, aes(days, avg_surv*100, linetype = life_stage), show.legend = F) +

      # facet_grid(.~sex) +

      invasion_color +
      invasion_fill +
      theme_classic() +
      def_theme +
      # ggtitle(label = "Tick Survival") +
      theme(
            # plot.title = element_text(hjust = 0.5)
            # strip.text = element_text(size = 18),
            legend.position = c(.1,.15)
            ) +
      #guides(fill=FALSE, color=FALSE) +
      scale_y_continuous(limits = c(0, 100 )) +
      scale_x_continuous(limits = c(0, 216)) +
      #scale_shape_manual(values = c(1,2)) +
      xlab("Days") +
      ylab("Survival") +
      NULL


ggsave(plot = all_stages_stacked, "figures/tick-survival-assay/all_stages_stacked.png", height = 7, width = 7, dpi = 300)

#ggsave(plot = tick_survival_all_time, "figures/tick-survival-assay/tick_survival_all_time.png")

# Begin break up by 40 day increments ####

tick_survival_40 <- tick_survival %>%
      filter(days <=66)

tick_survival_80 <- tick_survival %>%
      filter(between(days, 67, 132))

tick_survival_120 <- tick_survival %>%
      filter(between(days, 133, 202))


# Nymphs by days interval NOT USED PROBABLY ####

nymph_survival_40 <- ggplot(data = tick_survival_40) +
      stat_summary(aes(days, nymph_survival, fill = Invaded, color = Invaded),
                   fun.data = mean_se, geom = "pointrange") +
      stat_smooth(aes(days, nymph_survival, color = Invaded),
                  se = T, alpha = .2) +  invasion_color +
      invasion_fill +
      def_theme +
      ggtitle(label = "Nymphs") +
      theme(plot.title = element_text(hjust = 0.5)) +
      xlim(0, 66) +
      ylim(0, 1) +
      xlab("Days") +
      ylab("Survival") +
      theme(legend.position = "none",
            plot.title = element_text(size=12, face = "bold")) +
      #theme(axis.title.x = element_blank()) +
      NULL

nymph_survival_80 <- ggplot(data = tick_survival_80) +
      stat_summary(aes(days, nymph_survival, fill = Invaded, color = Invaded),
                   fun.data = mean_se, geom = "pointrange") +
      stat_smooth(aes(days, nymph_survival, color = Invaded),
                  se = T, alpha = .2) +  invasion_color +
      invasion_fill +
      def_theme +
      ggtitle(label = "Nymphs") +
      theme(plot.title = element_text(hjust = 0.5)) +
      xlim(67, 132) +
      ylim(0, 1) +
      xlab("Days") +
      #ylab("Survival") +
      theme(legend.position = "none") +
      #theme(axis.title.x = element_blank()) +
      theme(axis.title.y = element_blank(),
            plot.title = element_text(size=12, face = "bold")) +
      NULL

nymph_survival_120 <- ggplot(data = tick_survival_120) +
      stat_summary(aes(days, nymph_survival, fill = Invaded, color = Invaded),
                   fun.data = mean_se, geom = "pointrange") +
      stat_smooth(aes(days, nymph_survival, color = Invaded),
                  se = T, alpha = .2) +  invasion_color +
      invasion_fill +
      def_theme +
      ggtitle(label = "Nymphs") +
      theme(plot.title = element_text(hjust = 0.5)) +
      xlim(133, 202) +
      ylim(0, 1) +
      xlab("Days") +
      #ylab("Survival") +
      theme(legend.position = "none") +
      #theme(axis.title.x = element_blank()) +
      theme(axis.title.y = element_blank(),
            plot.title = element_text(size=12, face = "bold")) +
      NULL

nymphs_intervals <- cowplot::plot_grid(nymph_survival_40,
                                       nymph_survival_80, nymph_survival_120, ncol =3)

# Adult F by days NOT USED ####

adult_F_survival_40 <- ggplot(data = tick_survival_40) +
      stat_summary(aes(days, adult_F_survival, fill = Invaded, color = Invaded),
                   fun.data = mean_se, geom = "pointrange") +
      stat_smooth(aes(days, adult_F_survival, color = Invaded),
                  se = T, alpha = .2) +  invasion_color +
      invasion_fill +
      def_theme +
      ggtitle(label = "Adult Female") +
      theme(plot.title = element_text(hjust = 0.5)) +
      xlim(0, 40) +
      ylim(0, 1) +
      xlab("Days") +
      ylab("Survival") +
      theme(legend.position = "none",
            plot.title = element_text(size=12, face = "bold")) +
      #theme(axis.title.x = element_blank()) +
      NULL

adult_F_survival_80 <- ggplot(data = tick_survival_80) +
      stat_summary(aes(days, adult_F_survival, fill = Invaded, color = Invaded),
                   fun.data = mean_se, geom = "pointrange") +
      stat_smooth(aes(days, adult_F_survival, color = Invaded),
                  se = T, alpha = .2) +  invasion_color +
      invasion_fill +
      def_theme +
      ggtitle(label = "Adult Female") +
      theme(plot.title = element_text(hjust = 0.5)) +
      xlim(40, 80) +
      ylim(0, 1) +
      xlab("Days") +
      #ylab("Survival") +
      theme(legend.position = "none") +
      #theme(axis.title.x = element_blank()) +
      theme(axis.title.y = element_blank(),
            plot.title = element_text(size=12, face = "bold")) +
      NULL

adult_F_survival_120 <- ggplot(data = tick_survival_120) +
      stat_summary(aes(days, adult_F_survival, fill = Invaded, color = Invaded),
                   fun.data = mean_se, geom = "pointrange") +
      stat_smooth(aes(days, adult_F_survival, color = Invaded),
                  se = T, alpha = .2) +  invasion_color +
      invasion_fill +
      def_theme +
      ggtitle(label = "Adult Female") +
      theme(plot.title = element_text(hjust = 0.5)) +
      xlim(80, 126) +
      ylim(0, 1) +
      xlab("Days") +
      #ylab("Survival") +
      theme(legend.position = "none") +
      #theme(axis.title.x = element_blank()) +
      theme(axis.title.y = element_blank(),
            plot.title = element_text(size=12, face = "bold")) +
      NULL

adult_F_intervals <- cowplot::plot_grid(adult_F_survival_40,
                                        adult_F_survival_80, adult_F_survival_120, ncol =3)


# Adult M by days NOT USED####

adult_M_survival_40 <- ggplot(data = tick_survival_40) +
      stat_summary(aes(days, adult_M_survival, fill = Invaded, color = Invaded),
                   fun.data = mean_se, geom = "pointrange") +
      stat_smooth(aes(days, adult_M_survival, color = Invaded),
                  se = T, alpha = .2) +  invasion_color +
      invasion_fill +
      def_theme +
      ggtitle(label = "Adult Male") +
      theme(plot.title = element_text(hjust = 0.5)) +
      xlim(0, 40) +
      ylim(0, 1) +
      xlab("Days") +
      ylab("Survival") +
      theme(legend.position = "none",
            plot.title = element_text(size=12, face = "bold")) +
      #theme(axis.title.x = element_blank()) +
      NULL

adult_M_survival_80 <- ggplot(data = tick_survival_80) +
      stat_summary(aes(days, adult_M_survival, fill = Invaded, color = Invaded),
                   fun.data = mean_se, geom = "pointrange") +
      stat_smooth(aes(days, adult_M_survival, color = Invaded),
                  se = T, alpha = .2) +  invasion_color +
      invasion_fill +
      def_theme +
      ggtitle(label = "Adult Male") +
      theme(plot.title = element_text(hjust = 0.5)) +
      xlim(40, 80) +
      ylim(0, 1) +
      xlab("Days") +
      #ylab("Survival") +
      theme(legend.position = "none") +
      #theme(axis.title.x = element_blank()) +
      theme(axis.title.y = element_blank(),
            plot.title = element_text(size=12, face = "bold")) +
      NULL

adult_M_survival_120 <- ggplot(data = tick_survival_120) +
      stat_summary(aes(days, adult_M_survival, fill = Invaded, color = Invaded),
                   fun.data = mean_se, geom = "pointrange") +
      stat_smooth(aes(days, adult_M_survival, color = Invaded),
                  se = T, alpha = .2) +  invasion_color +
      invasion_fill +
      def_theme +
      ggtitle(label = "Adult Male") +
      theme(plot.title = element_text(hjust = 0.5)) +
      xlim(80, 126) +
      ylim(0, 1) +
      xlab("Days") +
      #ylab("Survival") +
      theme(legend.position = "none") +
      #theme(axis.title.x = element_blank()) +
      theme(axis.title.y = element_blank(),
            plot.title = element_text(size=12, face = "bold")) +
      NULL

adult_M_intervals <- cowplot::plot_grid(adult_M_survival_40,
                                        adult_M_survival_80, adult_M_survival_120, ncol =3)



# Survival and microclimate figures ####

survival_microclimate_fig <- cowplot::plot_grid(
      all_stages_stacked +
            xlab(""),
      min_rh_all_time +
            xlab("") +
            scale_x_continuous(limits = c(0, 216)),
      max_temp_all_time +
            xlab("Days") +
            scale_x_continuous(limits = c(0, 216)),
      ncol = 1
)

ggsave(plot = survival_microclimate_fig, "figures/tick-survival-assay/survival_microclimate_fig.png", height = 12, width = 7, dpi = 300)

nymphs_temp_rh_all_time <- cowplot::plot_grid(nymph_survival_all_time,
                                              max_temp_all_time, min_rh_all_time, ncol = 1)
m_adult_temp_rh_all_time <- cowplot::plot_grid(m_adult_survival_all_time,
                                               max_temp_all_time, min_rh_all_time, ncol = 1)
f_adult_temp_rh_all_time <- cowplot::plot_grid(f_adult_survival_all_time,
                                               max_temp_all_time, min_rh_all_time, ncol = 1)

# ggsave(plot = nymphs_temp_rh_all_time, height = 10, width = 7, "figures/tick-survival-assay/nymphs_temp_rh_all_time.png")
# ggsave(plot = m_adult_temp_rh_all_time, height = 10, width = 7,"figures/tick-survival-assay/m_adult_temp_rh_all_time.png")
# ggsave(plot = f_adult_temp_rh_all_time, height = 10, width = 7, "figures/tick-survival-assay/f_adult_temp_rh_all_time.png")

nymph_surv_min_rh <- cowplot::plot_grid(nymphs_intervals, avg_min_rhs_days_intervals, ncol = 1)

adult_F_surv_min_rh <- cowplot::plot_grid(adult_F_intervals, avg_min_rhs_days_intervals, ncol = 1)

adult_M_surv_min_rh <- cowplot::plot_grid(adult_M_intervals, avg_min_rhs_days_intervals, ncol = 1)

nymph_surv_max_temp <- cowplot::plot_grid(nymphs_intervals, avg_max_temps_days_intervals, ncol = 1)

adult_F_surv_max_temp <- cowplot::plot_grid(adult_F_intervals, avg_max_temps_days_intervals, ncol = 1)

adult_M_surv_max_temp <- cowplot::plot_grid(adult_M_intervals, avg_max_temps_days_intervals, ncol = 1)


# Analysis of bag specific survial w/ temp/rh ####
temp_rh_data_bag_specific <- temp_rh_data %>%
      filter(RH >= 20, date != "2018-06-21") %>%
      group_by(date, status, logger_id, days) %>%
      summarise(max_tempC = max(tempC),
                min_rh = min(RH)
      ) %>%
      ungroup(.) %>%
      filter(days <=11) %>%
      group_by(logger_id, status) %>%
      summarise(obs = n(),
                avg_dailymax_tempC = mean(max_tempC),
                max_tempC_sd = sd(max_tempC),
                max_temp_se = max_tempC_sd/sqrt(obs),
                avg_dailymin_rh = mean(min_rh),
                min_rh_sd = sd(min_rh),
                min_rh_se = min_rh_sd/sqrt(obs)
      )

#View(temp_rh_data_bag_specific)

survival_for_temp_rh_11_days <- tick_survival %>%
      filter(days == "11") %>%
      select(logger_id, Invaded, nymph_survival, adult_F_survival, adult_M_survival)

survival_temp_rh <-  left_join(temp_rh_data_bag_specific, survival_for_temp_rh_11_days,
                               by = c("logger_id", "status"="Invaded"))

View(survival_temp_rh)
range(survival_temp_rh$avg_max_tempC)


# 11 days first check figures for TEMP ####

nymph_temp_11_days <- ggplot(survival_temp_rh, aes(avg_max_tempC, nymph_survival,
                                                   color = status)) +
      #geom_smooth(aes(fill = status, color = status), se = T, method = "lm", alpha = .2) +
      geom_point(aes(color = status)) +
      geom_errorbarh(aes(xmin = avg_max_tempC - max_temp_se,
                         xmax = avg_max_tempC + max_temp_se)) +
      invasion_color +
      invasion_fill +
      ylab("Survival") +
      xlab(" ") +
      def_theme +
      guides(fill=FALSE, color=FALSE) +
      ggtitle(label = "Nymphs")  +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_y_continuous(limits = c(0, 1 )) +
      scale_x_continuous(limits = c(30, 43)) +
      NULL

adult_F_temp_11_days <- ggplot(survival_temp_rh, aes(avg_max_tempC, adult_F_survival,
                                                     color = status)) +
      #geom_smooth(aes(fill = status, color = status), se = T, method = "lm", alpha = .2) +
      geom_point(aes(color = status)) +
      #geom_errorbarh(aes(xmin = avg_max_tempC - max_temp_se,
      #xmax = avg_max_tempC + max_temp_se)) +
      invasion_color +
      invasion_fill +
      ylab(" ") +
      xlab("Avg max temp C ") +
      def_theme +
      guides(fill=FALSE, color=FALSE) +
      ggtitle(label = "Adult Females") +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_y_continuous(limits = c(0, 1 )) +
      scale_x_continuous(limits = c(30, 43)) +
      NULL

adult_M_temp_11_days <- ggplot(survival_temp_rh, aes(avg_max_tempC, adult_M_survival,
                                                     color = status)) +
      #geom_smooth(aes(fill = status, color = status), se = T, method = "lm", alpha = .2) +
      geom_point(aes(color = status)) +
      #geom_errorbarh(aes(xmin = avg_max_tempC - max_temp_se,
      #xmax = avg_max_tempC + max_temp_se)) +
      invasion_color +
      invasion_fill +
      ylab(" ") +
      xlab(" ") +
      def_theme +
      guides(fill=FALSE, color=FALSE) +
      ggtitle(label = "Adult Males") +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_y_continuous(limits = c(0, 1 )) +
      scale_x_continuous(limits = c(30, 43)) +
      NULL

survival_temp_first_check <- cowplot::plot_grid(nymph_temp_11_days,
                                                adult_F_temp_11_days,
                                                adult_M_temp_11_days, ncol = 3)

ggsave(plot = survival_temp_first_check, height = 5, width = 10, "figures/tick-survival-assay/survival_temp_first_check.png")

# 11 days survival for RH ####
nymph_rh_11_days <- ggplot(survival_temp_rh, aes(avg_min_rh, nymph_survival,
                                                 color = status)) +
      #geom_smooth(aes(fill = status, color = status), se = T, method = "lm", alpha = .2) +
      geom_point(aes(color = status)) +
      geom_errorbarh(aes(xmin = avg_min_rh - min_rh_se,
                         xmax = avg_min_rh + min_rh_se)) +
      invasion_color +
      invasion_fill +
      ylab("Survival") +
      xlab(" ") +
      def_theme +
      guides(fill=FALSE, color=FALSE) +
      ggtitle(label = "Nymphs")  +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_y_continuous(limits = c(0, 1 )) +
      NULL

adult_F_rh_11_days <- ggplot(survival_temp_rh, aes(avg_min_rh, adult_F_survival,
                                                   color = status)) +
      #geom_smooth(aes(fill = status, color = status), se = T, method = "lm", alpha = .2) +
      geom_point(aes(color = status)) +
      geom_errorbarh(aes(xmin = avg_min_rh - min_rh_se,
                         xmax = avg_min_rh + min_rh_se)) +
      invasion_color +
      invasion_fill +
      ylab(" ") +
      xlab("Avg min RH") +
      def_theme +
      guides(fill=FALSE, color=FALSE) +
      ggtitle(label = "Female Adults")  +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_y_continuous(limits = c(0, 1 )) +
      NULL

adult_M_rh_11_days <- ggplot(survival_temp_rh, aes(avg_min_rh, adult_M_survival,
                                                   color = status)) +
      #geom_smooth(aes(fill = status, color = status), se = T, method = "lm", alpha = .2) +
      geom_point(aes(color = status)) +
      geom_errorbarh(aes(xmin = avg_min_rh - min_rh_se,
                         xmax = avg_min_rh + min_rh_se)) +
      invasion_color +
      invasion_fill +
      ylab(" ") +
      xlab(" ") +
      def_theme +
      guides(fill=FALSE, color=FALSE) +
      ggtitle(label = "Male Adults")  +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_y_continuous(limits = c(0, 1 )) +
      NULL

survival_rh_first_check <- cowplot::plot_grid(nymph_rh_11_days, adult_F_rh_11_days, adult_M_rh_11_days, ncol = 3)

ggsave(plot = survival_rh_first_check, height = 5, width = 10, "figures/tick-survival-assay/survival_rh_first_check.png")


## Steven doing humidity time intervals ####

#+load datar

humidity_intervals <- read_csv("data/processed_data/humidity_minutes_by_logger.csv")
tail(humidity_intervals)

##### testing for 80 80-82 82+ interval #####
humidity_test_82 <- ggplot(humidity_intervals, aes(days, minutes_above_82, color = status)) +
  geom_smooth(aes(fill = status, color = status), se = T, method = "lm", alpha = .2) +
  geom_point(data = humidity_intervals,aes(y = minutes_above_82, color = status)) +
  #geom_errorbarh(aes(xmin = avg_min_rh - min_rh_se,
                     #xmax = avg_min_rh + min_rh_se)) +
  invasion_color +
  invasion_fill +
  ylab("Minutes above 82% RH") +
  xlab("Days") +
  def_theme +
  guides(fill=FALSE, color=FALSE) +
  ggtitle(label = "Minutes above 82% RH")  +
  #theme(plot.title = element_text(hjust = 0.5)) +
  #scale_y_continuous(limits = c(0, 1 )) +
  NULL

humidity_test_80b <- ggplot(humidity_intervals, aes(days, minutes_below_80, color = status)) +
  geom_smooth(aes(fill = status, color = status), se = T, method = "lm", formula = y ~ (poly(x, 2)), alpha = .2) +
  geom_point(data = humidity_intervals,aes(y = minutes_below_80, color = status)) +
  #geom_errorbarh(aes(xmin = avg_min_rh - min_rh_se,
  #xmax = avg_min_rh + min_rh_se)) +
  invasion_color +
  invasion_fill +
  ylab("Minutes below 80% RH") +
  xlab("Days") +
  def_theme +
  guides(fill=FALSE, color=FALSE) +
  ggtitle(label = "Minutes b 80% RH")  +
  #theme(plot.title = element_text(hjust = 0.5)) +
  #scale_y_continuous(limits = c(0, 1 )) +
  NULL





humid_above <- ggplot(data = humidity_intervals) +
  #geom_vline(xintercept = vline_days, linetype = "dashed", alpha=0.20) +
  stat_summary(aes(days, minutes_above_82, fill = status, color = status),fun.data = mean_se, geom = "pointrange") +
  #stat_smooth(aes(days, minutes_above_82, fill = status, color = status), se = T, alpha = .2) +
  invasion_color +
  invasion_fill +
  def_theme +
  #theme_classic() +
  ggtitle(label = "Minutes above 82% RH") +
  #theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Days") +
  ylab("Time above 82% RH (mins)") +
  #guides(fill=FALSE, color=FALSE) +
  #scale_y_continuous(limits = c(0, 1440 )) +
  #scale_x_continuous(limits = c(0, 264)) +
  #theme(axis.title.x = element_blank()) +
  NULL

humid_bw <- ggplot(data = humidity_intervals) +
  #geom_vline(xintercept = vline_days, linetype = "dashed", alpha=0.20) +

  stat_summary(aes(days, minutes_bw_80_82, fill = status, color = status),fun.data = mean_se, geom = "pointrange") +
  #stat_smooth(aes(days, minutes_above_82, fill = status, color = status), se = T, alpha = .2) +
  invasion_color +
  invasion_fill +
  def_theme +
  #theme_classic() +
  ggtitle(label = "Minutes between 80-82% RH") +
  #theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Days") +
  ylab("Time b/w 80-82% RH (mins)") +
  #guides(fill=FALSE, color=FALSE) +
  #scale_y_continuous(limits = c(0, 1440 )) +
  #scale_x_continuous(limits = c(0, 264)) +
  #theme(axis.title.x = element_blank()) +
  NULL

humid_below <- ggplot(data = humidity_intervals) +
  #geom_vline(xintercept = vline_days, linetype = "dashed", alpha=0.20) +

  stat_summary(aes(days, minutes_below_80, fill = status, color = status),fun.data = mean_se, geom = "pointrange") +
  #stat_smooth(aes(days, minutes_above_82, fill = status, color = status), se = T, alpha = .2) +
  invasion_color +
  invasion_fill +
  def_theme +
  #theme_classic() +
  ggtitle(label = "Minutes below 80% RH") +
  #theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Days") +
  ylab("Time below 80% RH (mins)") +
  #guides(fill=FALSE, color=FALSE) +
  #scale_y_continuous(limits = c(0, 1440 )) +
  #scale_x_continuous(limits = c(0, 264)) +
  #theme(axis.title.x = element_blank()) +
  NULL

ggsave(plot = humid_above, height = 5, width = 10, "figures/tick-survival-assay/humid_above.png")
ggsave(plot = humid_bw, height = 5, width = 10, "figures/tick-survival-assay/humid_bw.png")
ggsave(plot = humid_below, height = 5, width = 10, "figures/tick-survival-assay/humid_below.png")

cowplot::plot_grid(humid_above, humid_bw, humid_below, ncol= 1)
##### testing for new intervals of 78 ####

humid_78 <- read_csv("data/processed_data/humidity_78.csv")

humidity_test_78_above <- ggplot(humid_78, aes(days, minutes_above_85, color = status)) +
  geom_smooth(aes(fill = status, color = status), se = T, method = "lm", alpha = .2) +
  geom_point(data = humid_78,aes(y = minutes_above_85, color = status)) +
  #geom_errorbarh(aes(xmin = avg_min_rh - min_rh_se,
  invasion_color +
  invasion_fill +
  ylab("Minutes above 85% RH") +
  xlab("Days") +
  def_theme +
  guides(fill=FALSE, color=FALSE) +
  ggtitle(label = "Minutes above 85% RH")  +
  #theme(plot.title = element_text(hjust = 0.5)) +
  #scale_y_continuous(limits = c(0, 1 )) +
  NULL

humidity_test_78_below <- ggplot(humid_78, aes(days, minutes_below_78, color = status)) +
  geom_smooth(aes(fill = status, color = status), se = T, method = "lm", alpha = .2) +
  geom_point(data = humid_78,aes(y = minutes_below_78, color = status)) +
  #geom_errorbarh(aes(xmin = avg_min_rh - min_rh_se,
  invasion_color +
  invasion_fill +
  ylab("Minutes b 78% RH") +
  xlab("Days") +
  def_theme +
  guides(fill=FALSE, color=FALSE) +
  ggtitle(label = "Minutes b 78 % RH")  +
  #theme(plot.title = element_text(hjust = 0.5)) +
  #scale_y_continuous(limits = c(0, 1 )) +
  NULL


#####

###### testing for new intervals of 75 ####

humid_75 <- read_csv("data/processed_data/humidity_75.csv")

humidity_test_75_above <- ggplot(humid_75, aes(days, minutes_above_85, color = status)) +
  geom_smooth(aes(fill = status, color = status), se = T, method = "lm", formula = y ~ (poly(x,2)), alpha = .2) +
  geom_point(data = humid_75,aes(y = minutes_above_85, color = status), alpha = 0.16) +
  #geom_errorbarh(aes(xmin = avg_min_rh - min_rh_se,
  invasion_color +
  invasion_fill +
  ylab("Minutes above 85% RH") +
  xlab("Days") +
  def_theme +
  guides(fill=FALSE, color=FALSE) +
  ggtitle(label = "Minutes above 85 % RH")  +
  #theme(plot.title = element_text(hjust = 0.5)) +
  #scale_y_continuous(limits = c(0, 1 )) +
  #expand_limits(x = 0, y = 0) +
  geom_hline(yintercept = 720, linetype = "dashed") +
  scale_x_continuous(limits = c(0,360), breaks = c(0, 50, 100, 150, 200, 250, 300, 350)) +
  scale_y_continuous(limits = c(0,1440), breaks = c(0, 500, 1000, 1400)) +
  NULL

ggsave(plot = humidity_test_75_above, height = 7, width = 7, "figures/tick-survival-assay/humidity_test_75_above.png")

humidity_test_75_bw <- ggplot(humid_75, aes(days, minutes_bw_75_85, color = status)) +
  geom_smooth(aes(fill = status, color = status), se = T, method = "lm", formula = y ~ (poly(x,2)), alpha = .2) +
  geom_point(data = humid_75,aes(y = minutes_bw_75_85, color = status), alpha = 0.16) +
  #geom_errorbarh(aes(xmin = avg_min_rh - min_rh_se,
  invasion_color +
  invasion_fill +
  ylab("Minutes between 75-85% RH") +
  xlab("Days") +
  def_theme +
  guides(fill=FALSE, color=FALSE) +
  ggtitle(label = "Minutes between 75-85% RH")  +
  #expand_limits(x = 0, y = 0) +
  geom_hline(yintercept = 720, linetype = "dashed") +
  scale_x_continuous(limits = c(0,350), breaks = c(0, 50, 100, 150, 200, 250, 300, 350)) +
  scale_y_continuous(limits = c(0,1440), breaks = c(0, 500, 1000, 1400)) +
  #theme(plot.title = element_text(hjust = 0.5)) +
  #scale_y_continuous(limits = c(0, 1 )) +
  NULL

ggsave(plot = humidity_test_75_bw, height = 7, width = 7, "figures/tick-survival-assay/humidity_test_75_bw.png")

humidity_test_75_below <- ggplot(humid_75, aes(days, minutes_below_75, color = status)) +
  geom_smooth(aes(fill = status, color = status), se = T, method = "lm", formula = y ~ (poly(x,2)), alpha = .2) +
  geom_point(data = humid_75,aes(y = minutes_below_75, color = status), alpha = 0.16) +
  #geom_errorbarh(aes(xmin = avg_min_rh - min_rh_se,
  invasion_color +
  invasion_fill +
  ylab("Minutes below 75% RH") +
  xlab("Days") +
  #expand_limits(x = 0, y = 0) +
  scale_x_continuous(limits = c(0,360), breaks = c(0, 50, 100, 150, 200, 250, 300, 350)) +
  scale_y_continuous(limits = c(0,1440), breaks = c(0, 500, 1000, 1400)) +
  geom_hline(yintercept = 720, linetype = "dashed") +
  def_theme +
  guides(fill=FALSE, color=FALSE) +
  ggtitle(label = "Minutes below 75 % RH")  +
  #theme(plot.title = element_text(hjust = 0.5)) +
  #scale_y_continuous(limits = c(0, 1 )) +
  NULL

ggsave(plot = humidity_test_75_below, height = 7, width = 7, "figures/tick-survival-assay/humidity_test_75_below.png")

######


humidity_what <- read_csv("data/processed_data/humidity_means_se_treatment.csv")

humidity_test <- ggplot(humidity_test, aes(days, mean_above_82,
                                                color = status)) +
  #geom_smooth(aes(fill = status, color = status), se = T, method = "lm", alpha = .2) +
  geom_point(data = humidity_intervals,aes(y = minutes_above_82), color = status) +
  #geom_errorbarh(aes(xmin = avg_min_rh - min_rh_se,
  #xmax = avg_min_rh + min_rh_se)) +
  invasion_color +
  invasion_fill +
  ylab(" ") +
  xlab("Avg min RH") +
  def_theme +
  guides(fill=FALSE, color=FALSE) +
  ggtitle(label = "Female Adults")  +
  #theme(plot.title = element_text(hjust = 0.5)) +
  #scale_y_continuous(limits = c(0, 1 )) +
  NULL

