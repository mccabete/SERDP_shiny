###### running with correcte sd cards 11/19 ########

library(readr)
library(dplyr)
library(ggplot2)
library(stringi)

corrected <- read_csv("data/raw_data/2019_serdp_data/corrected_cards_new.csv")

corrected <- corrected %>% 
  mutate(first_date = stri_sub(first_photo, 1, 9),
         last_date = stri_sub(last_photo, 1, 9),
         cutoff_date = stri_sub(cut_off_date_time, 1, 9))
         
corrected <- corrected %>% 
  mutate(first_date = as.Date.character(first_date, "%m/%d/%Y"),
         last_date = as.Date.character(last_date, "%m/%d/%Y"),
         cutoff_date = as.Date.character(cut_off_date_time, "%m/%d/%Y"))

corrected <- corrected %>% 
  mutate(camera_days_raw = last_date - first_date) %>% 
  mutate(camera_days_raw = as.integer(camera_days_raw))

corrected <- corrected %>% 
  mutate(camera_days_cutoff = cutoff_date - first_date) %>% 
  mutate(camera_days_cutoff = as.integer(camera_days_cutoff))
#View(corrected)

corrected_stats <- corrected %>% 
  group_by(installation, plot_id, status) %>% 
   summarise(cutoff_deer_plot = sum(cutoff_deer_total),
            cutoff_cow_plot = sum(cutoff_cow_total),
            cutoff_turkey_plot = sum(cutoff_turkey_total),
            cutoff_pig_plot = sum(cutoff_pig_total),
            cutoff_raccoon_plot = sum(cutoff_raccoon_total),
            cutoff_fox_plot = sum(cutoff_fox_total),
            cutoff_wildlife_plot = sum(cutoff_deer_total, cutoff_cow_total, cutoff_pig_total, cutoff_turkey_total, cutoff_fox_total, cutoff_raccoon_total),
            wildlife_no_cow_plot = sum(cutoff_deer_total, cutoff_pig_total, cutoff_turkey_total, cutoff_fox_total, cutoff_raccoon_total),
            camera_days_raw_plot = sum(camera_days_raw),
            camera_days_cutoff_plot = sum(camera_days_cutoff))

write_csv(corrected_stats, "data/processed_data/2019 serdp processed data/corrected-host-tick-processed/corrected_raw_hosts_per_plot.csv")

corrected_new <- read_csv("data/processed_data/2019 serdp processed data/corrected-host-tick-processed/COMBINING_PLOTS_corrected_raw_hosts_per_plot.csv")
##### MANUAL COMBINATION OF wes i2 was combined with wes i3 for equal sampling during the same time period, done so because "same" site sharing space. riversedge combined i1 and i2, there was one camera in each so combined to compensate for the two cameras in n1. -SC
#^need to change name

corrected_stats_days <- corrected_new %>% 
  group_by(installation, plot_id, status) %>% 
   summarise(deer_plot_days = cutoff_deer_plot/camera_days_cutoff_plot,
     cow_plot_days = cutoff_cow_plot/camera_days_cutoff_plot,
     turkey_plot_days = cutoff_turkey_plot/camera_days_cutoff_plot,
     pig_plot_days = cutoff_pig_plot/camera_days_cutoff_plot,
     raccoon_plot_days = cutoff_raccoon_plot/camera_days_cutoff_plot,
     fox_plot_days = cutoff_fox_plot/camera_days_cutoff_plot,
     wildlife_plot_days = cutoff_wildlife_plot/camera_days_cutoff_plot,
     wildlife_no_cow_plot_days = wildlife_no_cow_plot/camera_days_cutoff_plot)

write_csv(corrected_stats_days, "data/processed_data/2019 serdp processed data/corrected-host-tick-processed/corected_hosts_per_plot_per_day.csv" )
            
tick_data <- read_csv("C://Users/Steven/Desktop/serdp/corrected_cards/tick_data.csv")

#### joined multiple dataframes to get tick data x host data. both averaged to the plot and single plot revisits included (more data points this way), also paired and unpaired data frames


invasion_color <- scale_color_manual(values = c("red", "deepskyblue"))
invasion_fill <- scale_color_manual(values = c("red", "deepskyblue"))


##begin testing installation only

revisit_pairs <- read_csv("C:/Users/Steven/Desktop/serdp/corrected_cards/resample_ticks_unpaired.csv")

revisit_pairs_inst <- revisit_pairs %>% 
  group_by(installation, status) %>% 
  summarise(inst_avg_adults = mean(total_adults_visit),
            inst_avg_nymphs = mean(total_nymphs_visit),
            inst_deer_total = sum(deer_plot_days),
            inst_wildlife_total = sum(wildlife_plot_days),
            inst_wildlife_no_cow_total = sum(wildlife_no_cow_plot_days)
            )
ggplot(data = revisit_pairs_inst, mapping = aes(x = inst_avg_nymphs, y = inst_deer_total, color= status)) +
  #geom_boxplot(outlier.size = NA, outlier.alpha = 0) +
  geom_point(size = 3, alpha = 0.5) +
  geom_smooth(method = "lm", se = T) +
  theme_classic() +
  theme(text = element_text(size=16)) +
  labs (y = 'Ticks/plot per installation') +
  xlab("Deer/day at installation") +
  invasion_color +
  invasion_fill +
  theme(axis.ticks.x = element_blank(), legend.position = "none", axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) +
  #annotate("text", x = 2, y = 8, label = "*", size = 7) +
  NULL


