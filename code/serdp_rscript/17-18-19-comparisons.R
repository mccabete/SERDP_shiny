#### 2019 comparisons to 17 18 serdp southeast data

library(stringr); library(ggplot2); library(dplyr); library(readr)

plot_visit <- read_csv("data/processed_data/plot_visit_data.csv")

summary(plot_visit)
na <- plot_visit %>% 
  filter(is.na(plot_visit$last_fire_year))

shelby <- plot_visit %>% 
  filter(installation=="shelby")

last_fire <- plot_visit %>% 
  filter(last_fire_year > 1900)

invaded <- last_fire %>% 
  filter(imcy_inv=="invaded")

unique(last_fire$installation)

old_ticks <- read_csv("data/processed_data/ticks.csv")

h1 <- filter(old_ticks, plot_id=="blanding h1")

old_ticks_stat <- old_ticks %>% 
  group_by(installation, plot_id, visit_year, life_stage) %>% 
  summarise(total_ticks = sum(count))
#^by visit year because visit date has many "extras" or "whalen" on random dates around the trapping date

unique(old_ticks_stat$plot_id)
unique(last_fire$plot_id)

old_plot_ticks <- left_join(old_ticks_stat, last_fire) %>% 
  ungroup(.)

old_plot_ticks <- old_plot_ticks[,-c(11:14)]
old_plot_ticks <- old_plot_ticks[,-c(9)]

write_csv(old_plot_ticks, "C:/Users/Steven/Desktop/testingrxnnorm.csv")
sort(unique(old_plot_ticks$plot_id))
sort(unique(last_fire$plot_id))

new_ticks <- read_csv("data/raw_data/2019_serdp_data/2019-only-tick-data.csv")

new_ticks <- new_ticks %>%
 mutate(date = lubridate::ymd(date),
        visit_year = lubridate::year(date))

colnames(new_ticks)[colnames(new_ticks)=="date"] <- "visit_date"

new_ticks$invaded[(new_ticks$invaded=="y")] <- "invaded"
new_ticks$invaded[(new_ticks$invaded=="n")] <- "native"

unique(new_ticks$plot_id)
new_ticks$plot_id[(new_ticks$plot_id=="i-1")] <- "i1"
new_ticks$plot_id[(new_ticks$plot_id=="i-2")] <- "i2"
new_ticks$plot_id[(new_ticks$plot_id=="i-3")] <- "i3"
new_ticks$plot_id[(new_ticks$plot_id=="i-4")] <- "i4"
new_ticks$plot_id[(new_ticks$plot_id=="i-5")] <- "i5"
new_ticks$plot_id[(new_ticks$plot_id=="n-1")] <- "n1"
new_ticks$plot_id[(new_ticks$plot_id=="n-2")] <- "n2"
new_ticks$plot_id[(new_ticks$plot_id=="n-3")] <- "n3"

colnames(new_ticks)[colnames(new_ticks)=="invaded"] <- "imcy_inv"

new_ticks <- new_ticks %>% 
     mutate(visit_date = lubridate::ymd(visit_date))

new_ticks_stat <- new_ticks %>% 
  group_by(installation, plot_id, imcy_inv, visit_date, life_stage) %>% 
  summarise(total_ticks = sum(count))

write_csv(new_ticks_stat, "C:/Users/Steven/Desktop/serdp/2019 data/tsa_ticks_test.csv")

new_ticks_manual_edit <- read_csv("C:/Users/Steven/Desktop/serdp/2019 data/tsa_ticks_test_edited.csv")

new_ticks_manual_edit[is.na(new_ticks_manual_edit)] <- 0

new_ticks_manual_edit <- new_ticks_manual_edit %>% 
  mutate(visit_date = lubridate::mdy(visit_date)) %>% 
  mutate(visit_year = lubridate::year(visit_date))

rbind.data.frame()

old_ticks_native_only <- old_plot_ticks %>% 
  filter(imcy_inv=="uninvaded")

all_ticks <- rbind.data.frame(old_plot_ticks, new_ticks_manual_edit)

all_ticks_regional_new <- rbind.data.frame(old_ticks_native_only, new_ticks_manual_edit)


#^was old_ticks_stat

all_ticks <- all_ticks %>% 
  mutate(visit_year = lubridate::year(visit_date)) %>% 
  #filter(plot_id !="shelby f1") %>% 
  #filter(plot_id !="shelby b1") %>% 
  filter(!is.na(all_ticks$imcy_inv))
  #mutate(years_since_fire = 2019-last_fire_year)

all_ticks$imcy_inv[(all_ticks$imcy_inv=="uninvaded")] <- "regional native"

all_ticks_regional_new$imcy_inv[(all_ticks_regional_new$imcy_inv=="uninvaded")] <- "regional native"


all_ticks_regional_new$visit_year[(all_ticks_regional_new$visit_year==2017)] <- 2017.5
all_ticks_regional_new$visit_year[(all_ticks_regional_new$visit_year==2018)] <- 2017.5

unique(all_ticks$imcy_inv)
unique(all_ticks$visit_year)

inv_old <- all_ticks %>% 
  filter(visit_year==2017) %>% 
  filter(imcy_inv=="invaded")

all_ticks$imcy_inv[(all_ticks$visit_year==2017 & all_ticks$imcy_inv=="invaded")] <- "weird"

all_ticks_w <- all_ticks %>% 
  filter(imcy_inv!="weird")




  

invasion_color <- scale_color_manual(values = c("red", "deepskyblue", "green","purple", "orange"))
invasion_fill <- scale_color_manual(values = c("red", "deepskyblue", "green"))
region_color <- scale_color_manual(values = c("green"))
life_stage_color <- scale_color_manual(values = c("purple", "orange"))

all_ticks_plot_year <- ggplot(data = all_ticks_regional_new, mapping = aes(x = visit_year, y = total_ticks, color = imcy_inv)) +
  geom_boxplot(outlier.size = NA, outlier.alpha = 0) + 
  geom_point(size = 3, alpha = .4, position = position_jitterdodge(), aes(color = life_stage)) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  labs (y = 'life_stage per plot') +
  invasion_color +
  xlab("year") +
  scale_x_continuous(breaks = c(2017,2018,2019)) +
  NULL

#adults only

adults <- all_ticks_regional_new %>% 
  filter(life_stage=="adult")

all_plot_year_adult <- ggplot(data = adults, mapping = aes(x = visit_year, y = total_ticks, color = imcy_inv)) +
  geom_boxplot(outlier.size = NA, outlier.alpha = 0) + 
  geom_point(size = 3, alpha = 0.5, position = position_jitterdodge()) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  labs (y = 'adults per plot') +
  invasion_color +
  xlab("year") +
  scale_x_continuous(breaks = c(2017,2018,2019)) +
  NULL

ggsave(plot = all_plot_year_adult, "C:/Users/Steven/Desktop/17-18-19-adults.png", width = 7, height = 7)

#nymphs only

nymphs <- all_ticks_regional_new %>% 
  filter(life_stage=="nymph")

all_plot_year_nmyph <- ggplot(data = nymphs, mapping = aes(x = visit_year, y = total_ticks, color = imcy_inv)) +
  geom_boxplot(outlier.size = NA, outlier.alpha = 0) + 
  geom_point(size = 3, alpha = 0.5, position = position_jitterdodge()) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  labs (y = 'nymphs per plot') +
  invasion_color +
  xlab("year") +
  scale_x_continuous(breaks = c(2017,2018,2019)) +
  NULL

ggsave(plot = all_plot_year_nymph, "C:/Users/Steven/Desktop/17-18-19-nymphs.png", width = 7, height = 7)

#years since fire etc

all_plot_fire_history <- ggplot(data = all_ticks, mapping = aes(x = years_since_fire, color = imcy_inv, fill = imcy_inv)) +
  geom_histogram(binwidth = 1) + 
  #geom_jitter(size = 3, alpha = 0.5, width = 0.15) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  labs (y = 'number of plots') +
  xlab("years since fire") +
  invasion_color +
  invasion_fill +
  #theme(legend.position="none") +
  #scale_x_continuous(breaks = c(2017,2018,2019)) +
  NULL

ggsave(plot = all_plot_fire_history, "C:/Users/Steven/Desktop/17-18-19-fire-history.png", width = 7, height = 7)

year_breakdown <- 
  ggplot() +
  geom_bar(data = all_ticks, aes(imcy_inv, color = imcy_inv, fill = imcy_inv), stat = "count", position = "dodge") +
  invasion_color +
  theme_bw() +
  NULL




###### playing with reaction norms etc 17 18 ticks fire burned on revistit ######

rxnnorm <- read_csv("C:/Users/Steven/Desktop/testingrxnnorm.csv")

filter(rxnnorm, imcy_inv=="invaded")

rxnnorm_stats <- rxnnorm %>% 
  group_by(installation, plot_id, imcy_inv, visit_year, years_since_fire, last_fire_year, burned_on_revisit) %>% 
  summarise(adult_plus_nymphs = sum(total_ticks)) 
  
  

rx_adult <- rxnnorm %>% 
  filter(life_stage=="adult")

rx_nymph <- rxnnorm %>% 
  filter(life_stage=="nymph")

ggplot(rxnnorm_stats, aes(x = visit_year, y = adult_plus_nymphs)) +
  stat_summary(aes(group = plot_id), fun.y = mean, geom = "path") +
  stat_summary(aes(color = years_since_fire), fun.data = mean_cl_boot, geom = "errorbar", width = 0.1) +
  stat_summary(aes(color = years_since_fire), fun.y = mean, geom = "errorbar", size = 4) +
  geom_point(aes(color = years_since_fire), size = 2) +
  scale_x_continuous(breaks = c(2017,2018))


rxnnorm_stats <- rxnnorm_stats %>% 
  ungroup(.)

ggplot(data = rxnnorm_stats, mapping = aes(x = visit_year, y = adult_plus_nymphs, group = visit_year)) +
  geom_boxplot(outlier.size = NA, outlier.alpha = 0) + 
  geom_jitter(size = 3, alpha = 0.9, width = 0.15, aes(color = years_since_fire)) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  labs (y = 'total ticks per plot') +
  xlab("visit year") +
  #theme(legend.position="none") +
  scale_x_continuous(breaks = c(2017,2018)) +
  NULL
