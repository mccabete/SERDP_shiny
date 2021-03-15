####### 2019 plot report data analysis #####

#### load packages ####
library(plyr); library(dplyr); library(ggplot2); library(readr)
library(stringi); library(stringr)

#+read in data ####
tick_data <- read_csv(file = "data/raw_data/2019_serdp_data/2019-only-tick-data.csv")


tick_data <- tick_data %>% 
  filter(installation !="avonpark") %>% 
  mutate(date = as.Date(as.character(date), format = "%Y%m%d"))

tick_data <- tick_data %>% 
  mutate(invaded = if_else(invaded=="y", "invaded", "native"))


tail(tick_data)
unique(tick_data$installation)
unique(tick_data$plot_id)
unique(tick_data$life_stage)
unique(tick_data$location)


ticks_adult <- filter(tick_data, life_stage == "adult")
ticks_nymphs <- filter(tick_data, life_stage == "nymph")

all_ticks <- tick_data %>% 
  group_by(installation, plot_id, invaded, life_stage, date) %>% 
  summarise(ticks_per_plot = sum(count))

ticks_zero <- filter(all_ticks, ticks_per_plot == 0)
ticks_zero <- ticks_zero %>% 
  group_by(installation, plot_id, invaded, date) %>% 
  summarise(total_zero = sum(count))

adults <- ticks_adult %>%
  group_by(installation, plot_id, invaded, date) %>%
  summarise(total_adult = sum(count))

nymph <- ticks_nymphs %>%
  group_by(installation, plot_id, invaded, date) %>%
  summarise(total_nymph = sum(count))

adult_nymph_csv <- left_join(nymph,adults) %>% 
  ungroup(.) 

test_join <- left_join(ticks_zero, adult_nymph_csv)

ticks_zero <- filter(tick_data, life_stage =="NA")

write_csv(adult_nymph_csv, "data/processed_data/2019_total_ticks_plot.csv")
  
  
  group_by(installation, plot_id, invaded) %>% 
  summarise(adult_per_plot = mean(total_adult),
            nymph_per_plot = mean(total_nymph))


Maov<- aov(total_adult ~ invaded, data = adults)
summary(Maov)
Maov_2<- aov(total_nymph ~ invaded, data = nymph)
summary(Maov_2)


status <- tick_data %>% 
  group_by(invaded, life_stage) %>% 
  summarise(total=sum(count))

###### setting figure themes #####

def_theme <- theme(legend.title = element_blank(),
                   legend.text = element_text(size = 12),
                   legend.position = "top",
                   axis.text = element_text(size = 12),
                   axis.title = element_text(size = 16),
                   plot.title = element_text(size = 28),
                   strip.background = element_blank(),
                   panel.grid = element_blank(),
                   panel.background = element_blank())
invasion_color <- scale_color_manual(values = c("red", "deepskyblue"))
invasion_fill <- scale_color_manual(values = c("red", "deepskyblue"))
invasion_color_2 <- scale_color_manual(values = c("deepskyblue", "red", "green", "purple"))
invasion_fill_2 <- scale_color_manual(values = c("deepskyblue", "red", "green", "purple"))

##### working on tick figures by installation/status#####
####
adult_drew <- ggplot(adults, aes(invaded, total_adult, color = invaded, position = "jitter")) +
  geom_boxplot(outlier.size = NA, outlier.alpha = 0) + 
  geom_jitter(size = 3, alpha = 0.5, width = 0.15) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  labs (y = 'Adults per plot') +
  xlab("") +
  invasion_color +
  invasion_fill +
  theme(legend.position="none")+
  scale_y_continuous(limits = c(0,20))



sample_plot <- ggplot(adults,
       aes(invaded, total_adult, color = invaded)) +
  stat_summary(aes(color = invaded), fun.data = "mean_se", geom = "pointrange", size = 1) +
  geom_point(alpha = .5, size = 2, position = "jitter") +
  invasion_color +
  invasion_fill +
  theme_classic() +
  xlab("") +
  ylab("Adult ticks") +
  ggtitle("Adults ticks per plot") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

ggsave(plot = sample_plot, "C:/Users/Steven/Desktop/example_stat_summary.png", height = 7, width = 7)
write_csv(adults, "C:/Users/Steven/Desktop/example_stat_summary_adult_ticks.csv")



ggplot(adults,
       aes(invaded, total_adult, color = invaded)) +
  stat_summary(aes(color = invaded), fun.data = "mean_se", geom = "pointrange", size = 1) +
  geom_point(alpha = .5, size = 2, show.legend = F, position = "jitter") +
  invasion_color +
  invasion_fill +
  theme_classic() +
  xlab("Status") +
  ylab("Adults per plot") +
  def_theme 
  
nymph_drew <- ggplot(nymph, aes(invaded, total_nymph, color = invaded, position = "jitter")) +
  geom_boxplot(outlier.size = NA, outlier.alpha = 0) + 
  geom_jitter(size = 3, alpha = 0.5, width = 0.15) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  labs (y = 'Nymphs per plot') +
  xlab("") +
  invasion_color +
  invasion_fill +
  theme(legend.position="none")
  #scale_y_continuous(limits = c(0,40))+

# ####
# ggsave(plot = adult_drew, "C:/Users/Steven/Desktop/adult_ticks_2019.png", height = 4, width = 3)
# ggsave(plot = nymph_drew, "C:/Users/Steven/Desktop/nymph_ticks_2019.png", height = 3, width = 3)

adult_figure <- ggplot(ticks_adult, aes(date, count, color = invaded)) +
  geom_smooth(aes(color = invaded), method = "lm", alpha = .2) +
  geom_jitter(data = ticks_adult, aes(y = count, x = date, color = invaded)) +
  def_theme + 
  invasion_color +
  #invasion_fill + 
  xlab("Date") +
  ylab("Number of adults") +
  #scale_y_continuous(limits = c(0,5)) +
  NULL

nymph_figure <- ggplot(ticks_nymphs, aes(date, count, color = invaded)) +
  geom_smooth(aes(color = invaded), method = "lm", alpha = .2) +
  geom_jitter(data = ticks_nymphs, aes(y = count, x = date, color = invaded)) +
  def_theme + 
  #scale_x_date(date_breaks = "1 month", date_labels = "%m-%d-%Y") +
  #scale_x_continuous(limits = c(04-01-2019, 06-01-2019), breaks = c(04-01-2019, 05-01-2019, 06-01-2019)) +
  invasion_color +
  #invasion_fill + 
  xlab("Date") +
  ylab("Number of nymphs") + 
  #scale_y_continuous(limits = c(0,10)) +
  NULL

tick_data_no_brown <- tick_data %>% 
  filter(installation != "brown") %>% 
ggplot(tick_data, aes(invaded, count, color = invaded)) +
  geom_boxplot() +
  theme_bw()

ggplot(tick_data_no_brown, aes(invaded, count, color = invaded)) +
  geom_boxplot() +
  theme_bw()

####### tick abundance BY TRAP  
ggplot(tick_data, aes(invaded, count, color = invaded, position = "jitter")) +
                        geom_boxplot(outlier.size = NA) + 
                        geom_jitter(size = 3, alpha = 0.5, width = 0.15) +
                        theme_classic() +
                        theme(text = element_text(size=20)) +
                        labs (y = 'Ticks per trap') +
                        xlab("") +
                        annotate("text", x = 1.5, y = 50.5, label = "n.s.", size = 7) +
                        invasion_color +
                        invasion_fill +
                        theme(legend.position="none")+
  scale_y_continuous(limits = c(0,10))

status_count <- tick_data %>% 
  group_by(invaded) %>% 
  summarise(number_invaded = n())

ticks_per_plot <- tick_data %>% 
  group_by(installation, date, plot_id, invaded) %>% 
  summarise(ticks_per_plot = sum(count)) 

total_ticks_all <- tick_data %>% 
  group_by(installation, invaded) %>% 
  summarise(ticks_per_plot = sum(count)) 

total_ticks_all$invaded[total_ticks_all$invaded=="invaded"] <- "zummaryY"
total_ticks_all$invaded[total_ticks_all$invaded=="native"] <- "zummaryN"


ggplot(ticks_per_plot, aes(installation, ticks_per_plot, color = invaded)) +
  geom_boxplot() +
  geom_boxplot(data = total_ticks_all, aes(invaded, ticks_per_plot, color = invaded)) +
  invasion_color_2 +
  invasion_fill_2 +
  ylab("Ticks per plot") +
  xlab("Site") +
  theme_bw()

ggplot(total_ticks_all, aes(invaded, ticks_per_plot), color = invaded) +
  geom_boxplot() +
  theme_bw() +
  invasion_color +
  invasion_fill +
  NULL

###### final tick figure for export by drew #####

tick_figure_status <- ggplot(data = ticks_per_plot, mapping = aes(x = invaded, y = ticks_per_plot, color= invaded, position = "jitter")) +
  #scale_fill_manual(values=c("#E69F00", "#999999", "#56B4E9")) +
  geom_boxplot(outlier.size = NA) + 
  geom_jitter(size = 3, alpha = 0.5, width = 0.15) +
  #geom_boxplot(alpha = 0.5) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  labs (y = 'Ticks per plot') +
  xlab("") +
  annotate("text", x = 1.5, y = 50.5, label = "n.s.", size = 7) +
  invasion_color +
  invasion_fill +
  theme(legend.position="none") +
  geom_hline(yintercept=0, linetype="dashed", color = "gray", size=.25) 

ggsave(plot = tick_figure_status, "data/raw_data/2019_serdp_data/esa figures/tick_abundance.png", width = 7, height = 7)

#ggsave(plot = avg_time_days_temp_rh_barchart, "figures/tick-survival-assay/avg_time_days_temp_rh_barchart.png"

Maov<- aov(ticks_per_plot ~ invaded, data = ticks_per_plot)
summary(Maov)


ggplot(total_ticks_all, aes(installation, ticks_per_plot), color = invaded) +
  geom_boxplot() +
  invasion_color +
  NULL

###### post ESA looking at 17-18 tick data comparisons to 2019 #####

old_ticks <- read_csv("data/processed_data/ticks.csv")
plot_visit_data <- read_csv("data/processed_data/plot_visit_data.csv")

old_w_status <- left_join(old_ticks, plot_visit_data)
old_w_status <- old_w_status[,c(1:18)]
old_w_status <- old_w_status[,-c(9:17)]
unique(old_w_status$imcy_inv)
n_distinct(old_w_status)

colnames(old_w_status)[colnames(old_w_status)=="visit_date"] <- "date"
colnames(old_w_status)[colnames(old_w_status)=="installation.x"] <- "installation"

colnames(tick_data)[colnames(tick_data)=="invaded"] <- "imcy_inv"

tick_data <- tick_data[,-c(10:11)]
old_w_status

old_w_status$imcy_inv[old_w_status$imcy_inv=="NA"] <- "uninvaded"
#mutate_each(funs(replace(., which(is.na(.)), 1)))
old_w_status <- old_w_status %>% 
  mutate(imcy_inv = replace(imcy_inv, which(is.na(imcy_inv)), "uninvaded"))

unique(old_w_status$imcy_inv)
filter(old_w_status, is.na(imcy_inv))
filter(tick_data, is.na(imcy_inv))
serdp_ticks <- rbind(old_w_status, tick_data)
unique(serdp_ticks$imcy_inv)

unique(serdp_ticks$imcy_inv)
write_csv(serdp_ticks, "C:/Users/Steven/Desktop/serdp/2019 data/serdp_ticks_combined_2017_2019.csv")

serdp_ticks <- serdp_ticks %>% 
  mutate(visit_year = lubridate::year(date))
dod_ticks <- serdp_ticks %>% 
  filter(date < "2018-12-31") %>% 
  filter(invaded != "NA")
florida_ticks <- serdp_ticks %>% 
  filter(date > "2018-12-31")

ticks_on_dod <- ggplot(data = dod_ticks, mapping = aes(x = invaded, y = count, color= invaded, position = "jitter")) +
  geom_boxplot(outlier.size = NA) + 
  geom_jitter(size = 3, alpha = 0.5, width = 0.15) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  labs (y = 'Ticks per trap') +
  xlab("") +
  annotate("text", x = 1.5, y = 50.5, label = "n.s.", size = 7) +
  invasion_color +
  invasion_fill +
  #coord_cartesian(ylim =c(0,75)) +
  theme(legend.position="none") +
  ggtitle("Ticks on DoD") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0,20), breaks = c(0, 5, 10, 15, 20))

ticks_2019 <- ggplot(data = florida_ticks, mapping = aes(x = invaded, y = count, color= invaded, position = "jitter")) +
  geom_boxplot(outlier.size = NA) + 
  geom_jitter(size = 3, alpha = 0.5, width = 0.15) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  xlab("") +
  ylab("") +
  annotate("text", x = 1.5, y = 50.5, label = "n.s.", size = 7) +
  invasion_color +
  invasion_fill +
  theme(legend.position="none") +
  theme(axis.title.y = element_blank()) +
  ggtitle("2019 Florida Ticks") +
  theme(plot.title = element_text(hjust = 0.5)) +
  #coord_cartesian(ylim =c(0,75)) +
  scale_y_continuous(limits = c(0,20), breaks = c(0, 5, 10, 15, 20))

cowplot::plot_grid(ticks_on_dod, ticks_2019, ncol = 2)

###### begin assessing 2019 plot data for veg hts, bioass, canopy cover, species for covers etc ####

quadrat_data <- read_csv("data/processed_data/quadrat1m.csv")

biomass_data <- read_csv("data/processed_data/quadrat25cm.csv")

canopy_data <- read_csv("data/processed_data/canopy-cover.csv")

species_data <- read_csv("data/processed_data/species1m.csv")

quadrat_data_2019 <- quadrat_data %>% 
  filter(visit_year==2019) %>% 
  mutate(plot_id = as.character(stringr::str_sub(plot_id, -2, -1)),
         status_2 = stringr::str_sub(plot_id, 1, 2),
         status = if_else(status_2 == "n", "native", "invaded"))

biomass_data_2019 <- biomass_data %>% 
  filter(visit_year==2019) %>% 
  mutate(plot_id = as.character(stringr::str_sub(plot_id, -2, -1)),
         status_2 = stringr::str_sub(plot_id, 1, 2),
         status = if_else(status_2 == "n", "native", "invaded"))

canopy_data_2019 <- canopy_data %>% 
  filter(visit_year==2019) %>% 
  mutate(plot_id = as.character(stringr::str_sub(plot_id, -2, -1)),
         status_2 = stringr::str_sub(plot_id, 1, 2),
         status = if_else(status_2 == "n", "native", "invaded"))

species_data_2019 <- species_data %>% 
  filter(visit_year==2019) %>% 
  mutate(plot_id = as.character(stringr::str_sub(plot_id, -2, -1)),
         status_2 = stringr::str_sub(plot_id, 1, 2),
         status = if_else(status_2 == "n", "native", "invaded"),
         date = as.Date(date, "%m/%d/%Y"))

quadrat_data_2019[is.na(quadrat_data_2019)] <- 0

quadrat_stats <- quadrat_data_2019 %>% 
  group_by(installation, date, plot_id, transect_id, status) %>%
  filter(!is.na(woody_veg_ht1), !is.na(woody_veg_ht2), !is.na(woody_veg_ht3), !is.na(herb_veg_ht1), !is.na(herb_veg_ht2), !is.na(herb_veg_ht3), !is.na(litter_ht1), !is.na(litter_ht2), !is.na(litter_ht3), !is.na(pct_green)) %>%
  summarise(avg_woody_veg_ht = mean(woody_veg_ht1, woody_veg_ht2, woody_veg_ht3), 
            avg_herb_veg_ht = mean(herb_veg_ht1, herb_veg_ht2, herb_veg_ht3),
            avg_litter_ht = mean(litter_ht1, litter_ht2, litter_ht3),
            avg_pct_green = mean(pct_green))


quadrat_stats$status[quadrat_stats$plot_id=="n1"] <- "native"
quadrat_stats$status[quadrat_stats$plot_id=="n2"] <- "native"
quadrat_stats$avg_herb_veg_ht[quadrat_stats$avg_herb_veg_ht==454] <- 35


quadrat_stats_2 <- quadrat_stats %>% 
  group_by(installation, date, plot_id, status) %>% 
  summarise(plot_woody_veg_ht = mean(avg_woody_veg_ht),
            plot_herb_veg_ht = mean(avg_herb_veg_ht),
            plot_litter_ht = mean(avg_litter_ht),
            plot_pct_green = mean(avg_pct_green))
#quadrat1m stats complete ready for join

biomass_stats <- biomass_data_2019 %>% 
  group_by(installation, date, plot_id, status) %>% 
  summarise(plot_standing_dry = mean(standing_fuel_mass_dry),
            plot_litter_dry = mean(litter_mass_dry))
biomass_stats$status[biomass_stats$plot_id=="n1"] <- "native"
biomass_stats$status[biomass_stats$plot_id=="n2"] <- "native"
#quadrat25cm biomass stats ready to join

canopy_stats <- canopy_data_2019 %>% 
  group_by(installation, date, plot_id, status) %>% 
  summarise(mean_canopy_cover_dots = mean(fill_dots, na.rm = T),
            plot_canopy_cover_pct = mean_canopy_cover_dots*1.04)
canopy_stats$status[canopy_stats$plot_id=="n1"] <- "native"
canopy_stats$status[canopy_stats$plot_id=="n2"] <- "native"

species_data_2019[is.na(species_data_2019)] <- 0

species_stats <- species_data_2019 %>% 
  group_by(installation, date, plot_id, status, transect_id) %>% 
  summarise(total_quadrat_pct_cover = sum(pct_cover)) %>% 
  ungroup(.) %>% 
  group_by(installation, date, plot_id, status) %>% 
  summarise(plot_pct_plant_cover = sum(total_quadrat_pct_cover)/4)

species_data_2019[is.na(species_data_2019)] <- 0
species_stats$status[species_stats$plot_id=="n1"] <- "native"
species_stats$status[species_stats$plot_id=="n2"] <- "native"
#species stats done ready to merge all#

total_1 <- left_join(biomass_stats, quadrat_stats_2)
quadrat_data_all_2019_no_species <- left_join(canopy_stats, total_1) 
quadrat_data_all_2019 <- left_join(species_stats, quadrat_data_all_2019_no_species)
write_csv(quadrat_data_all_2019, "data/processed_data/2019_quadrat_biomass_canopy_analysis_non_removed_pair.csv")

###### SITE CONDITIONS figure making #####
####^ exported above all quadrat data to excel to manually remove plots that were not pairs (previously was all plots sampled. there were multiple invaded sites sampled at some locations but only one native for example. should be 8 total native-invaded paired plots)

quadrat_data_all_2019 <- read_csv("data/processed_data/2019_quadrat_biomass_canopy_analysis.csv")

quadrat_data_all_2019$status[quadrat_data_all_2019$status=="invaded"] <- "Invaded"
quadrat_data_all_2019$status[quadrat_data_all_2019$status=="native"] <- "Native"
#changing to capital letters for turkey

quadrat_data_all_2019 <- quadrat_data_all_2019 %>% 
  mutate(plot_standing_dry_m2 = plot_standing_dry*16,
         plot_litter_dry_m2 = plot_litter_dry*16)


#veg height-----
veg_ht <- ggplot(data = quadrat_data_all_2019, mapping = aes(x = status, y = plot_herb_veg_ht, color= status, position = "jitter")) +
  geom_boxplot(outlier.size = NA, outlier.alpha = 0) + 
  geom_jitter(size = 3, alpha = 0.5, width = 0.15) +
  theme_classic() +
  theme(text = element_text(size=20), axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) +  labs (y = 'Vegetation height (cm)') +
  xlab("") +
  invasion_color +
  invasion_fill +
  theme(legend.position="none") +
  scale_y_continuous(breaks = c(0,25,50,75,100,125), limits = c(0,130)) +
  annotate("text", x = 1.5, y = 117, label = "***", size = 7) +
  NULL

#biomass standing-----
biomass_standing <- ggplot(data = quadrat_data_all_2019, mapping = aes(x = status, y = plot_standing_dry_m2, color= status, position = "jitter")) +
  geom_boxplot(outlier.size = NA, outlier.alpha = 0) + 
  geom_jitter(size = 3, alpha = 0.5, width = 0.15) +
  theme_classic() +
  theme(text = element_text(size=20), axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) +  labs (y = quote('Standing biomass'~(g/m^2))) +
  xlab("") +
  invasion_color +
  invasion_fill +
  theme(legend.position="none") +
  scale_y_continuous(breaks = c(0,200,400,600,800,1000), limits = c(0,1120)) +
  annotate("text", x = 1.5, y = 1008, label = "*", size = 7) +
  NULL

#biomass litter-----
biomass_litter <- ggplot(data = quadrat_data_all_2019, mapping = aes(x = status, y = plot_litter_dry_m2, color= status, position = "jitter")) +
  geom_boxplot(outlier.size = NA, outlier.alpha = 0) + 
  geom_jitter(size = 3, alpha = 0.5, width = 0.15) +
  theme_classic() +
  theme(text = element_text(size=20), axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) +  labs (y = quote('Litter biomass'~(g/m^2))) +
  xlab("") +
  invasion_color +
  invasion_fill +
  theme(legend.position="none") +
  scale_y_continuous(breaks = c(0,250,500,750,1000), limits = c(0,1120)) +
  annotate("text", x = 1.5, y = 900, label = "n.s.", size = 7) +
  NULL

#canopy cover-----
canopy_cover <- ggplot(data = quadrat_data_all_2019, mapping = aes(x = status, y = plot_canopy_cover_pct, color= status, position = "jitter")) +
  geom_boxplot(outlier.size = NA, outlier.alpha = 0) + 
  geom_jitter(size = 3, alpha = 0.5, width = 0.15) +
  theme_classic() +
  theme(text = element_text(size=20), axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) +
  labs (y = 'Overstory canopy cover (%)') +
  xlab("") +
  invasion_color +
  invasion_fill +
  theme(legend.position="none") +
  scale_y_continuous(breaks = c(40,60,80,100), limits = c(0,100)) +
  annotate("text", x = 1.5, y = 65, label = "n.s.", size = 7) +
  NULL

#pct green, proxy for percent cover of tillers?-----
pct_green <- ggplot(data = quadrat_data_all_2019, mapping = aes(x = status, y = plot_pct_green, color= status, position = "jitter")) +
  geom_boxplot(outlier.size = NA, outlier.alpha = 0) + 
  geom_jitter(size = 3, alpha = 0.5, width = 0.15) +
  theme_classic() +
  theme(text = element_text(size=20), axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) +  labs (y = 'Percent green per plot (%)') +
  xlab("") +
  invasion_color +
  invasion_fill +
  theme(legend.position="none") +
  scale_y_continuous(limits = c(0,100))+
  #annotate("text", x = 1.5, y = 115, label = "***", size = 7) +
  NULL

pct_cover <- ggplot(data = quadrat_data_all_2019, mapping = aes(x = status, y = plot_pct_plant_cover, color= status, position = "jitter")) +
  geom_boxplot(outlier.size = NA, outlier.alpha = 0) + 
  geom_jitter(size = 3, alpha = 0.5, width = 0.15) +
  theme_classic() +
  theme(text = element_text(size=20), axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) +  labs (y = 'Plant cover (%)') +
  xlab("") +
  invasion_color +
  invasion_fill +
  theme(legend.position="none") +
  scale_y_continuous(limits = c(0,100)) +
  annotate("text", x = 1.5, y = 90, label = "**", size = 7) +
  NULL


green_vs_total <- cowplot::plot_grid(pct_green, pct_cover, ncol = 2)

Maov_canopy<- aov(plot_canopy_cover_pct ~ status, data = quadrat_data_all_2019)
summary(Maov_canopy)

Maov_litter_mass<- aov(plot_litter_dry_m2 ~ status, data = quadrat_data_all_2019)
summary(Maov_litter_mass)

Maov_standing_mass<- aov(plot_standing_dry_m2 ~ status, data = quadrat_data_all_2019)
summary(Maov_standing_mass)

Maov_veg_ht<- aov(plot_herb_veg_ht ~ status, data = quadrat_data_all_2019)
summary(Maov_veg_ht)

Maov_pct_green<- aov(plot_pct_green ~ status, data = quadrat_data_all_2019)
summary(Maov_pct_green)

Maov_pct_cover<- aov(plot_pct_plant_cover ~ status, data = quadrat_data_all_2019)
summary(Maov_pct_cover)

ggsave(plot = veg_ht, "data/raw_data/2019_serdp_data/esa figures/2019_veg_ht.png", height = 7, width = 7)

ggsave(plot = biomass_standing, "data/raw_data/2019_serdp_data/esa figures/2019_biomass_standing.png", height = 7, width = 7)

ggsave(plot = biomass_litter, "data/raw_data/2019_serdp_data/esa figures/2019_biomass_litter.png", height = 7, width = 7)

ggsave(plot = canopy_cover, "data/raw_data/2019_serdp_data/esa figures/2019_canopy_cover.png", height = 7, width = 7)

site_conditions <- egg::ggarrange(canopy_cover, veg_ht, pct_cover, biomass_standing, ncol = 2)

ggsave(plot = site_conditions, "data/raw_data/2019_serdp_data/esa figures/2019_site_conditions.png", height = 8, width = 8)

#^ with all installation and summary box
ggplot(quadrat_stats_2, aes(installation, plot_herb_veg_ht, color = status)) +
  geom_boxplot() +
  geom_boxplot(data = quadrat_stats_all, aes(status, plot_herb_veg_ht, color = status)) +
  invasion_color_2 +
  theme_bw() +
  NULL

ggplot(quadrat_stats_2, aes(status, plot_herb_veg_ht, color = status)) +
  geom_boxplot(outlier.colour = "black", outlier.shape = 24) +
  geom_point() +
  invasion_color +
  theme_bw() +
  NULL

quadrat_stats_all <- quadrat_stats %>% 
  group_by(installation, status) %>% 
  summarise(plot_herb_veg_ht = mean(avg_herb_veg_ht)) 

quadrat_stats_all$status[quadrat_stats_all$status=="invaded"] <- "zummaryI"
quadrat_stats_all$status[quadrat_stats_all$status=="native"] <- "zummaryN"


##### reading in secondary logger info #####
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
  #filter(tempC > 10) %>% 
  #filter(RH > 20) %>% 
  mutate(installation = "test",
         plot_id = "test",
         status = "n")

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
  mutate(days = julian(date, origin = as.Date("2019-06-12"))) %>% 
  group_by(date, installation, plot_id, status, logger_id, days) %>% 
  summarise(daily_min_rh = min(RH))

loggers_hancock_below <- loggers_hancock %>% 
  filter(RH < 79.99) %>%
  group_by(date, installation, plot_id, status, logger_id) %>%
  summarise(obs_below_75 = n(),
            minutes_below_75 = obs_below_75*5,
            hours_below_75 = minutes_below_75/60) %>%
  select(date, installation, status, plot_id, logger_id, hours_below_75)

loggers_hancock_above <- loggers_hancock %>%
  filter(RH >80.01) %>%
  group_by(date, installation, plot_id, status, logger_id) %>%
  summarise(obs_above_85 = n(),
            minutes_above_85 = obs_above_85*5,
            hours_above_85 = minutes_above_85/60) %>%
  select(date, installation, status, plot_id, logger_id, hours_above_85)

hancock_thresholds_1 <- left_join(loggers_hancock_above, loggers_hancock_below)
hancock_thresholds <- left_join(hancock_thresholds_1, loggers_hancock_min)

hancock_thresholds[is.na(hancock_thresholds)] <- 0

write_csv(hancock_thresholds, "data/processed_data/2019 serdp processed data/logger-sampling/hancock_thresholds.csv")

####riversedge
loggers_riversedge_min <- loggers_riversedge %>% 
  mutate(days = julian(date, origin = as.Date("2019-06-12"))) %>% 
  group_by(date, installation, plot_id, status, logger_id, days) %>%   summarise(daily_min_rh = min(RH))

loggers_riversedge_below <- loggers_riversedge %>% 
  filter(between(RH, 20, 79.99)) %>%
  group_by(date, installation, plot_id, status, logger_id) %>%
  summarise(obs_below_75 = n(),
            minutes_below_75 = obs_below_75*5,
            hours_below_75 = minutes_below_75/60) %>%
  select(date, installation, status, plot_id, logger_id, hours_below_75)

loggers_riversedge_above <- loggers_riversedge %>%
  filter(RH >80.01) %>%
  group_by(date, installation, plot_id, status, logger_id) %>%
  summarise(obs_above_85 = n(),
            minutes_above_85 = obs_above_85*5,
            hours_above_85 = minutes_above_85/60) %>%
  select(date, installation, status, plot_id, logger_id, hours_above_85)

riversedge_thresholds_1 <- left_join(loggers_riversedge_above, loggers_riversedge_below)
riversedge_thresholds <- left_join(riversedge_thresholds_1, loggers_riversedge_min)

riversedge_thresholds[is.na(riversedge_thresholds)] <- 0

write_csv(riversedge_thresholds, "data/processed_data/2019 serdp processed data/logger-sampling/riversedge_thresholds.csv")

####brown

loggers_brown_min <- loggers_brown %>% 
  mutate(days = julian(date, origin = as.Date("2019-06-12"))) %>% 
  group_by(date, installation, plot_id, status, logger_id, days) %>%   summarise(daily_min_rh = min(RH))

loggers_brown_below <- loggers_brown %>% 
  filter(between(RH, 20, 79.99)) %>%
  group_by(date, installation, plot_id, status, logger_id) %>%
  summarise(obs_below_75 = n(),
            minutes_below_75 = obs_below_75*5,
            hours_below_75 = minutes_below_75/60) %>%
  select(date, installation, status, plot_id, logger_id, hours_below_75)

loggers_brown_above <- loggers_brown %>%
  filter(RH >80.01) %>%
  group_by(date, installation, plot_id, status, logger_id) %>%
  summarise(obs_above_85 = n(),
            minutes_above_85 = obs_above_85*5,
            hours_above_85 = minutes_above_85/60) %>%
  select(date, installation, status, plot_id, logger_id, hours_above_85)

brown_thresholds_1 <- left_join(loggers_brown_above, loggers_brown_below)
brown_thresholds <- left_join(brown_thresholds_1, loggers_brown_min)

brown_thresholds[is.na(brown_thresholds)] <- 0

write_csv(brown_thresholds, "data/processed_data/2019 serdp processed data/logger-sampling/brown_thresholds.csv")

####silversprings

loggers_silversprings_min <- loggers_silversprings %>% 
  mutate(days = julian(date, origin = as.Date("2019-06-12"))) %>% 
  group_by(date, installation, plot_id, status, logger_id, days) %>%   summarise(daily_min_rh = min(RH))

loggers_silversprings_below <- loggers_silversprings %>% 
  filter(RH < 79.99) %>%
  group_by(date, installation, plot_id, status, logger_id) %>%
  summarise(obs_below_75 = n(),
            minutes_below_75 = obs_below_75*5,
            hours_below_75 = minutes_below_75/60) %>%
  select(date, installation, status, plot_id, logger_id, hours_below_75)

loggers_silversprings_above <- loggers_silversprings %>%
  filter(RH >80.01) %>%
  group_by(date, installation, plot_id, status, logger_id) %>%
  summarise(obs_above_85 = n(),
            minutes_above_85 = obs_above_85*5,
            hours_above_85 = minutes_above_85/60) %>%
  select(date, installation, status, plot_id, logger_id, hours_above_85)

silversprings_thresholds_1 <- left_join(loggers_silversprings_above, loggers_silversprings_below)
silversprings_thresholds <- left_join(silversprings_thresholds_1, loggers_silversprings_min)

silversprings_thresholds[is.na(silversprings_thresholds)] <- 0

write_csv(silversprings_thresholds, "data/processed_data/2019 serdp processed data/logger-sampling/silversprings_thresholds.csv")

####munson

loggers_munson_min <- loggers_munson %>% 
  mutate(days = julian(date, origin = as.Date("2019-06-12"))) %>% 
  group_by(date, installation, plot_id, status, logger_id, days) %>%   summarise(daily_min_rh = min(RH))

loggers_munson_below <- loggers_munson %>% 
  filter(between(RH, 20, 79.99)) %>%
  group_by(date, installation, plot_id, status, logger_id) %>%
  summarise(obs_below_75 = n(),
            minutes_below_75 = obs_below_75*5,
            hours_below_75 = minutes_below_75/60) %>%
  select(date, installation, status, plot_id, logger_id, hours_below_75)

loggers_munson_above <- loggers_munson %>%
  filter(RH >80.01) %>%
  group_by(date, installation, plot_id, status, logger_id) %>%
  summarise(obs_above_85 = n(),
            minutes_above_85 = obs_above_85*5,
            hours_above_85 = minutes_above_85/60) %>%
  select(date, installation, status, plot_id, logger_id, hours_above_85)

munson_thresholds_1 <- left_join(loggers_munson_above, loggers_munson_below)
munson_thresholds <- left_join(munson_thresholds_1, loggers_munson_min)

munson_thresholds[is.na(munson_thresholds)] <- 0

write_csv(munson_thresholds, "data/processed_data/2019 serdp processed data/logger-sampling/munson_thresholds.csv")

####peace

loggers_peace_min <- loggers_peace %>% 
  mutate(days = julian(date, origin = as.Date("2019-06-12"))) %>% 
  group_by(date, installation, plot_id, status, logger_id, days) %>%   summarise(daily_min_rh = min(RH))

loggers_peace_below <- loggers_peace %>% 
  filter(between(RH, 20, 79.99)) %>%
  group_by(date, installation, plot_id, status, logger_id) %>%
  summarise(obs_below_75 = n(),
            minutes_below_75 = obs_below_75*5,
            hours_below_75 = minutes_below_75/60) %>%
  select(date, installation, status, plot_id, logger_id, hours_below_75)

loggers_peace_above <- loggers_peace %>%
  filter(RH >80.01) %>%
  group_by(date, installation, plot_id, status, logger_id) %>%
  summarise(obs_above_85 = n(),
            minutes_above_85 = obs_above_85*5,
            hours_above_85 = minutes_above_85/60) %>%
  select(date, installation, status, plot_id, logger_id, hours_above_85)

peace_thresholds_1 <- left_join(loggers_peace_above, loggers_peace_below)
peace_thresholds <- left_join(peace_thresholds_1, loggers_peace_min)

peace_thresholds[is.na(peace_thresholds)] <- 0

write_csv(peace_thresholds, "data/processed_data/2019 serdp processed data/logger-sampling/peace_thresholds.csv")

###wes

loggers_wes_min <- loggers_wes %>% 
  mutate(days = julian(date, origin = as.Date("2019-06-12"))) %>% 
  group_by(date, installation, plot_id, status, logger_id, days) %>%   summarise(daily_min_rh = min(RH))

loggers_wes_below <- loggers_wes %>% 
  filter(between(RH, 20, 79.99)) %>%
  group_by(date, installation, plot_id, status, logger_id) %>%
  summarise(obs_below_75 = n(),
            minutes_below_75 = obs_below_75*5,
            hours_below_75 = minutes_below_75/60) %>%
  select(date, installation, status, plot_id, logger_id, hours_below_75)

loggers_wes_above <- loggers_wes %>%
  filter(RH >80.01) %>%
  group_by(date, installation, plot_id, status, logger_id) %>%
  summarise(obs_above_85 = n(),
            minutes_above_85 = obs_above_85*5,
            hours_above_85 = minutes_above_85/60) %>%
  select(date, installation, status, plot_id, logger_id, hours_above_85)

wes_thresholds_1 <- left_join(loggers_wes_above, loggers_wes_below)
wes_thresholds <- left_join(wes_thresholds_1, loggers_wes_min)

wes_thresholds[is.na(wes_thresholds)] <- 0

write_csv(wes_thresholds, "data/processed_data/2019 serdp processed data/logger-sampling/wes_thresholds.csv")

####end processing each indiviudal installation for sep csv's for drew analysis 



loggers_by_status <- loggers %>% 
  group_by(status, date) %>% 
  summarise(mean_tempC = mean(tempC),
            mean_rh = mean(RH))

write_csv(loggers_by_status, "data/raw_data/2019_serdp_data/scripts/loggers_by_status.csv")

#figure for average rh all time
ggplot(loggers_by_status, aes(date, mean_rh, color = status)) +
  geom_smooth(method = "lm", formula = y ~ (poly(x,3)), alpha = .2) +
  geom_point() +
  invasion_color +
  theme_bw() +
  NULL

#figure for average temp all time
ggplot(loggers_by_status, aes(date, mean_tempC, color = status)) +
  geom_smooth() +
  geom_point() +
  invasion_color +
  theme_bw() +
  NULL

loggers_by_logger <- loggers %>% 
  group_by(logger_id, status, date) %>% 
  summarise(min_daily_rh = min(RH)) %>% 
  ungroup(.) %>% 
  group_by(status, date) %>% 
  summarise(avg_daily_min_rh = mean(min_daily_rh)) %>% 
  select(status, date, avg_daily_min_rh)

write_csv(loggers_by_logger, "data/raw_data/2019_serdp_data/scripts/loggers_by_logger.csv")

#figure for daily min rh 
ggplot(loggers_by_logger, aes(date, avg_daily_min_rh, color = status)) +
  geom_smooth(method = "lm", formula = y ~ (poly(x,3)), alpha = .2) +
  geom_point() +
  invasion_color +
  theme_bw() +
  NULL

#starting analysis on time below/above 

thres_1 <- logger_thresholds_below_75 <- loggers %>%
  filter(between(RH, 20, 74.99)) %>%
  group_by(date, status, logger_id) %>%
  summarise(obs_below_75 = n(),
            minutes_below_75 = obs_below_75*5) %>%
  select(date, status, logger_id, minutes_below_75)

thres_2 <- logger_thresholds_bw_75_85 <- loggers %>%
  filter(between(RH, 75, 85)) %>%
  group_by(date, status, logger_id) %>%
  summarise(obs_bw_75_85 = n(),
            minutes_bw_75_85 = obs_bw_75_85*5) %>%
  select(date, status, logger_id, minutes_bw_75_85)

thres_3 <- logger_thresholds_above_85 <- loggers %>%
  filter(RH >85.01) %>%
  group_by(date, status, logger_id) %>%
  summarise(obs_above_85 = n(),
            minutes_above_85 = obs_above_85*5) %>%
  select(date, status, logger_id, minutes_above_85)

threshold_75 <- left_join(thres_2, thres_1)
humidity_75 <- left_join(thres_3, threshold_75)

humidity_75 <- humidity_75 %>% 
  mutate(days = julian(date, origin = as.Date("2019-06-12")))

##^save that as csv later for drew
write_csv(humidity_75, "data/raw_data/2019_serdp_data/scripts/esa_serdp_thresholds.csv")

#above 85
ggplot(humidity_75, aes(days, minutes_above_85, color = status)) +
  geom_smooth(aes(fill = status, color = status), se = T, method = "lm", formula = y ~ (poly(x,2)), alpha = .2) +
  geom_point(data = humidity_75, aes(y = minutes_above_85, color = status), alpha = 0.16) +
  #geom_errorbarh(aes(xmin = avg_min_rh - min_rh_se,
  invasion_color +
  #invasion_fill +
  ylab("Minutes above 85% RH") +
  xlab("Days") +
  def_theme +
  guides(fill=FALSE, color=FALSE) +
  ggtitle(label = "Minutes above 85 % RH")  +
  #theme(plot.title = element_text(hjust = 0.5)) +
  #scale_y_continuous(limits = c(0, 1 )) +
  #expand_limits(x = 0, y = 0) +
  geom_hline(yintercept = 720, linetype = "dashed") +
  scale_x_continuous(limits = c(0,50), breaks = c(0, 10, 20, 30, 40, 50)) +
  scale_y_continuous(limits = c(0,1440), breaks = c(0, 500, 1000, 1400)) +
  NULL

#below 75
ggplot(humidity_75, aes(days, minutes_below_75, color = status)) +
  geom_smooth(aes(fill = status, color = status), se = T, method = "lm", formula = y ~ (poly(x,2)), alpha = .2) +
  geom_point(data = humidity_75, aes(y = minutes_below_75, color = status), alpha = 0.16) +
  #geom_errorbarh(aes(xmin = avg_min_rh - min_rh_se,
  invasion_color +
  #invasion_fill +
  ylab("minutes_below_75") +
  xlab("Days") +
  def_theme +
  guides(fill=FALSE, color=FALSE) +
  ggtitle(label = "minutes_below_75")  +
  #theme(plot.title = element_text(hjust = 0.5)) +
  #scale_y_continuous(limits = c(0, 1 )) +
  #expand_limits(x = 0, y = 0) +
  geom_hline(yintercept = 720, linetype = "dashed") +
  scale_x_continuous(limits = c(0,50), breaks = c(0, 10, 20, 30, 40, 50)) +
  scale_y_continuous(limits = c(0,1440), breaks = c(0, 500, 1000, 1400)) +
  NULL

#bw 75-85

ggplot(humidity_75, aes(days, minutes_bw_75_85, color = status)) +
  geom_smooth(aes(fill = status, color = status), se = T, method = "lm", formula = y ~ (poly(x,2)), alpha = .2) +
  geom_point(data = humidity_75, aes(y = minutes_bw_75_85, color = status), alpha = 0.16) +
  #geom_errorbarh(aes(xmin = avg_min_rh - min_rh_se,
  invasion_color +
  #invasion_fill +
  ylab("minutes_bw_75_85") +
  xlab("Days") +
  def_theme +
  guides(fill=FALSE, color=FALSE) +
  ggtitle(label = "minutes_bw_75_85")  +
  #theme(plot.title = element_text(hjust = 0.5)) +
  #scale_y_continuous(limits = c(0, 1 )) +
  #expand_limits(x = 0, y = 0) +
  geom_hline(yintercept = 720, linetype = "dashed") +
  scale_x_continuous(limits = c(0,50), breaks = c(0, 10, 20, 30, 40, 50)) +
  scale_y_continuous(limits = c(0,1440), breaks = c(0, 500, 1000, 1400)) +
  NULL

loggers_by_status_2 <- loggers %>% 
  group_by(status) %>% 
  summarise(mean_tempC = mean(tempC),
            mean_rh = mean(RH))

####### begin looking at dung data for 2019 host abundance####

dung_data <- read_csv("data/processed_data/dung.csv")

summary(dung_data)
tail(dung_data)

dung_2019 <- dung_data %>%
  filter(visit_year == 2019)

dung_2019 <- dung_2019 %>%
  mutate(test = substr(plot_id, 1,1)) %>% 
  mutate(status = if_else(test =="i", "invaded", "native"))
dung_2019 <- dung_2019[,-c(10)]

dung_2019_species <- dung_2019 %>% 
  filter(installation != "avonpark") %>% 
  group_by(installation, plot_id, status, species) %>% 
  summarise(total_1m = sum(dung1m),
            total_2m = sum(dung2m),
            plot_total = sum(dung1m, dung2m))

dung_2019_species <- dung_2019_species %>% 
  filter(species!="armadillo") %>% 
  filter(species!="gopher tortoise") %>% 
  filter(species!="turkey") %>% 
  filter(species!="turkey eggs") %>% 
  filter(species!="horse") %>% 
  filter(species!="racoon") 

dung_2019_species$species[dung_2019_species$species=="rabbit"] <- "cottontail"
  
  

ggplot(dung_2019_species, aes(species, plot_total, color = status, position = "dodge")) +
  geom_boxplot() +
  geom_point(position = "jitter") +
  #geom_boxplot(data = dung_2019_species, aes(status, total_1m, color = status)) +
  invasion_color +
  invasion_fill +
  ylab("Plot total dung counts") +
  xlab("Site") +
  theme_bw()

ggplot(dung_2019_species, aes(species, total_1m, color = status)) +
  geom_boxplot() +
  #geom_boxplot(data = dung_2019, aes(status, total_1m, color = status)) +
  invasion_color +
  invasion_fill +
  ylab("Dung in 1m transects/plot") +
  xlab("species") +
  theme_bw()


unique(ticks_per_plot$installation)
unique(dung_2019_species$installation)

ticks_per_plot_2 <- ticks_per_plot %>% 
  mutate(
    first = substr(plot_id, 1,1),
    second = substr(plot_id, 3,3))

ticks_per_plot_2$plot_id <- paste(ticks_per_plot_2$first, ticks_per_plot_2$second, sep = "")
ticks_per_plot_2 <- ticks_per_plot_2[,-c(6:7)]

ticks_x_dung_species <- left_join(dung_2019, ticks_per_plot_2)

ggplot(ticks_x_dung, aes(total_1m, ticks_per_plot, color = status, fill = status)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_jitter() +
  theme_bw() +
  invasion_color +
  invasion_fill +
  scale_y_continuous(limits = c(0,60)) +
  scale_x_continuous(limits = c(0,200)) +
  NULL

#### "dung_2019_species" dataframe not good? for making figures with dung because multiple duplicate rows for species, also the multiple trap visit on a certain plot but only one dung data entry for the plot

dung_2019_all <- dung_2019 %>%
  filter(installation != "avonpark") %>%
  group_by(installation, plot_id, status) %>% 
  summarise(all_dung = sum(dung1m, dung2m))

ticks_all_2 <- tick_data %>% 
  mutate(
    first = substr(plot_id, 1,1),
    second = substr(plot_id, 3,3))
ticks_all_2$plot_id <- paste(ticks_all_2$first, ticks_all_2$second, sep = "")
ticks_all_2 <- ticks_all_2[,-c(6:7)]

ticks_2019_all <- ticks_all_2 %>% 
  group_by(installation, plot_id, invaded) %>% 
  summarise(all_ticks = sum(count))
colnames(ticks_2019_all)[colnames(ticks_2019_all)=="invaded"] <- "status"

ticks_x_dung_all <- left_join(dung_2019_all, ticks_2019_all)

anti_join(ticks_2019_all, dung_2019_all)

ggplot(ticks_x_dung_all, aes(all_dung, all_ticks, color = status, fill = status)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_jitter() +
  theme_bw() +
  invasion_color +
  invasion_fill +
  #scale_y_continuous(limits = c(0,60)) +
  #scale_x_continuous(limits = c(0,200)) +
  NULL
