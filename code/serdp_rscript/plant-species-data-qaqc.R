#' # Script for doing QA/QC on the plant species data

#' # Script for doing QA/QC on the canopy cover data

#+ load packages ####
library(plyr); library(dplyr);
library(readr)
library(stringi)
library(ggplot2)
library(tidyverse)

#+ load processed plot visit data ####
plot_visit_data <- read_csv("data/processed_data/plot_visit_data.csv")

#' ## Plant species data recorded in 1-meter quadrats
#'
#+ species data ####
species_data <- read_csv("data/raw_data/species1m.csv")
species_data <- select(species_data, Order:functional_group__1)

species_data$plot_id[species_data$plot_id=="cogon"] <- "theater_cogon"

## Steven changed blanding cogon to "theater_cogon" ###

species_data$date <- as.Date(as.character(species_data$date), format = "%Y%m%d")
species_data <- species_data %>%
      filter(date>"2017-06-01") %>%
      mutate(visit_year = lubridate::year(date),
             plot_id = paste(installation, plot_id, sep = " "))
species_data$veg_id <- tolower(species_data$veg_id)
length(unique(species_data$veg_id))

# species_data <- left_join(
#       species_data,
#       select(plot_visit_data, -notes)
# )

summary(species_data)

## Steven begin processing here ##

unique(species_data$plot_id)

filter(species_data, is.na(pct_cover)) %>%
  select(Order, date, plot_id, transect_id, veg_id, pct_cover, ht_under50cm)

species_data$pct_cover[species_data$Order=="2289"] <- 25
species_data$ht_over100cm[species_data$Order=="2289"] <- 1

species_data$pct_cover[species_data$Order=="2458"] <- 3

species_data$pct_cover[species_data$Order=="2583"] <- 6

species_data$pct_cover[species_data$Order=="2808"] <- 1
species_data$pct_cover[species_data$Order=="2809"] <- 1

species_data$pct_cover[species_data$Order=="3031"] <- 5

## Added missing pct cover & hts, "pea" shelby e1 north is true NA ##

summary(species_data)


sort(unique(species_data$scientific_name))
species_data$scientific_name <- paste(species_data$Genus,species_data$Species, sep=" ")
sort(unique(species_data$scientific_name))
## Fixed Genus/Species naming errors ##

#### steven adding species functional groups from elena ####

names(species_data)
summary(species_data)
unique(species_data$functional_group)

species_data$functional_group[species_data$Order=="2493"] <- "forb"
species_data$functional_group[species_data$Order=="2925"] <- "forb"
species_data$functional_group[species_data$Order=="2228"] <- "forb"
species_data$functional_group[species_data$Order=="2641"] <- "forb"
species_data$functional_group[species_data$Order=="2477"] <- "forb"
species_data$functional_group[species_data$Order=="3029"] <- "shrub"

species_data$functional_group[species_data$Order=="2606"] <- "forb"
species_data$functional_group[species_data$Order=="2487"] <- "forb"
species_data$functional_group[species_data$Order=="2486"] <- "forb"
species_data$functional_group[species_data$Order=="2544"] <- "forb"

# order # 2048 and 3507 still unknown

species_data <- species_data %>%
      mutate(plot_id = case_when(
            plot_id=="gordon z1" ~ "gordon a1",
            plot_id=="gordon y1" ~ "gordon b1",
            plot_id=="gordon x1" ~ "gordon c1",
            plot_id=="gordon w1" ~ "gordon d1",
            plot_id=="gordon v1" ~ "gordon e1",
            plot_id=="gordon t1" ~ "gordon f1",
            plot_id=="gordon s1" ~ "gordon g1",
            plot_id=="gordon r1" ~ "gordon h1",
            TRUE ~ plot_id
      ))

unique(species_data$plot_id)

write_csv(species_data, "data/processed_data/species1m.csv")

### Steven stopped processing here ####

species_data <- read_csv("data/processed_data/species1m.csv")
summary(species_data)

sort(unique(species_data$functional_group))
d <- filter(species_data, is.na(functional_group)) %>%
      select(plot_id, veg_id, Notes, visit_year)
unique(d$veg_id)

d2 <- filter(species_data, pct_cover>=5)
filter(d2, is.na(functional_group))$veg_id
n_distinct(d2$functional_group)

sort(unique(species_data$functional_group__1))
sort(unique(species_data$veg_id))


#################
#### NOT RUN ####
#############################################################################
# species_sub <- (filter(species_data, quadrat_id %in% c("e10","w10","n10","s10")))
# length(unique(species_sub$veg_id))
# summary(species_sub)
#
# ## Testing for differences in dense vs. less dense data collection
# hist(log(species_data$pct_cover))
# var(log(species_data$pct_cover))
# hist(log(species_sub$pct_cover))
# var(log(species_sub$pct_cover))
# t.test(log(species_data$pct_cover), log(species_sub$pct_cover),
#        var.equal = T, paired = F)


# species_over_5pct <- filter(species_data, pct_cover>5)
# # species_sub_over_5pct <- filter(species_sub_over_5pct, cogongrass != "NA")
#
# ggplot(species_over_5pct,
#        aes(years_since_fire, pct_cover, color = veg_id)) +
#       geom_point(position="jitter") +
#       theme_bw() +
#       facet_grid(installation~imcy_inv) +
#       ylab("% cover") +
#       xlab("Years since fire") +
#       ggtitle("Range of species cover (>5%) at 1-meter quadrats")
# ggsave("~/Dropbox (UF)/SERDP-Project/figures/species-pct-cover.png",
#        height = 7)

# # datac$se <- datac$sd / sqrt(datac$N)
# avg_species_cover <- species_data %>%
#       group_by(installation, imcy_inv, years_since_fire) %>%
#       summarise(avg_pct_cover = mean(pct_cover),
#                 # n_obs = length(veg_id),
#                 max_pct_cover = max(pct_cover),
#                 se = sd(pct_cover)/sqrt(length(pct_cover)))
# ggplot(avg_species_cover,
#        aes(as.factor(years_since_fire), avg_pct_cover, fill = imcy_inv)) +
#       geom_bar(stat = "identity", position = "dodge") +
#       geom_errorbar(aes(ymin = avg_pct_cover + se, ymax = avg_pct_cover - se),
#                     width = .2, position=position_dodge(width = .9)) +
#       facet_grid(installation~.) +
#       theme_bw() +
#       ylab("Average % cover") +
#       xlab("Years since fire") +
#       ggtitle("Averaged % cover of vegetation for each plot",
#               subtitle = "averaged across 1-meter quadrats")
# ggsave("~/Dropbox (UF)/SERDP-Project/figures/avg-pct-cover-bars.png",
#        height = 7)

# species_sub <- (filter(species_data2, !quadrat_id %in% c("w2","w4","n2","n4","s2","s4","e2","e4")))
# species_sub_plot <- ggplot(species_sub %>%
#                              filter(pct_cover > 5) %>%
#                              group_by(plot_id, veg_id) %>%
#                              summarise(avg_pct_cover = mean(pct_cover),
#                                        occurrence = length(veg_id)),
#       aes(veg_id,avg_pct_cover, label = occurrence))

# species_sub_plot +
#       geom_bar(stat = "identity") +
#       facet_grid(plot_id ~.) +
#       theme_bw() +
#       ggtitle("12 quadrats per plot")

# dbh_census_years %>%
# group_by(sample_year, speciesid, stem_status) %>%
# summarise(occurrence = length(unique(plotid))) %>%
# arrange(desc(occurrence))

# head(species_data)
#
# ggplot(species_data %>%
#              group_by(installation, plot_id) %>%
#              summarise(richness = length(unique(veg_id))),
#        aes(plot_id, richness)) +
#       geom_point() +
#       facet_grid(.~installation) +
#       theme_bw()
#
# ggplot(species_data %>%
#              filter(pct_cover > 1) %>%
#              group_by(installation, plot_id),
#        aes(plot_id, pct_cover, color = veg_id)) +
#       geom_point(position = "jitter") +
#       facet_grid(.~installation) +
#       theme_bw()
