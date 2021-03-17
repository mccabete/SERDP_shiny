# Gradient Site Data

library(readr); library(readxl); library(tidyr)
# library(plyr); library(dplyr);
library(ggplot2)

# read in data
gradient_biomass <- read_excel("data/gradient_sites/Biomass_July2015_FINAL.xlsx")
# Collected biomass of all plants (live and standing dead) plus litter within the confines of one 0.25 x 0.25 m quadrat per plot. Vines whose leaves projected areally over the quadrats were collected even if they were not rooted in the quadrat. For all other species, we made an effort to only collected aboveground biomass from plants rooted in the plot.
str(gradient_biomass)
gradient_biomass$avetillerlength <- as.numeric(gradient_biomass$avetillerlength)
summary(gradient_biomass)
# cogon: green, live
# cogon_standingdead: rooted in plot, but dead/senesced >60% of tiller)
gradient_biomass <- gradient_biomass %>% mutate(dead_live_ratio = cogon_standingdead/cogon)


uninvaded <- filter(gradient_biomass, trt == "UN")
uninvaded <- gather(uninvaded, type, biomass, cogon:unknown)


cogon_sites <- filter(gradient_biomass, trt == "REF")
summary(cogon_sites)
# rearrange data
cogon_sites <- gather(cogon_sites, type, biomass, cogon:unknown)
# ?gather
summary(cogon_sites)

gradient_cogon <- cogon_sites %>%
      filter(type=="cogon"|type=="cogon_standingdead", biomass>0) %>%
      group_by(site, plot) %>%
      mutate(tot_biomass = biomass*16) %>%
      select(site,plot,tot_biomass)

cogon_data_plot <- ggplot(cogon_sites)
cogon_data_plot +
      geom_boxplot(aes (x = type, y = biomass*16))
cogon_data_plot +
      geom_point(aes(x = biomass*16, y = type)) +
      xlab("biomass (g/m^2)") +
      ylab("type") +
      theme_bw()

# ratio of standing dead to live cogon grass
cogon_data_plot +
      geom_point(aes(x = dead_live_ratio, y = site)) +

      xlab("Standing Dead:Live Cogongrass") +
      ylab("site") +
      theme_bw()

ggplot(gradient_biomass, aes(x = cogon*16, y = cogon_standingdead*16)) +
      geom_point(shape = 1) +
      scale_x_continuous(limits = c(0,1000)) +
      scale_y_continuous(limits = c(0,1000)) +
      geom_abline(slope = 1, intercept = 0) +
      geom_abline(slope = .25, intercept = 0, linetype = "dashed") +
      geom_abline(slope = 4, intercept = 0, linetype = "dashed") +
      xlab("Live Cogongrass") +
      ylab("Standing Dead") +
      theme_bw()

# ggsave("images/cogondeadlive.png", width = 6, height = 6, dpi = 600)

# Uninvaded  Plots
uninvaded_plot <- ggplot(uninvaded)
uninvaded_plot +
      geom_boxplot(aes (x = type, y = biomass*16))
uninvaded_plot +
      geom_point(aes(x = biomass*16, y = type)) +
      xlab("biomass (g/m^2)") +
      ylab("type") +
      theme_bw()

# All untreated biomass
uninvaded %>%
      group_by(type) %>%
      summarise()

all_biomass <- rbind(
      gradient_biomass %>%
            filter(trt=="UN") %>%
            select(othergrass:totalnative,unknown) %>%
            mutate(biomass = rowSums(.), type = "native") %>%
            select(type, biomass) %>%
            mutate(biomass = biomass*16),
      gradient_biomass %>%
            filter(trt=="REF") %>%
            select(cogon,cogon_standingdead) %>%
            mutate(biomass = rowSums(.), type = "cogongrass") %>%
            select(type, biomass) %>%
            mutate(biomass = biomass*16)
)

all_biomass$type <- factor(all_biomass$type, levels = c("native","cogongrass"), labels = c("Native","Cogongrass"))


def_theme <- theme(legend.title = element_blank(),
                   legend.text = element_text(size = 18),
                   legend.position = "right",
                   axis.text = element_text(size = 18),
                   axis.title = element_text(size = 18),
                   plot.title = element_text(size = 20),
                   strip.background = element_blank(),
                   panel.grid = element_blank()
)
invasion_color <- scale_color_manual(values = c("blue","darkorange"))
invasion_fill <- scale_fill_manual(values = c("blue","darkorange"))

ggplot(all_biomass, aes(type,biomass, fill = type)) +
      geom_boxplot(outlier.shape = 21, outlier.color = "tan", show.legend = F) +
      theme_bw() +
      invasion_fill +
      def_theme +
      labs(fill = "Fuel Type",
           x = "",
           # expression(paste("mpg (  ", m^{-2}, ")")))
           y = expression(paste("Fuel load (g/", m^{2}, ")")))
ggsave("figures/gradient_site_cogon_other_comparison.eps", dpi = 300)
