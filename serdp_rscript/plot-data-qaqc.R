#' Script for doing QA/QC on the general plot and plot visit data

#+ load packages ####
library(plyr); library(dplyr); library(ggplot2); library(readr)
library(stringi)

#' Plot location data are sourced from the 'update_plot_locations.R' script.
#'

#+ examine plot location data ####

# plot_shp <- rgdal::readOGR("data/plot-locations.shp")# read in spatial data
# # plot_shp@data$xcoord_lon <- plot_shp@coords[,1]# add coords to data frame slot
# # plot_shp@data$ycoord_lat <- plot_shp@coords[,2]
#
# sort(unique(plot_shp$name))
#
# plot_shp@data <- plot_shp@data %>%
#       mutate(name = as.character(name),
#              instal= as.character(instal),
#              pid = tolower(substr(name, start = nchar(name)-1, stop = nchar(name))),
#              pid = case_when(
#                    pid=="z1" ~ "a1",
#                    pid=="y1" ~ "b1",
#                    pid=="x1" ~ "c1",
#                    pid=="w1" ~ "d1",
#                    pid=="v1" ~ "e1",
#                    pid=="t1" ~ "f1",
#                    pid=="s1" ~ "g1",
#                    pid=="r1" ~ "h1",
#                    TRUE ~ pid
#              ),
#              plot_id = paste(instal, pid, sep = " "),
#              pid2 = toupper(paste(substr(pid, 1,1), "-", substr(pid, 2,2), sep = ""))
#       )
#
# unique(plot_shp$instal)
#
# #+ Create labels column for installation full names
# plot_shp$inst_full_name[plot_shp$instal=="movietheatercog"] <- "Camp Blanding"
# plot_shp$inst_full_name[plot_shp$instal=="blanding"] <- "Camp Blanding"
# plot_shp$inst_full_name[plot_shp$instal=="eglin"] <- "Eglin AFB"
# plot_shp$inst_full_name[plot_shp$instal=="tyndall"] <- "Tyndall AFB"
# plot_shp$inst_full_name[plot_shp$instal=="cogoncoast"] <- "Tyndall AFB"
# plot_shp$inst_full_name[plot_shp$instal=="avonpark"] <- "Avon Park AFR"
# plot_shp$inst_full_name[plot_shp$instal=="cogon10x"] <- "Avon Park AFR"
# plot_shp$inst_full_name[plot_shp$instal=="cogon p"] <- "Avon Park AFR"
# plot_shp$inst_full_name[plot_shp$instal=="shelby"] <- "Camp Shelby"
# plot_shp$inst_full_name[plot_shp$instal=="moody"] <- "Moody AFB"
# plot_shp$inst_full_name[plot_shp$instal=="jackson"] <- "Fort Jackson"
# plot_shp$inst_full_name[plot_shp$instal=="benning"] <- "Fort Benning"
# plot_shp$inst_full_name[plot_shp$instal=="gordon"] <- "Fort Gordon"
#
# unique(plot_shp$inst_full_name)


plot_shp <- sf::st_read("data/plot-locations.shp")
# plot_shp <- plot_shp %>%
#       select(instal, lon, lat, plot_id, pid, pid2,
#              inst_name = inst_f_)
# #+ Write out new shapefile
# sf::st_write(plot_shp, "data/plot-locations.shp", delete_layer = T)

#+ Make regular data frame ####
# plot_locs <- plot_shp@data
# summary(plot_locs)
# str(plot_locs)
# sort(as.character(plot_locs$name))# sort plot names alphabetically

#' We have a few "plots" that are locations of cogongrass invasions that we marked to send to land managers. I'll remove those in the next section. Currently there are 98 plot locations and we need to remove three "extra" cogon plots. There was a "movie theater cogon" plot at Camp Blanding that we sampled but does not have any recorded burn history. It's in a kind of funny place.
#'

#+ revise plot location data ####
# plot_locs <- plot_locs %>%
#       rename(installation = instal,
#              installation_full_name = inst_full_name)
#
# str(plot_locs)
# summary(plot_locs)
# duplicated(plot_locs$plot_id)# check for duplicate plot ids
# unique(plot_locs$plot_id)
#
# plot_locs$plot_id[plot_locs$plot_id=="movietheatercog on"] <- "blanding theater_cogon"
#
# write_csv(plot_locs, "data/processed_data/plot_locations.csv")# save checked data



#' We have located 94 unique plots and some have been visited multiple times. Next I assess the plot visit records, which are entered as the plot id, the date of visit, whether or not the plot is invaded, and the year of the last fire. Year since last fire is a coarse measure - we need to do further data processing and analysis to obtain the precise date of the last fire and relate that to the season of burn and perhaps months since last fire.

#+ plot visit data ####

plot_visit_data <- read_csv("data/raw_data/plot_visit.csv")
summary(plot_visit_data)
sort(plot_visit_data$plot_id)# do we need to exclude the movie theater cogon?

plot_visit_data <- plot_visit_data %>%
      # rename(pid = plot_id) %>%
      mutate(
      years_since_fire = as.numeric(format(visit_date, "%Y")) - last_fire_year,
      plot_id = paste(installation, plot_id),
      visit_year = lubridate::year(visit_date)
)

#' We have three NA values in "years since fire"
#'
#+ investigate NAs in "years since fire"
filter(plot_visit_data, is.na(last_fire_year))
# theater cogon at Blanding that has no history, and two plots at Shelby that need data requested
## I sent emails to Lisa Yager & Heide Stinson to request fire history data for Shelby. I also emailed the fire person at Tyndall requesting actual fire history data - right now the year is the best we have and in some cases it is a guess.

# plot_visit_data <- filter(plot_visit_data, !is.na(last_fire_year))

# names(plot_locs)
names(plot_visit_data)
sort(unique(plot_shp$plot_id))
sort(unique(plot_visit_data$plot_id))
unique(plot_visit_data$plot_id) %in% unique(plot_shp$plot_id)

plot_visit_data <- plot_visit_data %>%
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
            )
      )

# join plot locations to plot visit data
plot_visit_data <- left_join(
      plot_visit_data,
      select(plot_shp, -pid, -name))
summary(plot_visit_data)
# filter(plot_visit_data, is.na(xcoord_lon))$plot_id

# plot_visit_data$notes

sort(plot_visit_data$plot_id)
sort(duplicated(plot_visit_data$plot_id))
#' We revisited 9 of the 29 plots that were initially sampled in 2017.


#+
summary(plot_visit_data)
filter(plot_visit_data, is.na(years_since_fire))$plot_id
plot_visit_data <- plot_visit_data %>%
      mutate(last_fire_year = ifelse(plot_id=="shelby j1",2012,last_fire_year),
             years_since_fire = visit_year - last_fire_year) %>%
      select(installation:visit_year, lon:inst_name)

# filter(plot_visit_data, is.na(inst_name))$plot_id
# plot_visit_data <- plot_visit_data %>%
#       filter(plot_id != "blanding theater_cogon")

# sort(unique(plot_visit_data$plot_id))

#+ write out plot visit data ####
write_csv(plot_visit_data, "data/processed_data/plot_visit_data.csv")



# Join plot visit dates to spatial data ####
pvisit_data <- read_csv("data/processed_data/plot_visit_data.csv")

plot_shp <- left_join(plot_shp,
                      select(pvisit_data, -installation, -notes, -lon, -lat))

st_write(plot_shp, "data/plot_visit.shp")

#+ initial plotting of plot visit data ####
# ggplot(plot_visit_data %>%
#              group_by(installation) %>%
#              summarise(plot_ct = n_distinct(plot_id)),
#        aes(installation, plot_ct, label=plot_ct)) +
#       geom_col() +
#       geom_text(nudge_y = .2) +
#       ylab("Number of plots") +
#       ggtitle("Number of plots established at each installation") +
#       theme_classic()
#
# ggplot(plot_visit_data, aes(as.factor(years_since_fire))) +
#       geom_bar(aes(fill = imcy_inv), position = "dodge") +
#       ylab("Number of plots") +
#       xlab("Years since last fire") +
#       ggtitle("Plots sampled across burn units") +
#       theme_bw()
# # Number of burn units sampled
# length(unique(plot_visit_data$years_since_fire))
#
# ggplot(plot_visit_data, aes(as.factor(years_since_fire))) +
#       geom_bar() +
#       facet_grid(installation ~ .) +
#       ylab("Number of plots") +
#       xlab("Years since last fire") +
#       ggtitle("Number of plots across burn units by installation") +
#       theme_bw()
#
# ggplot(plot_visit_data, aes(imcy_inv)) +
#       geom_bar() +
#       ylab("Number of plots") +
#       xlab("") +
#       # ggtitle("Invaded plots: 8, Uninvaded plots: 21") +
#       theme_bw()
