## Copy raw data from Dropbox to project directory ----
library(readxl)

cogon_data <- read_excel("~/Dropbox (UF)/SERDP-Project/data/cogon-data-entry.xlsx", sheet = 1)
write.csv(cogon_data, "data/raw_data/cogon-data.csv", row.names = F)

densiometer <- read_excel("~/Dropbox (UF)/SERDP-Project/data/densiometer-canopy-cover-data-entry.xlsx", sheet = 1)
write.csv(densiometer, "data/raw_data/densiometer-data.csv", row.names = F)

dung <- read_excel("~/Dropbox (UF)/SERDP-Project/data/dung-data-entry.xlsx", sheet = 1)
write.csv(dung, "data/raw_data/dung.csv", row.names = F)

# plot_info <- read.csv("~/Dropbox (UF)/SERDP-Project/data/plot-info.csv")
# write.csv(plot_info, "data/raw_data/plot-info.csv", row.names = F)

quadrat1m <- read_excel("~/Dropbox (UF)/SERDP-Project/data/quadrat1m-data-entry.xlsx", sheet = 1)
write.csv(quadrat1m, "data/raw_data/quadrat1m.csv", row.names = F)

quadrat25cm <- read_excel("~/Dropbox (UF)/SERDP-Project/data/quadrat25cm-data-entry.xlsx", sheet = 1)
write.csv(quadrat25cm, "data/raw_data/quadrat25cm.csv", row.names = F)

species1m <- read_excel("~/Dropbox (UF)/SERDP-Project/data/species1m-data-entry.xlsx", sheet = 1)
write.csv(species1m, "data/raw_data/species1m.csv", row.names = F)

woody_subplot <- read_excel("~/Dropbox (UF)/SERDP-Project/data/subplot-woody-species-data-entry.xlsx", sheet = 1)
write.csv(woody_subplot, "data/raw_data/subplot-woody-species.csv", row.names = F)

ticks <- read_excel("~/Dropbox (UF)/SERDP-Project/data/ticks-data-entry.xlsx", sheet = 1)
write.csv(ticks, "data/raw_data/ticks.csv", row.names = F)

trees <- read_excel("~/Dropbox (UF)/SERDP-Project/data/tree-data-entry.xlsx", sheet = 1)
write.csv(trees, "data/raw_data/trees.csv", row.names = F)

plot_visits <- read_excel("~/Dropbox (UF)/SERDP-Project/data/plot-visit-data-entry.xlsx", sheet = 1)
write.csv(plot_visits, "data/raw_data/plot_visit.csv", row.names = F)

