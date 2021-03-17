##### reading in 2019 data from flash drive ####

library(readxl)

tick_data <- read_excel("D:/serdp/2019 data/2019-only-tick-data.xlsx", sheet = 1)
write.csv(tick_data, "data/raw_data/2019_serdp_data/2019-only-tick-data.csv", row.names = F)

tree_data <- read_excel("D:/serdp/2019 data/tree-data-entry.xlsx", sheet = 1)
write.csv(tree_data, "data/raw_data/2019_serdp_data/tree-data-entry.csv", row.names = F)

species_1m <- read_excel("D:/serdp/2019 data/species1m-data-entry.xlsx", sheet = 1)
write.csv(species_1m, "data/raw_data/2019_serdp_data/species1m-data-entry.csv", row.names = F)

quadrat_25cm <- read_excel("D:/serdp/2019 data/quadrat25cm-data-entry.xlsx", sheet = 1)
write.csv(quadrat_25cm, "data/raw_data/2019_serdp_data/quadrat25cm-data-entry.csv", row.names = F)

camera_traps <- read_excel("D:/serdp/2019 data/camera-traps-info.xlsx", sheet = 1)
write.csv(camera_traps, "data/raw_data/2019_serdp_data/camera-traps-info.csv", row.names = F)

woody_subplot <- read_excel("D:/serdp/2019 data/subplot-woody-species-data-entry.xlsx", sheet = 1)
write.csv(woody_subplot, "data/raw_data/2019_serdp_data/subplot-woody-species-data-entry.csv", row.names = F)

quadrat_1m <- read_excel("D:/serdp/2019 data/quadrat1m-data-entry.xlsx", sheet = 1)
write.csv(quadrat_1m, "data/raw_data/2019_serdp_data/quadrat1m-data-entry.csv", row.names = F)

densiometer <- read_excel("D:/serdp/2019 data/densiometer-canopy-cover-data-entry.xlsx", sheet = 1)
write.csv(densiometer, "data/raw_data/2019_serdp_data/densiometer-canopy-cover-data-entry.csv", row.names = F)

plot_visit_2019 <- read_excel("D:/serdp/2019 data/2019-plot-visit-entry.xlsx", sheet = 1)
write.csv(plot_visit_2019, "data/raw_data/2019_serdp_data/2019-plot-visit-entry.csv", row.names = F)

dung_data <- read_excel("D:/serdp/2019 data/dung-data-entry.xlsx", sheet = 1)
write.csv(dung_data, "data/raw_data/2019_serdp_data/dung-data-entry.csv", row.names = F)
