# Making STEM MAPS for tree surveys
#
# Adapted from code provided by Margaret Metz

library(plyr); library(dplyr); library(readr); library(ggplot2)
library(plotrix)
library(stringr)

tree_data <- read_csv("data/processed_data/trees.csv")# load tree data
plotids <- sort(unique(tree_data$plot_id))# vector of unique plot ids

plot_data <- read_csv("data/processed_data/plot_locations.csv")
plot_data <- plot_data %>%
      # mutate(plot_id = str_to_title(plot_id)) %>%
      select(plot_id, Longitude = lon, Latitude = lat, installation_full_name)

stem_map_data <- tree_data %>%
      arrange(desc(date)) %>%
      filter(!duplicated(stem_id)) %>%
      select(stem_id, plot_id, tag, dbh, health, distance, azimuth, scientific_name, Genus) %>%
      mutate(
            Genus = if_else(Genus %in% c("Acer","Carya", "Pinus", "Quercus"), Genus, "other"),
             symbol.pch = case_when(
                   Genus == "Acer" ~ 24,
                   Genus == "Carya" ~ 23,
                   Genus == "Pinus" ~ 21,
                   Genus == "Quercus" ~ 22,
                   Genus == "other" ~ 25,
                   ),
             symbol.colors = case_when(
                   Genus == "Acer" ~ "darkorange",
                   Genus == "Carya" ~ "brown",
                   Genus == "Pinus" ~ "red",
                   Genus == "Quercus" ~ "blue",
                   Genus == "other" ~ "black",
                   ),
            symbol.fill = case_when(
                  Genus == "Acer" ~ "darkorange",
                  Genus == "Carya" ~ "brown",
                  Genus == "Pinus" ~ "red",
                  Genus == "Quercus" ~ "blue",
                  Genus == "other" ~ "black",
            ),
            health = ifelse(health<1, "dead","alive")
             )


## Function to iterate over the vector of plot ids and make a map ####
for(i in plotids){

      tmp = filter(stem_map_data, plot_id==i)

      expansion=.4*cut(tmp$dbh, c(0,10,20,40,80, max(tree_data$dbh, na.rm=T)), labels=F)

      dead = tmp$health=="dead" & !is.na(tmp$health)
      dead[dead==TRUE] = "white"
      dead[dead==FALSE] = tmp$symbol.colors[dead==FALSE]

      ## Set page/map size
      # quartz(width = 8.5, height = 11, pointsize = 10)

      png(filename = paste("stem_maps/",i,".png",sep = ""),
          width = 8.5, height = 11,
          units = "in", pointsize = 10, res = 150)
      par(mai=c(0.5, 0.5, 0.5, 0.5))

      ## Make stem plot
      polar.plot(tmp$distance, tmp$azimuth,
           start=90, clockwise=T, rp.type="n",
           labels=paste(c("N","NE","E","SE","S","SW","W","NW"), seq(0,315,45), sep="\n"),
           label.pos=c(0, 45, 90, 135, 180, 225, 270, 315),
           radial.lim=c(0,12.6), mar=c(5, 5, 15, 5),
           label.prop=1.05, font=2, family="serif"
           )

xpos <- cos(pi*((-1)*tmp$azimuth+90)/180) * tmp$distance
ypos <- sin(pi*((-1)*tmp$azimuth+90)/180) * tmp$distance

points(xpos,ypos, pch=tmp$symbol.pch, cex=expansion, col=tmp$symbol.colors, bg=dead)


text(xpos,ypos, tmp$tag, cex=.5, pos=1, offset=c(-.5,-.6))

legend("bottomleft",
       legend=unique(tmp$Genus),
       col=unique(tmp$symbol.colors),
       pch=unique(tmp$symbol.pch),
       pt.bg=unique(tmp$symbol.fill),
       cex=0.8,
       inset=c(-0.035,.1))

legend("bottomright",
       legend=c("alive", "dead", NA,"0-10cm", "10-20cm", "20-40cm", "40-80cm", ">80cm"),
       col="black",
       pch=c(21,21,NA,rep(21,5)),
       pt.bg=c("black", "white", NA, "black", "black", "black", "black", "black"),
       pt.cex=c(1,1,NA, c(.4*(1:5))), cex=.85, inset=c(-0.035,.1))

pdata = filter(plot_data, plot_id==unique(tmp$plot_id)) %>%
      mutate(plot_id = str_to_title(plot_id))

mtext(paste("Plot ID: ", pdata$plot_id), font = 2, line=8, cex=2)

mtext(paste("Longitude ",pdata$Longitude,"\nLatitude ", pdata$Latitude, sep=""), line=5.75)

mtext(paste("Map printed ", date(), sep=""), side=1, adj=0, cex=.5, line=5)

dev.off()

}

dev.off()
