##################################
# Biodiversity in National Parks #
# by Alessio Benedetti           #
# server.R file                  #
##################################

library(shiny)
library(tidyverse)
library(leaflet.extras)
library(rvest)
library(sf)

#####################
# SUPPORT FUNCTIONS #
#####################

# function to retrieve a park image from the park wiki page
park_image <- function (park_Name){
  
  #bug1_fix#
  park_WikiUrl <- gsub(" ","_",paste0("https://en.wikipedia.org/wiki/",park_Name))
  #bug1_fix#
  park_Img <- read_html(park_WikiUrl)
  park_Img <- park_Img %>% html_nodes("img")
  
  list_park_Img <- (grepl("This is a featured article", park_Img) | grepl("Question_book-new.svg.png", park_Img) | grepl("Listen to this article", park_Img) | grepl("This is a good article", park_Img))
  park_Img <- park_Img[min(which(list_park_Img == FALSE))]
  
  park_Img <- gsub("\"","'",park_Img)
  park_Img <- gsub("//upload.wikimedia.org","https://upload.wikimedia.org",park_Img)
  park_Img <- sub("<img","<img style = 'max-width:100%; max-height:200px; margin: 10px 0px 0px 0px; border-radius: 5%; border: 1px solid black;'",park_Img)
  
  return(park_Img)
  
}
  
# function that build the park card html pop up
park_card <- function (park_Name, park_Code, park_State, park_Acres, park_Latitude, park_Longitude) {
  
  card_content <- paste0("<style>div.leaflet-popup-content {width:auto !important;}</style>",
                    "<link rel='stylesheet' href='https://use.fontawesome.com/releases/v5.7.1/css/all.css' integrity='sha384-fnmOCqbTlWIlj8LyTjo7mOUStjsKC4pOpQbqyi7RrhN7udi9RwhKkMHpvLbHG9Sr' crossorigin='anonymous'>",
                    "<table style='width:100%;'>",
                    "<tr>",
                    "<th><b><h2 style='text-align: left;'>",park_Name,"</h2></b></th>",
                    "<th><img style = 'border:1px solid black;' src='https://www.crwflags.com/art/states/",park_State,".gif' alt='flag' title='Flag of ",state.name[match(park_State,state.abb)]," ' width=80></th>",
                    "</tr>",
                    "</table>",
                    "<div class='flip-card'>",
                      "<div class='flip-card-inner'>",
                        "<div class='flip-card-front'>",
                          "<table style='width:100%;'>",
                            "<tr>",
                              "<td colspan='2'>",park_image(park_Name),"</td>",
                            "</tr>",
                            "<tr>",
                              "<td style='padding: 5px;'><h4><b>Code: </b>",park_Code,"</h4></td>",
                              "<td style='padding: 5px;'><h4><b>Acres: </b>",format(park_Acres, big.mark = ' '),"</h4></td>",
                            "</tr>",
                            "<tr>",
                              "<td style='padding: 5px;'><h4><b>Latitude: </b>",park_Latitude,"</h4></td>",
                              "<td style='padding: 5px;'><h4><b>Longitude: </b>",park_Longitude,"</h4></td>",
                            "</tr>",
                          "</table>",
                        "</div>",
                        "<div class='flip-card-back'>",
                          "<h3>Media links</h3> ",
                          "<hr>",
                          "<table style='width:80%;'>",
                            "<tr>",
                              "<td style='text-align: left; padding-left: 25px;'><h4>Official page:</h4></td>",
                              "<td><a style='color:white;' href='https://www.nps.gov/",park_Code,"/index.htm' target='_blank'><i class='fas fa-globe fa-2x'></i></a></td>",
                            "</tr>",
                            "<tr>",
                              "<td style='text-align: left; padding-left: 25px;'><h4>Wikipedia page:<h4></td>",
                              "<td><a style='color:white' href='https://en.wikipedia.org/wiki/",park_Name,"' target='_blank'><i class='fab fa-wikipedia-w fa-2x'></i></td></p>",
                            "</tr>",        
                            "<tr>",
                              "<td style='text-align: left; padding-left: 25px;'><h4>Pictures:<h4></td>",
                              "<td><a style='color:white' href='https://www.google.com/search?tbm=isch&q=",park_Name,"&tbs=isz:m' target='_blank'><i class='fas fa-images fa-2x'></i></a></td>",
                            "</tr>",
                            "<tr>",
                              "<td style='text-align: left; padding-left: 25px;'><h4>Youtube videos:<h4></td>",
                              "<td><a style='color:white' href='https://www.youtube.com/results?search_query=",park_Name,"' target='_blank'><i class='fab fa-youtube fa-2x'></i></td>",
                            "</tr>",
                          "</table>",
                        "</div>",
                      "</div>",
                    "</div>"
  )
  
  return(card_content)
  
}

##################
# DATA WRANGLING #
##################

tick_map <- st_read("/projectnb/dietzelab/mccabete/SERDP_shiny/code/serdp_data/Tick_prevelence_absense_map.shp") #By default, this function abriviates column names? Renaming them does cause errors. 
tick_map$pathogen_number <- tick_map$Hmn_pt_ * tick_map$Ticks

# preprocessed parks file:
#   3 records were multi states parks, only was was attributed
#     DEVA,Death Valley National Park,CA/NV,4740912,36.24,-116.82  --> CA
#     GRSM,Great Smoky Mountains National Park,TN/NC,521490,35.68,-83.53 --> TN
#     YELL,Yellowstone National Park,WY/MT/ID,2219791,44.6,-110.5 --> WY
#   added (U.S.) suffix to Glacier National Park record for wiki disambigaution

parks <- read.csv("www/parks.csv")
species <- read.csv("www/species.csv")

# tidy & enrich dataframes
levels(species$Park.Name)[levels(species$Park.Name)=='Glacier National Park'] <- 'Glacier National Park (U.S.)'
parks$Acres <- as.numeric(parks$Acres)
parks$Latitude <- as.numeric(parks$Latitude)
parks$Longitude <- as.numeric(parks$Longitude)

parks <- parks %>%
  mutate(
    ParkRegion = state.region[match(parks$State,state.abb)]
  )

parks$ParkGroup <- ""
parks$ParkGroup[1:28] <- "First Group"
parks$ParkGroup[29:56] <- "Second Group"

species <- species %>%
  mutate(
    ParkRegion = parks$ParkRegion[match(substr(species$Species.ID,1,4),parks[,c("ParkCode")])]
  )

species <- species %>%
  mutate(
    ParkGroup = parks$ParkGroup[match(substr(species$Species.ID,1,4),parks[,c("ParkCode")])]
  )

species <- species %>%
  mutate(
    ParkState = parks$State[match(species$Park.Name,parks$ParkName)]
  )

# support structures
parksNames <- sort(as.character(unique(species[,c("Park.Name")])))
speciesCategories <- sort(as.character(unique(species[,c("Category")])))
speciesCategoriesByState <- species %>% group_by(Category, ParkState) %>% tally(sort=TRUE)
states <- states(cb=F) ## Error if set to TRUE> 
speciesStates <- sort(as.character(unique(speciesCategoriesByState$ParkState[complete.cases(speciesCategoriesByState)]))) 

################
# SERVER LOGIC #
################

shinyServer(function(input, output) {
   
 # parks map
 # output$parksMap <- renderLeaflet({
 #  leaflet(data=parks) %>% addProviderTiles(providers$Stamen.Watercolor, group = "Stamen Watercolor", options = providerTileOptions(noWrap = TRUE)) %>%#, minZoom = 4)) %>%
 #  addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Open Street Map", options = providerTileOptions(noWrap = TRUE)) %>%
 #  addProviderTiles(providers$NASAGIBS.ViirsEarthAtNight2012, group = "Nasa Earth at Night", options = providerTileOptions(noWrap = TRUE)) %>%
 #  addProviderTiles(providers$Stamen.TerrainBackground, group = "Stamen Terrain Background", options = providerTileOptions(noWrap = TRUE)) %>%
 #  addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery", options = providerTileOptions(noWrap = TRUE)) %>%
 #  addFullscreenControl() %>%
 #  addMarkers(
 #    ~Longitude,
 #    ~Latitude,
 #    icon = makeIcon(
 #      iconUrl = "32px-US-NationalParkService-Logo.svg.png",
 #      shadowUrl = "32px-US-NationalParkService-Logo.svg - black.png",
 #      shadowAnchorX = -1, shadowAnchorY = -2
 #    ),
 #    clusterOptions = markerClusterOptions()
 #  ) %>%
 #  addLayersControl(
 #    baseGroups = c("Stamen Watercolor","Open Street Map","Nasa Earth at Night","Stamen Terrain Background","Esri World Imagery"),
 #    position = c("topleft"),
 #    options = layersControlOptions(collapsed = TRUE)
 #  )
 # })

  ######### Tess edits  ###########
  
  # output$parksMap <- renderLeaflet({
  #    leaflet(data=parks) %>% 
  #    addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Open Street Map", options = providerTileOptions(noWrap = TRUE)) %>%
  #    addProviderTiles(providers$Stamen.TerrainBackground, group = "Stamen Terrain Background", options = providerTileOptions(noWrap = TRUE)) %>%
  #    addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery", options = providerTileOptions(noWrap = TRUE)) %>%
  #    addFullscreenControl() %>%
  #    addMarkers(
  #      ~Longitude,
  #      ~Latitude,
  #      icon = makeIcon(
  #        iconUrl = "32px-US-NationalParkService-Logo.svg.png",
  #        shadowUrl = "32px-US-NationalParkService-Logo.svg - black.png",
  #        shadowAnchorX = -1, shadowAnchorY = -2
  #      ),
  #      clusterOptions = markerClusterOptions()
  #    ) %>%
  #    addLayersControl(
  #      baseGroups = c("Open Street Map","Stamen Terrain Background","Esri World Imagery"),
  #      position = c("topleft"),
  #      options = layersControlOptions(collapsed = TRUE)
  #    )
  #   })
  # 
  
  #Tick Abundance Layer
  pal_tick_abundance <- colorBin("YlOrRd", domain = tick_map$tcks_p_, bins = 3)
  
  labels_tick_abundance <- sprintf(
    "<strong>%s</strong><br/>%g Ticks per trap <br/> %g ticks, %g traps ",
    tick_map$FULLNAM, tick_map$tcks_p_, tick_map$Ticks, tick_map$Trp_ffr
  ) %>% lapply(htmltools::HTML)
  
  #Pathogen Prevelence Data Layer
  pal_path_prevelance <- colorBin("YlOrRd", domain = tick_map$Hmn_pt_, bins = 3)
  
  labels_path_prevelance <- sprintf(
    "<strong>%s</strong><br/>%g Pathogens per Tick <br/> %g pathogens, %g ticks ",
    tick_map$FULLNAM, tick_map$Hmn_pt_, tick_map$pathogen_number, tick_map$Ticks
  ) %>% lapply(htmltools::HTML)
  
  # Risk Layer
  pal_risk <- colorBin("YlOrRd", domain = tick_map$PxA, bins = 3)
  
  labels_risk <- sprintf(
    "<strong>%s</strong><br/>%g Risk of Tick-borne Pathogen <br/> Exposure per 24 hours",
    tick_map$FULLNAM, tick_map$PxA
  ) %>% lapply(htmltools::HTML)
  
  
  # Map
  output$parksMap <- renderLeaflet({
    leaflet(data = tick_map) %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery", options = providerTileOptions(noWrap = TRUE)) %>%
      addFullscreenControl() %>%
      #------------ Risk Layer ----------#
      addPolygons(fillColor = ~pal_risk(PxA),
                  weight = 1,
                  opacity = 1,
                  color = "white",
                  fillOpacity = 1,
                  label = labels_risk, 
                  group = "Tick Borne Disease Risk" 
      ) %>%
      addLegend(pal = pal_risk, 
                values = ~PxA, 
                opacity = 0.7, 
                title = "Tick Borne Disease Risk",
                position = "bottomright", 
                group = "Tick Borne Disease Risk"
      ) %>%
      #------------ Pathogen Presence Layer ----------#
      addPolygons(fillColor = ~pal_path_prevelance(Hmn_pt_),
                  weight = 1,
                  opacity = 1,
                  color = "white",
                  fillOpacity = 1,
                  label = labels_path_prevelance, 
                  group = "Pathogen Presence" 
      ) %>%
      addLegend(pal = pal_path_prevelance, 
                values = ~Hmn_pt_, 
                opacity = 0.7, 
                title = "Human Pathogens/Tick",
                position = "bottomright", 
                group = "Pathogen Presence"
      ) %>%
      #------------ Tick Abundance Layer ----------#
      addPolygons(fillColor = ~pal_tick_abundance(tcks_p_),
                  weight = 1,
                  opacity = 1,
                  color = "white",
                  fillOpacity = 1,
                  label = labels_tick_abundance, 
                  group = "Tick Abundance" 
                  ) %>%
      addLegend(pal = pal_tick_abundance, 
                values = ~tcks_p_, 
                opacity = 0.7, 
                title = "Tick Abundance",
                position = "bottomright", 
                group = "Tick Abundance"
                ) %>%
      #------------ Layer control ----------#
      addLayersControl(
        #baseGroups = c("Esri World Imagery"),
        overlayGroups = c("Tick Abundance", "Pathogen Presence",  "Tick Borne Disease Risk"),
        position = c("topleft"),
        options = layersControlOptions(collapsed = TRUE)
      )
  })
  
  
  #####################################
  

})
