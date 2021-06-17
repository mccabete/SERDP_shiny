#library(rgdal, lib.loc = "/share/pkg.7/r/3.6.0/install/lib64/R/library")
library(shiny)
library(tidyverse)
library(leaflet.extras)
library(rvest)
library(sf)



##################
# DATA WRANGLING #
##################

tick_map <- st_read("www/Tick_prevelence_absense_map.shp") #By default, this function abriviates column names? Renaming them does cause errors. 
tick_map$pathogen_number <- tick_map$Hmn_pt_ * tick_map$Ticks

installation_lookup_table <- read.csv("www/SERDP_data_installtion_lookup_table.csv")
ticks <- read.csv("www/ticks.csv", stringsAsFactors = FALSE)

##################
# HELPER FUNCTIONS #
##################

subset_data <- function (data, installation_name){
  name <- installation_lookup_table$data_name[installation_name == installation_lookup_table$Formal_name]
  
  data <- filter(data, installation == name)
  
  return(data)
}

################
# SERVER LOGIC #
################

shinyServer(function(input, output) {
   
 
  
  #Tick Abundance Layer
  pal_tick_abundance <- colorBin("Blues", domain = tick_map$tcks_p_, bins = 4)
  
  labels_tick_abundance <- sprintf(
    "<strong>%s</strong><br/>%g Ticks per trap <br/> %g ticks, %g traps ",
    tick_map$FULLNAM, tick_map$tcks_p_, tick_map$Ticks, tick_map$Trp_ffr
  ) %>% lapply(htmltools::HTML)
  
  #Pathogen Prevelence Data Layer
  pal_path_prevelance <- colorBin("Purples", domain = tick_map$Hmn_pt_, bins = 4)
  
  labels_path_prevelance <- sprintf(
    "<strong>%s</strong><br/>%g Pathogens per Tick <br/> %g pathogens, %g ticks ",
    tick_map$FULLNAM, tick_map$Hmn_pt_, tick_map$pathogen_number, tick_map$Ticks
  ) %>% lapply(htmltools::HTML)
  
  # Risk Layer
  pal_risk <- colorBin("YlOrRd", domain = tick_map$PxA, bins = 4)
  
  labels_risk <- sprintf(
    "<strong>%s</strong><br/>%g Risk of Tick-borne Pathogen <br/> Exposure per 24 hours",
    tick_map$FULLNAM, tick_map$PxA
  ) %>% lapply(htmltools::HTML)
  

  #### Map
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
      ) %>%
      hideGroup(c("Tick Abundance", "Pathogen Presence"))
  }) # parkMap
      
  #### Tick Borne DIsease Summery Plots
  #tick_data <- subset_data(ticks, input$installation)
  
  output$hist_summary_ticks <- renderPlot( {
    tick_data <- subset_data(ticks, input$installation)
      ggplot(tick_data) +
        theme(
          legend.title = element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA),  
          plot.background = element_rect(fill = "transparent", colour = NA) 
          
        ) +
        geom_histogram(aes(x = count)) + 
        ylab("Frequency") + 
        xlab("Ticks Observed per Sampling Event") + 
        ggtitle("test") + 
      ggtitle(input$installation)
    })
  
  
  output$tick_species <- renderPlot({
    tick_data <- subset_data(ticks, input$installation)
    tick_data <- select(tick_data, c("visit_year", "count","life_stage" ,"species_name"))
    tick_data <- filter(tick_data, count >  0)
    
    ggplot(tick_data) +
      geom_col(aes(x = species_name, y = count,  fill = life_stage)) + 
      theme(
        legend.title = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),  
        plot.background = element_rect(fill = "transparent", colour = NA),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) +
      ylab("Total Ticks Collected") + 
      xlab("")
    
    })

  
  
  #####################################
  

})
