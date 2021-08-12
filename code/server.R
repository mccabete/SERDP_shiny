#library(rgdal)
#library(rgdal, lib.loc = "/share/pkg.7/r/3.6.0/install/lib64/R/library")
library(shiny)
library(tidyverse)
library(leaflet.extras)
library(rvest)
library(sf)
library(reactable)

library(sjPlot)
library(patchwork)
library(MuMIn)
library(DHARMa)
library(lme4)
library(piecewiseSEM)
library(effectsize)
library(ggeffects)

##################
# DATA WRANGLING #
##################

#setwd("/projectnb/dietzelab/mccabete/SERDP_shiny/code")

# Helper data ----
installation_lookup_table <- read.csv("www/SERDP_data_installtion_lookup_table.csv")


# Tick data ----
tick_map <- st_read("www/Tick_prevelence_absense_map.shp") #By default, this function abriviates column names? Renaming them does cause errors. 
tick_map$pathogen_number <- tick_map$Hmn_pt_ * tick_map$Ticks

ticks <- read.csv("www/ticks.csv", stringsAsFactors = FALSE)
pathogens <- read.csv("www/pathogenicity_by_installation.csv", stringsAsFactors = FALSE)
#pathogens <- select(pathogens, c("Installation", "tbo", "Human", "Wildlife", "Domestic_Animals", "Endosymbiont", "Human_Endo","Human_Animal", "Unknown", "Vertebrate_Host","Disease"))
dung <- read.csv("www/dung.csv", stringsAsFactors = FALSE)
dung <- select(dung, c("installation", "species", "visit_year")) %>% na.omit()

# Vegetation data ----
plot_data <- read.csv("www/all_plotlevel_data.csv")

# SEM data ----
#path_data <- read.csv("www/path_analysis_data.csv")
#all_host_effects <- read.csv("www/all_hosts_sem_effects.csv") # Could change if we need it
load("www/tick_glmer.Rdata")
path_data <- read.csv("www/path_data.csv", stringsAsFactors = FALSE)
glm_map <- read_csv("www/glm_names_map.csv")
#state_vars_name <- c("Days Since Fire", "% Litter Cover", "Litter Depth",  "% Canopy Cover","Standing Biomass g/(m^2)", "1 Year Vapor Pressure Deficit")

##################
# HELPER FUNCTIONS #
##################

subset_data <- function (data, installation_name, grep = FALSE){
  
  name <- installation_lookup_table$data_name[installation_name == installation_lookup_table$Formal_name]
  
  if(grep){
    
    similar_name_rows <- grep(name, data$plot_id) 
    data <- data[similar_name_rows,]
    
  }else{
    
    data <- filter(data, installation == name)
    
  }
  
  
  return(data)
}

## Helper Function for hypotheticals 
names_to_variables <- function(state_vars_name){
  name <- glm_map$glm_names[glm_map$state_vars_name == state_vars_name]
  return(name)
}


variable_transform <- function(p, reactive_vars){  # takes ggpredict object, returns a ggredict object with rawdata backtransformed 
  num_vars <- length(reactive_vars)
  no_transforms <- c("avg_litter_depth_all", "avg_canopy_cover", "avg_1yr_vp..Pa.")
  
  for(i in 1:num_vars){
    variable <- reactive_vars[i]
    
    if(!(variable %in% no_transforms)){
      
      rawdata <-  attr(p, "rawdata")
      at_list <- attr(p, "at.list")
      
      if(variable == "biomass_log"){
        
        if(i == 1){
          rawdata$x <- exp(as.numeric(rawdata$x))
          p$x <- exp(as.numeric(p$x))
        }else{
          
          rawdata$group <- as.character(exp(as.numeric(rawdata$group))) 
          at_list$biomass_log <- as.character(exp(as.numeric(at_list$biomass_log)))
          p$group <- as.character(exp(as.numeric(as.character(p$group))))
          
        }
        
        
      }
      
      if(variable == "d_since_fire_log"){
        
        
        if(i == 1){
          rawdata$x <- exp(as.numeric(rawdata$x))
          p$x <- exp(as.numeric(p$x))
        }else{
          rawdata$group <- as.character(exp(as.numeric(rawdata$group))) 
          at_list$d_since_fire_log <- as.character(exp(as.numeric(at_list$d_since_fire_log)))
          p$group <- as.character(exp(as.numeric(as.character(p$group))))
          
          
        }
        
      }
      
      if(variable == "logit_litter"){
        
        if(i == 1){
          rawdata$x <- exp(as.numeric(rawdata$x))/(1 + exp(as.numeric(rawdata$x)))
          p$x <- exp(as.numeric(p$x))/(1 + exp(as.numeric(p$x)))
        }else{
          rawdata$group <- as.character(exp(as.numeric(rawdata$group)) / (1 + exp(as.numeric(rawdata$group)))) 
          at_list$logit_litter <- as.character(exp(as.numeric(at_list$logit_litter))/ (1 + exp(as.numeric(at_list$logit_litter))))
          p$group <- as.character(exp(as.numeric(as.character(p$group))) / (1 + exp(as.numeric(as.character(p$group)))))
          
        }
        
      }
      attr(p, "rawdata") <- rawdata
      attr(p, "at.list") <- at_list
      
    }
    
  }
  
  return(p)
  
}

################
# SERVER LOGIC #
################

shinyServer(function(input, output) {

#### Tick Borne Disease ----     
  # TBD Map prep ----
  #Tick Abundance Layer
  pal_tick_abundance <- colorBin("Blues", domain = tick_map$tcks_p_, bins = 4)
  
  labels_tick_abundance <- sprintf(
    "<strong>%s</strong><br/>%g Ticks per trap <br/> %g ticks, %g traps ",
    tick_map$FULLNAM, tick_map$tcks_p_, tick_map$Ticks, tick_map$trp_ffr
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
  

  # TBD Map ----
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
      addPolygons(fillColor = ~pal_tick_abundance(tick_map$tcks_p_),
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
  
  # Download Map Data
  output$download_tick_map <- downloadHandler(
    filename = function() {
      paste0("Tick_Borne_Disease_Map", ".csv")
    },
    content = function(file) {
      write.csv(tick_map, file)
    }
  )

 
  # Tick Borne Disease Summery Plots ----
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
  
  output$download_tick_summary <- downloadHandler(
    
    filename = function(){
      paste("Tick_Species_and_Sampling", ".csv")
    },
    content = function(file){
      write.csv(ticks, file)
    }
    
  )
   
  
  
  # Pathogen data ----
  
  
  output$pathogen_data <- renderReactable(reactable(pathogens, filterable = TRUE, searchable = TRUE, columns = list(
    Organism_or_Pathogen = colDef(name = "Pathogen / Organism"), 
    Vertebrate_Host = colDef(name = "Vertebrate Host"), 
    Disease = colDef(name = "Disease"), 
    Infection_population =colDef(name = "Infection Population") 
                                                  ) 
                                          ))

  output$download_pathogen <- downloadHandler(
    filename = function() {
      paste0("pathogens", ".csv")
    },
    content = function(file) {
      write.csv(pathogens, file)
    }
  )
  
  # Tick Host data ----
  output$host_data <- renderReactable(reactable(dung, filterable = TRUE, searchable = TRUE, columns = list(
    installation = colDef(name = "Installation"), 
    species = colDef(name = "Host Detected"), 
    visit_year = colDef(name = "Year")
  ) 
  ))
  
  output$download_host <- downloadHandler(
    filename = function() {
      paste0("hosts_detected", ".csv")
    },
    content = function(file) {
      write.csv(dung, file)
    }
  )
  
#### Vegetation ----
  # Litter (cover and biomass)
  # Standing Biomass
  # Species plots
  
#### Fire (Maybe fire effects?) ----

  
#### Exploring Hypotheticals ----
  
  
  
   # pretty_paste <- function(var_name, vals = NULL){
   #    if (!is.null(vals)){
   #      tmp <- paste0("c(", paste(vals, collapse = ","), ")")
   #      text <- paste(var_name, " ", "[", tmp ,"]", sep = "" )
   #    }else{
   # 
   #      text <- paste(var_name)
   # 
   #    }
   # 
   #   return(text)
   # }

   # pretty_paste <- function(var_name){
   #   
   #     #paste0("c(", paste(vals, collapse = ","), ")")
   #     text <- paste0(var_name, " ", "[", "vals","]")
   # 
   #   return(text)
   # }
  
  ## Reactive Elements
  
  #state_vars_levels <- reactiveValues(avg_1yr_vp..Pa = path_data$avg_1yr_vp..Pa., biomass_log = path_data$biomass_log )

   
  # eventReactive(input$avg_1yr_vp..Pa, {
  # state_vars_levels$avg_1yr_vp..Pa <- input$avg_1yr_vp..Pa
  # })
  # 
  # eventReactive(input$standing_biomass, {
  #   state_vars_levels$biomass_log <- input$standing_biomass
  # })
  
  observeEvent(input$num_cov, {
    updateTabsetPanel(inputId = "state_vars", selected = input$num_cov)
  
  }) 
  
  cov_names <- reactive({
    switch(input$num_cov,
           single_covariate = names_to_variables(input$state_variable),
           two_covariates = c(names_to_variables(input$state_variable1), names_to_variables(input$state_variable2))
    )
  })
  
  
  cov_xlab <- reactive({
    switch(input$num_cov,
           single_covariate = input$state_variable,
           two_covariates = input$state_variable1
    )
  })
  
  cov_group_lable <- reactive({
    switch(input$num_cov,
           single_covariate = NULL, 
           two_covariates = input$state_variable2
    )
  })
  
  
  ### Adding custom values to project into the future
  observeEvent(input$custom_vals_boolean, {
    updateTabsetPanel(inputId = "custom_vars", selected = input$custom_vals_boolean)
    
  })
  
 observeEvent(input$state_variable1, {
   updateSliderInput(inputId = "x_cov_slider", 
                     min = 0, 
                     max = ceiling(max(path_data[[names_to_variables(input$state_variable1)]]) * 10),
                     value = mean(path_data[[names_to_variables(input$state_variable1)]])
                     
   )
 })

observeEvent(input$state_variable2,{
  updateSliderInput(inputId = "y_cov_slider", 
                    min = ceiling(min(path_data[[names_to_variables(input$state_variable2)]])), 
                    max = ceiling(max(path_data[[names_to_variables(input$state_variable2)]]) * 10),
                    value = c(ceiling(min(path_data[[names_to_variables(input$state_variable2)]])), mean(path_data[[names_to_variables(input$state_variable2)]]))
                    
  )
})
  
cov_terms <- reactive({
  if(input$custom_vals_boolean == "no_custom_vars"){
    return(cov_names())
  }else{
    term1 <- paste0( names_to_variables(input$state_variable1), " [c(", paste(input$x_cov_slider, collapse = ","), ")]")
    term2 <- paste0( names_to_variables(input$state_variable1), " [c(", paste(input$y_cov_slider, collapse = ","), ")]")
    return(c(term1, term2))
  }
  
})
  
  output$tick_abundance_estimated_plot <- renderPlot({
    p <-  ggpredict(tick_glmer, type = "re",  terms = c(cov_terms()))
    p <- variable_transform(p, cov_names())
    
    plt <- plot(p, rawdata = TRUE) + 
      ggtitle("") +
      xlab(paste(cov_xlab())) + 
      ylab("Predicted Ticks Per Trap") +
      labs(color = paste(cov_group_lable())) #+ 
    plt
    
    
  })
  

  

})
