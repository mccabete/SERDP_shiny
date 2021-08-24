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
# HELPER FUNCTIONS # Should I source all the functions from separate files? 
##################

source("www/functions/plot_comarison_ggplot.R")

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

var_to_names <- function(glm_name){
  name <- glm_map$state_vars_name[glm_map$glm_names == glm_name]
  return(name)
}


variable_transform_plot <- function(p, reactive_vars){  # takes ggpredict object, returns a ggredict object with rawdata backtransformed 
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

var_trans_normal_units <- function(vals, variable_name){
  
  var <- variable_name
  if(!(variable_name %in% glm_map$glm_names)){
    var <- names_to_variables(variable_name)
  }
  
 
  if(var == "logit_litter"){
    vals <- exp(vals)/(1 + vals)
  }
  if(var == "d_since_fire_log"){
    vals <- exp(vals)
  }
  if(var == "biomass_log"){
    vals <- exp(vals)
  }
return(vals)  
  
}

var_trans_lm <- function(vals, variable_name){
  
  var <- variable_name
  if(!(variable_name %in% glm_map$glm_names)){
    var <- names_to_variables(variable_name)
  }
  
  
  if(var == "logit_litter"){
    vals <- log( vals/(1 - vals) )
  }
  if(var == "d_since_fire_log"){
    vals <- log(vals)
  }
  if(var == "biomass_log"){
    vals <- log(vals)
  }
  return(vals)  
  
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
  ### ggpredict tab ----
  observeEvent(input$num_cov, {
    updateTabsetPanel(inputId = "state_vars", selected = input$num_cov)
    #input$custom_vals_boolean <- "no_custom_vars"
  
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
  
 output$slider_ggpredict <- renderUI({
   map(cov_names(), ~sliderInput_ggpredict(.x))
 }) 
  
# 
#  observeEvent(cov_xlab(), {
#    updateSliderInput(inputId = "x_cov_slider",
#                      min = 0,
#                      max = ceiling(
#                        max(
#                          var_trans_normal_units(path_data[[names_to_variables(cov_xlab())]], cov_xlab())) * 5),
#                      value = c(ceiling(
#                        min(
#                          var_trans_normal_units(path_data[[names_to_variables(cov_xlab())]], cov_xlab()))),
#                        
#                        var_trans_normal_units(mean(path_data[[names_to_variables(cov_xlab())]]), cov_xlab()))
# 
#    )
#  })
 #slider_x_title <- renderText(paste("Custom", input$state_variable1, "values"))
# slider_y_title <- renderText(paste("Custom", input$state_variable2, "values"))

# observeEvent(input$state_variable2,{
#   updateSliderInput(inputId = "y_cov_slider",
#                     min = max(
#                       ceiling(
#                         min(
#                           var_trans_normal_units(path_data[[names_to_variables(input$state_variable2)]], input$state_variable2))), 0), ## Negative values are weird here.
#                     max = ceiling(
#                       max(
#                         var_trans_normal_units(path_data[[names_to_variables(input$state_variable2)]], input$state_variable2)) * 5),
#                     value = c(ceiling(
#                                   min(
#                                     var_trans_normal_units(path_data[[names_to_variables(input$state_variable2)]], input$state_variable2))),
# 
#                               var_trans_normal_units(mean(path_data[[names_to_variables(input$state_variable2)]]), input$state_variable2))
# 
#   )
# })

# cov_terms <- reactive({
#   switch(input$custom)
# 
# })
# cov_terms <- reactive({
#   if(input$custom_vals_boolean == "no_custom_vars"){
#     return(cov_names())
#   }else if(input$num_cov == "single_covariate"){
#     var_x <- var_trans_lm(input$x_cov_slider, input$state_variable)
#     term1 <- paste0( names_to_variables(input$state_variable), " [", paste(var_x, collapse = ","), "]")
#     return(term1)
#   }else{
#     var_x <- var_trans_lm(input$x_cov_slider, input$state_variable1)
#     var_y <- var_trans_lm(input$y_cov_slider, input$state_variable2)
#     term1 <- paste0( names_to_variables(input$state_variable1), " [", paste(var_x, collapse = ","), "]")
#     term2 <- paste0( names_to_variables(input$state_variable2), " [", paste(var_y, collapse = ","), "]")
#     return(c(term1, term2))
#   }
# 
# })

cov_terms <- reactive({
  if(input$custom_vals_boolean == "no_custom_vars"){
    return(cov_names())
  }else if(input$num_cov == "single_covariate"){
    tmp <- paste0("ggpredict_vals_", cov_names())
    var_x <- var_trans_lm(input[[tmp]], input$state_variable)
    term1 <- paste0( names_to_variables(input$state_variable), " [", paste(var_x, collapse = ","), "]")
    return(term1)
  }else{
    tmp <- paste0("ggpredict_vals_", cov_names()[1])
    tmp2 <- paste0("ggpredict_vals_", cov_names()[2])
    var_x <- var_trans_lm(input[[tmp]], input$state_variable1)
    var_y <- var_trans_lm(input[[tmp2]], input$state_variable2)
    term1 <- paste0( names_to_variables(input$state_variable1), " [", paste(var_x, collapse = ","), "]")
    term2 <- paste0( names_to_variables(input$state_variable2), " [", paste(var_y, collapse = ","), "]")
    return(c(term1, term2))
  }
  
})

cov_names_reactive <- reactive({
  if(input$custom_vals_boolean == "no_custom_vars"){
    return(cov_names())
  }else if(input$num_cov == "single_covariate"){
    cov_names_sub <- c(names_to_variables(input$state_variable))
    return(cov_names_sub)
  }else{
    cov_names_sub <- c(names_to_variables(input$state_variable1), names_to_variables(input$state_variable2))
    return(cov_names_sub)
  }

})

  output$tick_abundance_estimated_plot <- renderPlot({
    p <-  ggpredict(tick_glmer, type = "re",  cov_terms()) #terms = c(cov_terms()) #cov_names()
    p <- variable_transform_plot(p, cov_names_reactive()) # cov_names_reactive() #cov_names()
    
    plt <- plot(p, rawdata = TRUE) + 
      ggtitle("") +
      xlab(paste(cov_xlab())) + 
      ylab("Predicted Ticks Per Trap") +
      labs(color = paste(cov_group_lable())) #+ 
    plt
    
    
  })
  

  
  ### Violin tab----
  
  ## Read in Baseline models
  percent_canopy_baseline <-  read_csv("www/percent_canopy_baseline.csv")
  biomass_baseline <-  read_csv("www/biomass_baseline.csv")
  logit_litter_baseline <-  read_csv("www/logit_litter_baseline.csv")
  litter_depth_baseline <-  read_csv("www/litter_depth_baseline.csv")
  vpd_baseline <-  read_csv("www/vpd_baseline.csv")
  ticks_baseline <-  read_csv("www/ticks_baseline.csv")
  
  ## Checkboxes
  
  
  # fire_vals <- reactive({
  #   if("d_since_fire_log" %in% input$sub_lm){
  #     return("slider output")
  #   }else{
  #     return(values_at(path_data$d_since_fire_log, values = "quart2"))
  #   }
  # })
  

  
  
  ## violin plots
  # output$violin_ticks <- renderPlot(plot_comparison_ggplot(ticks, ticks_baseline, title = "Pridicted Ticks"))

})
