
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
library(purrr)

##################
# DATA WRANGLING #
##################

#setwd("/projectnb/dietzelab/mccabete/SERDP_shiny/code")

# Helper data ----
installation_lookup_table <- read.csv("www/SERDP_data_installtion_lookup_table.csv")


# Tick data ----
tick_map <- st_read("www/Tick_prevelence_absense_map.shp") #By default, this function abriviates column names? Renaming them does cause errors. 
tick_map$pathogen_number <- tick_map$Hmn_pt_ * tick_map$Ticks
tick_map_pretty_names <- dplyr::rename(tick_map,"Installtion" = FULLNAM, 
                                       "Total Ticks" = ttl_tck, 
                                       "Human Pathogens Per Tick" = Hmn_pt_, 
                                       "Trapping Effort" = trp_ffr, 
                                       "Ticks Per Trap" = tcks_p_, 
                                       "Tick Borne Disease Risk" = PxA, 
                                       "Pathogens Detected" = pathogen_number
                                       )
tick_map_pretty_names <- select(tick_map_pretty_names, c("Installtion", 
                                                         "Total Ticks",
                                                         "Human Pathogens Per Tick", 
                                                         "Trapping Effort", 
                                                         "Ticks Per Trap", 
                                                         "Tick Borne Disease Risk", 
                                                         "Pathogens Detected"))

ticks <- read.csv("www/ticks.csv", stringsAsFactors = FALSE)
ticks_for_distribution <- select(ticks, c(
  "installation" , 
  "count"
))
ticks_for_distribution <- ticks_for_distribution %>% 
  group_by(installation ) %>% 
  summarise( count = sum(count))

ticks_for_distribution$installation <- data_name_to_formal(ticks_for_distribution$installation)


pathogens <- read.csv("www/pathogenicity_by_installation.csv", stringsAsFactors = FALSE)
#pathogens <- select(pathogens, c("Installation", "tbo", "Human", "Wildlife", "Domestic_Animals", "Endosymbiont", "Human_Endo","Human_Animal", "Unknown", "Vertebrate_Host","Disease"))
dung <- read.csv("www/dung.csv", stringsAsFactors = FALSE)
dung <- select(dung, c("installation", "species", "visit_year")) %>% na.omit()
dung <- dung[dung$installation %in% installation_lookup_table$data_name, ]
dung$installation <- data_name_to_formal(dung$installation)
dung <- dung %>% group_by(installation, species) %>% summarise(count = n())

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
source("www/functions/helper_functions.R")

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
      write.csv(tick_map_pretty_names, file)
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
      write.csv(ticks_for_distribution, file)
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
    count = colDef(name = "Count")
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
  

  
 #  ### Violin tab----
 #  
 #  ## Read in Baseline models
 #  # percent_canopy_baseline <-  read_csv("www/percent_canopy_baseline.csv")
 #  # biomass_baseline <-  read_csv("www/biomass_baseline.csv")
 #  # logit_litter_baseline <-  read_csv("www/logit_litter_baseline.csv")
 #  # litter_depth_baseline <-  read_csv("www/litter_depth_baseline.csv")
 #  # vpd_baseline <-  read_csv("www/vpd_baseline.csv")
 #  # ticks_baseline <-  read_csv("www/ticks_baseline.csv")
 #  
 #  ## Read in lm and glm objects to predict from 
 #  
 #  ## Check which scenario
 #  observeEvent(input$sub_lm, {
 #    updateTabsetPanel(inputId = "dependant_scenario", selected = input$sub_lm)
 #    
 #    ## Reset values to baseline
 #    #fire_vals <- values_at(path_data$d_since_fire_log, values = "quart2")
 #    #percent_canopy_vals <- percent_canopy_baseline$predicted
 #    #biomass_vals <- biomass_baseline$predicted
 #    #logit_litter_vals <- logit_litter_baseline$predicted
 #    #litter_depth_vals <- litter_depth_baseline
 #    #vpd_vals <- vpd_baseline$predicted
 #    #ticks_vals <- ticks_baseline$predicted
 #  }) 
 #  
 # 
 #  #slide_vals() <-re
 #  
 #  slider_names <- reactive(paste0("col", input$vars_to_slide))
 #  
 #  output$slider <- renderUI({
 #    map(input$vars_to_slide, ~ sliderInput_var(.x))
 #  })
 #  
 #  assign_vals_shiny <- function(term_name){ ### does this need to be reactive? 
 #    vals <- input[[term_name]]
 #  }
 #  
 #  ggpredict_var <- function(term_name, map_of_model_inputs, vals){
 #    map_of_model_inputs <- map_of_model_inputs[input_name == term_name,]
 #    num_dependant_models <- length( map_of_model_inputs$models_effected)
 #    
 #    val_storage <- matrix(NA, ncol = num_dependant_models + 1, rnow = 2000) ## Very large number, because values start small but grows combinatorially at each level. 
 #    val_storage[1,1] <- vals ## First user-input value. Model's effected must start with the model called
 #    
 #    for(i in 1:(num_dependant_models)){
 #     sm_map <- map_of_model_inputs[input_name == map_of_model_inputs$models_effected[i],]
 #     tmp_values <- na.omit(val_storage[,i])
 #     #term_paste <- paste0(sm_map$terms, " [tmp_values]", ) ## WHAT ABOUT MULTIPLE TERMS? ONLY LITTER, NEED PASTE STATMENT TO DO BOTH
 #      predictions <- ggpredict(sm_map$model, type = "re", terms = term_paste )
 #      
 #      val_storage[,i + 1] <- sample_from_conditionals(predictions, n = 5)
 #      
 #    }
 #    val_storage <- as.data.frame(val_storage)
 #    names(val_storage) <- c("user_input", map_of_model_inputs$models_effected) 
 #    
 #  }
 # 
 #  ### NEED TO MAKE SURE THEY CAN ONLY CHECKBOX THE MODELS THAT ARE ALLOWED TO BE MODIFIED TOGETHER
 #  
 #  calculate_conditional_sub <- eventReactive(input$simulate, {
 #   
 #   samples <- map(slider_names(), ggpredict_var(.x, map_of_model_inputs, assign_vals_shiny(.x))) ## Unlist samples in a way that makes sense
 #   #index <- which(names(samples) == "fire_levels") ## Need to find for every term 
 #   terms_paste <- paste0(names(samples), " [samples", index, "]") ## Can't just use the names because of ggpredict. 
 #   tick_pred <- ggpredict("tpt_effects_noHosts / map_of_model_inputs$inputname ==  'ticks'", type = "re", terms = terms_past)
 #   
 #   tick_samples <- sample_from_conditionals(tick_pred, n = 5)
 #   
 #   samp_names <- names(samp_names)
 #   final_df <- cbind(samples, tick_samples)
 #   names(final_df) <- c(samp_names, "ticks_predicted")
 #   
 #   return(final_df)
 #   
 # })
 #  
 #  
 #  ## Plot output
 #  
 #  #output$dynamic_plots <-  ## How to generate a dynamic number of plots? can I pass the UI statement? 
 #  
 # # model_list <- reactive(input$dependant_scenario, {
 # #   if(input$dependant_scenario == "independant_effects"){
 # #     return(vars_independant)
 # #   }else if (nput$dependant_scenario == "intermediate_effects"){
 # #     return(vars_intermediate)
 # #   }else{
 # #     return(vars_dependant)
 # #   }
 # # })
 # # 
 # #equation_list <- 
 # # observeEvent(input$avg_canopy_cover, percent_canopy_vals <- input$avg_canopy_cover)
 # # observeEvent(input$biomass_log, biomass_vals <- input$biomass_log)
 # # observeEvent(input$logit_litter, logit_litter_vals <- input$logit_litter)
 # # observeEvent(input$avg_litter_depth_all, litter_depth_vals <- input$avg_litter_depth_all )
 # # observeEvent(input$avg_1yr_vp..Pa., vpd_vals <- input$avg_1yr_vp..Pa.)
 # 
 # 
 #  ## Update variables
 # # fire_data <-  eventReactive(input$simulate, { ## When I do the actual model runs
 # #  #output$test_plot <- renderPlot(hist(fire_levels))
 # #   tmp <- fire_levels() - 10
 # #    return(tmp)
 # #  })
 # #  
 # #  output$test_plot <- renderText(paste(fire_data()))
 #  # fire_vals <- reactive({
 #  #   if("d_since_fire_log" %in% input$sub_lm){
 #  #     return("slider output")
 #  #   }else{
 #  #     return(values_at(path_data$d_since_fire_log, values = "quart2"))
 #  #   }
 #  # })
 #  
 # 
 #  
 #  
 #  ## violin plots
 #  # output$violin_ticks <- renderPlot(plot_comparison_ggplot(ticks, ticks_baseline, title = "Pridicted Ticks"))

}) #Shiny server function
