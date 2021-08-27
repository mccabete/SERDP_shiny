library(maptools)
library(multcomp)
library(sandwich)
library(zoo)
library(Matrix)
library(abind)
library(car)
library(numDeriv)
#library(rgdal, lib.loc = "/share/pkg.7/r/4.1.1/install/lib64/R/library")
library(leaflet)
library(shinydashboard)
library(shinycssloaders)
library(DT)
##library(tigris, "/share/pkg.7/r/4.0.5/install/lib64/R/library")
#library(tigris, lib.loc = "/share/pkg.7/r/4.1.1/install/lib64/R/library")
library(reactable)

#library(tigris, "/share/pkg.7/r/4.0.5/install/lib64/R/library")
##################################################################################

# library(leaflet)
# library(shinydashboard)
# library(shinycssloaders)
# library(DT)
# library(tigris)
# library(reactable)


###########
# DATA    #
###########
source("www/functions/plot_comarison_ggplot.R")
source("www/functions/helper_functions.R")

installation.name <- c("Avon Park Air Force Range", "Fort Benning", "Camp Blanding Army Base",
                        "Eglin Air Force Base", "Fort Gordon Army Base", "Fort Jackson Army Base", "Moody Air Force Base",
                        "Camp Shelby Joint Forces Training Center", "Tyndall Air Force Base")
state_vars_name <- c("Days Since Fire", "% Litter Cover", "Litter Depth",  "% Canopy Cover","Standing Biomass g/(m^2)", "1 Year Vapor Pressure Deficit")
#stat_vars_sig <-  c("Days Since Fire", "% Litter Cover", "Litter Depth",  "% Canopy Cover","Standing Biomass g/(m^2)", "1 Year Vapor Pressure Deficit") # Need to modify by what is significant at predicting ticks?
path_data <- read.csv("www/path_data.csv", stringsAsFactors = FALSE)


# glm_names <- as.data.frame(lme4::getME(tick_glmer, "X"))
# glm_names <- names(glm_names)
# glm_names <- glm_names[glm_names !="(Intercept)"]
# stat_vars_df <- cbind(state_vars_name, glm_names)
# stat_vars_df <- as.data.frame(stat_vars_df)
# write_csv(stat_vars_df, file = "www/glm_names_map.csv")

#######################
# Helper UI functions #
#######################

sliderInput_var <- function(id) {
  id_var <- names_to_variables(id)
  tmp_nums <- var_trans_normal_units(path_data[[id_var]], id)
  id_name <- paste0("col", id_var)
  min <- min(tmp_nums) / 5
  max <- max(tmp_nums) * 5
  
  mean <- mean(tmp_nums)
  
  sliderInput(id_name, label = id, 
              min = round(min, digits = 1),
              max = round(max, digits = 1), 
              value = c(round(mean, digits = 1)))
  
}

sliderInput_ggpredict <- function(id) {
  
  tmp_nums <- var_trans_normal_units(path_data[[id]], id)
  min <- min(tmp_nums) / 5
  max <- max(tmp_nums) * 5
  
  shiny_id <- paste0("ggpredict_vals_", id)
  sliderInput(shiny_id, label = var_to_names(id), 
              min = round(min, digits = 1),
              max = round(max, digits = 1), 
              value = c(round(mean(tmp_nums), digits = 1), round(min(tmp_nums), digits = 1)))
}

###############
# Hidden Tabs #
###############
num_covariates_list <- c("single_covariate", "two_covariates")
names(num_covariates_list) <- c("1", "2") # There must be a better way

covariate_boolean_choices <- c( "no_custom_vars", "yes_custom_vars")
names(covariate_boolean_choices) <- c("No", "Yes")

dependant_scenario_list <- c("independant_effects", "intermediate_effects", "dependant_effects")
vars_independant <- c("Days Since Fire", "1 Year Vapor Pressure Deficit")
vars_intermediate <- c("% Canopy Cover", "Litter Depth", "1 Year Vapor Pressure Deficit")
vars_dependant <- c("Standing Biomass g/(m^2)", "% Litter Cover", "Litter Depth", "1 Year Vapor Pressure Deficit")


sliders_tab <- tabsetPanel(
  id = "dependant_scenario", 
  type = "hidden",
  tabPanel("independant_effects",
           fluidRow(sliders <- purrr::map(vars_independant, sliderInput_var))
           
           ),
  tabPanel("intermediate_effects",
           fluidRow(sliders <- purrr::map(vars_intermediate, sliderInput_var),
                    p("Warning: % Canopy Cover, and % Litter Cover are both informed by Days Since Fire. If you manually change values of % Canopy cover, it represents a scenario where the % Litter Cover is infromed by Days Since Fire, but % Canopy isn't.")
                    )
  ), 
  tabPanel("dependant_effects",
           fluidRow(sliders <- purrr::map(vars_dependant, sliderInput_var))
           )
) ## sliders tab

custom_predictor_vals <- tabsetPanel(
  id = "custom_vars",
  type = "hidden",
  tabPanel("yes_custom_vars",
           uiOutput("slider_ggpredict")
  ),

  tabPanel("no_custom_vars", column(12))

)


parameter_tabs <- tabsetPanel(
  id = "state_vars",
  type = "hidden",
  tabPanel("single_covariate",
           selectInput(
             "state_variable", "Select predictor of tick populations", state_vars_name,
             multiple = FALSE
           )
  ),
  tabPanel("two_covariates", 
           selectInput(
             "state_variable1", "Select predictor of tick populations", state_vars_name,
             multiple = FALSE
           ),
           selectInput(
             "state_variable2", "Select interacting predictor", state_vars_name, # state_vars_name without the first state variable? 
             multiple = FALSE
           )
  )
  
)

###########
# LOAD UI #
###########

shinyUI(fluidPage(
  
  # load custom stylesheet
  includeCSS("www/style.css"),
  
  
  # remove shiny "red" warning messages on GUI
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  # load page layout
  dashboardPage(
    
    skin = "green",
      
    dashboardHeader(title="SERDP Project 2636", titleWidth = 300),
    
    dashboardSidebar(width = 300,
      sidebarMenu(
        HTML(paste0(
          "<br>",
          "<img style = 'display: block; margin-left: auto; margin-right: auto;' src='Pine_Savanah.png' width = '186'></a>",
          "<br>"
        )),
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("Tick Borne Disease", tabName = "tick_borne_disease", icon = icon("disease"),
              menuSubItem("Disease Risk Map", tabName = "disease_risk_map"),
              menuSubItem("Tick Pathogens", tabName = "pathogens"),
              menuSubItem("Tick Hosts", tabName = "tick_host")
                 ),
        menuItem("Vegetation", tabName = "vegetation", icon = icon("pagelines"), 
                 menuSubItem("Litter", tabName = "litter"), 
                 menuSubItem("Canopy", tabName = "canopy_cover")#, 
                 # menuSubItem("Biomass", tabName = "biomass_tab")
                  ),
        menuItem("Exploring Hypotheticals", tabName = "sem", icon = icon("project-diagram"), 
                 menuSubItem("Predictors of Tick Populations", tabName = "ggpredict_plots")#, 
                 #menuSubItem("How Predictors Interact", tabName = "violin")
                 )
        
      )
      
    ), # end dashboardSidebar
    
    dashboardBody(
      
      tabItems(
        
        tabItem(tabName = "home",
          
          # home section
          includeMarkdown("www/home.md")
          
        ),
        
        tabItem(tabName = "tick_borne_disease") ,## Tab 
        tabItem(tabName = "disease_risk_map", 
                fluidRow(
                  column(1),
                  column(10,
                         leafletOutput("parksMap", width = "100%", height = "100px") %>% withSpinner(color = "green")
                  ),
                  column(1)
                ),
                
                fluidRow(
                  column(2),
                  column(6, p("Try hovering over installations for mean values used to generate this map, or download the shapefiles and data.")),
                  column(2, 
                         downloadButton("download_tick_map", label = "Download Map Data")
                         ),
                  column(2)
                ),
                
                selectInput(
                  "installation", "Please Select Installtion for tick data summary", installation.name,
                  multiple = FALSE
                ),
                
                fluidRow(
                  column(6,
                         plotOutput("hist_summary_ticks")
                  ), 
                  column(6, 
                         plotOutput("tick_species")
                  )
                ),
                fluidRow(
                  column(6),
                  column(6, 
                         downloadButton("download_tick_summary", label = "Download Tick Species and Sampling Data"))
                )
        ), # disease risk map
        
        tabItem(tabName = "pathogens", 
              
                reactableOutput("pathogen_data"),
                
                downloadButton("download_pathogen")
                ), #pathogens tab
        tabItem(tabName = 'tick_host',
          reactableOutput("host_data"), 
          downloadButton("download_host")
        ), # Tick hosts tab
        
        tabItem(tabName = "vegetation"), 
        tabItem(tabName = "litter",
                selectInput(
                  "installation_litter", "Please Select Installtion for leaf litter data summary", installation.name,
                  multiple = FALSE
                ),
                fluidRow(
                  column(6, plotOutput("pecent_litter_cover_plot")
                        ),
                  column(6, plotOutput("litter_depth_plot")
                        )
                )
                ), # Litter tabitem
        tabItem(tabName = "canopy_cover", 
                selectInput(
                  "installation_canopy", "Please Select Installtion for canopy cover summary", installation.name,
                  multiple = FALSE
                ),
                plotOutput("pecent_canopy_cover_plot")
        ), # canopy cover tabitem
        # tabItem(tabname = "biomass_tab", 
        #         selectInput(
        #           "installation_biomass", "Please Select Installtion for biomass summary", installation.name,
        #           multiple = FALSE
        #         ),
        #         plotOutput("biomass_plot_hist")
        #         ), #biomass tabitem
        tabItem(tabName = "sem"),
        tabItem(tabName = "ggpredict_plots",
                # selectInput(
                #   "state_variable", "Select predictor of tick populations", state_vars_name,
                #   multiple = FALSE
                # ),
                # selectInput(
                #   "state_variable2", "See how other predictors interact", state_vars_name,
                #   multiple = FALSE
                # ),
                #numericInput("cv_fire_days", "CV Fire Days", NA ), ## Need to set NA's to exisitng measurments
                #numericInput("FRI", "15 yr Fire Return Interval", NA),
                # numericInput("time_since_fire", "Time Since Fire", NA),
                # numericInput("canopy_cover", "Canopy Cover", NA),
                # numericInput("litter_cover", "Litter Cover", NA),
                # numericInput("litter_depth", "Litter Depth", NA),
                # numericInput("standing_biomass", "Standing Biomass", NA),
                # numericInput("avg_1yr_vp..Pa", "1 Year Vapor Pressure Deficit", NA), #,
                #numericInput("Tick_abundance_estimated", "Tick Abundance", NA)

                #plotOutput("tick_abundance_estimated_plot"),
                #textOutput("vals_placement")
                
                sidebarLayout(
                  sidebarPanel(
                    
                    parameter_tabs,
                    selectInput("num_cov", "Number of predictors", 
                                choices = num_covariates_list
                    ),
                    selectInput("custom_vals_boolean", "Project to custom values?",
                                choices = covariate_boolean_choices),
                    custom_predictor_vals

                  ),
                  mainPanel(
                    plotOutput("tick_abundance_estimated_plot")
                  )
                )


                )#, # ggpredict_plots tab
        # tabItem(tabName = "violin", 
        #         sidebarPanel(
        #           radioButtons("sub_lm", "Choose one or more predictors to provide custom values:",
        #                              choiceNames = c("Stage 1", "Stage 2", "Stage 3"), ## PAss html here? something more intuitive?
        #                              choiceValues = dependant_scenario_list),
        #           checkboxGroupInput("vars_to_slide", "Check the variables to change thier value", choices = glm_map$state_vars_name), 
        #           uiOutput("slider"), 
        #           #sliders_tab, 
        #           actionButton("simulate", "Draw Graphs")
        #         ), 
        #         mainPanel(
        #           fluidRow(textOutput("test_plot"))
        #         )
        #   
        #        
        #         #plotOutput("violin_ticks")
        #         
        #         ) # violin tab
        
        
      ) # End of tabItems for all tabs
    
    ) # end dashboardBody
  
  )# end dashboardPage
  
  ) # Fluid page
  ) # ui Function


