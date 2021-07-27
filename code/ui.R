
#library(rgdal)
#library(rgdal, lib.loc = "/share/pkg.7/r/3.6.0/install/lib64/R/library")
library(leaflet)
library(shinydashboard)
#library(collapsibleTree)
library(shinycssloaders)
library(DT)
library(tigris, "/share/pkg.7/r/4.0.5/install/lib64/R/library")
library(reactable)


###########
# DATA    #
###########
installation.name <- c("Avon Park Air Force Range", "Fort Benning", "Camp Blanding Army Base",
                        "Eglin Air Force Base", "Fort Gordon Army Base", "Fort Jackson Army Base", "Moody Air Force Base", 
                        "Camp Shelby Joint Forces Training Center", "Tyndall Air Force Base")
#state_vars_name <- c("Days Since Fire", "% Litter Cover", "Litter Depth",  "% Canopy Cover","Standing Biomass g/(m^2)", "1 Year Vapor Pressure Deficit")
state_vars_name <- c("Standing Biomass g/(m^2)", "1 Year Vapor Pressure Deficit")
# glm_names <- as.data.frame(lme4::getME(tick_glmer, "X"))
# glm_names <- names(glm_names)
# glm_names <- glm_names[glm_names !="(Intercept)"]
# stat_vars_df <- cbind(state_vars_name, glm_names)
# stat_vars_df <- as.data.frame(stat_vars_df)
# write_csv(stat_vars_df, file = "www/glm_names_map.csv")

parameter_tabs <- tabsetPanel(
  id = "state_vars",
  type = "hidden",
  tabPanel("single_covariate",
           selectInput(
             "state_variable1", "Select predictor of tick populations", state_vars_name,
             multiple = FALSE
           )
           
  ),
  tabPanel("two_covariates", 
           selectInput(
             "state_variable1", "Select predictor of tick populations", state_vars_name,
             multiple = FALSE
           ),
           selectInput(
             "state_variable2", "See how other predictors interact", state_vars_name, # state_vars_name without the first state variable? 
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
        menuItem("Exploring Hypotheticals", tabName = "sem", icon = icon("project-diagram")
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
                         downloadButton("download_tick_summary", label = "Download Tick Species and Sampleing Data"))
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
        
        tabItem(tabName = "sem",
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
                    selectInput("num_cov", "Number of predictors", 
                                choices = c("single_covariate", "two_covariates")
                    ),
                    parameter_tabs,
                  ),
                  mainPanel(
                    plotOutput("tick_abundance_estimated_plot")
                  )
                )


                ) # sem tab
        
        
      ) # End of tabItems for all tabs
    
    ) # end dashboardBody
  
  )# end dashboardPage
  
  ) # Fluid page
  ) # ui Function


