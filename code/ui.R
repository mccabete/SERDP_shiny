##################################
# Biodiversity in National Parks #
# by Alessio Benedetti           #
# ui.R file                      #
##################################
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
                 )#,
        # menuItem("Exploring Hypotheticals", tabName = "sem", icon = icon("project-diagram")
        #          )
        
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
        )#, # Tick hosts tab
        
        # tabItem(tabName = "sem", 
        #         selectInput(
        #           "installation_sem", "Please Select Installtion", installation.name,
        #           multiple = FALSE
        #         ),
        #         numericInput("cv_fire_days", "CV Fire Days", NA ), ## Need to set NA's to exisitng measurments 
        #         numericInput("FRI", "15 yr Fire Return Interval", NA),
        #         numericInput("time_since_fire", "Time Since Fire", NA),
        #         numericInput("canopy_cover", "Canopy Cover", NA),
        #         numericInput("litter_cover", "Litter Cover", NA),
        #         numericInput("litter_depth", "Litter Depth", NA),
        #         numericInput("standing_biomass", "Standing Biomass", NA),
        #         numericInput("Oneyr_vpd", "1 Year Vapor Pressure Deficit", NA), #, 
        #         #numericInput("Tick_abundance_estimated", "Tick Abundance", NA)
        #         
        #         plotOutput("tick_abundance_estimated_plot")
        #       
        #         
        #         )
        
      ) # End of tabItems for all tabs
    
    ) # end dashboardBody
  
  )# end dashboardPage

))
