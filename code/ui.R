##################################
# Biodiversity in National Parks #
# by Alessio Benedetti           #
# ui.R file                      #
##################################
#library(rgdal)
#library(rgdal, lib.loc = "/share/pkg.7/r/3.6.0/install/lib64/R/library")
library(leaflet)
library(shinydashboard)
library(collapsibleTree)
library(shinycssloaders)
library(DT)
library(tigris)
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
              menuSubItem("Tick Pathogens", tabName = "pathogens")
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
                  column(8, p("Try hovering over installations for mean values used to generate this map.")),
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
                )  
        ), # disease risk map
        
        tabItem(tabName = "pathogens", 
              
              #  selectInput(
              #    "installation_path", "Please Select Installtion(s) for pathogen data", installation.name,
              #    multiple = TRUE
              #  ),
                
                reactableOutput("pathogen_data"),
                
                downloadButton("download_pathogen")
                ) #pathogens tab
        
      ) # End of tabItems
    
    ) # end dashboardBody
  
  )# end dashboardPage

))
