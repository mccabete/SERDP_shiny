##################################
# Biodiversity in National Parks #
# by Alessio Benedetti           #
# ui.R file                      #
##################################

library(leaflet)
library(shinydashboard)
library(collapsibleTree)
library(shinycssloaders)
library(DT)
library(tigris)


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
        menuItem("Tick Borne Disease", tabName = "tick_borne_disease", icon = icon("disease"))
      )
      
    ), # end dashboardSidebar
    
    dashboardBody(
      
      tabItems(
        
        tabItem(tabName = "home",
          
          # home section
          includeMarkdown("www/home.md")
          
        ),
        
        tabItem(tabName = "tick_borne_disease",
         
            
              
              leafletOutput("parksMap", width = "75%", height = "100px") %>% withSpinner(color = "green"),
              
               
                
           
            
             
             selectInput(
                "installation", "Please Select Installtion(s) for data summary", installation.name,
                multiple = FALSE
              ),
            
              
              plotOutput("hist_summary_ticks"), 
             
            # dataTableOutput("tick_species")
            
            
                
            
            )## Tab 
          
                
              
      ) # End of tabItems
    
    ) # end dashboardBody
  
  )# end dashboardPage

))
