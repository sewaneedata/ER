################################################################################
# INTERACIVE DASHBOARD ------
# Description: 

################################################################################
# Libraries ------

library(shiny)
library(bslib)
library(markdown)
library(shinydashboard)

################################################################################
# UI ------
################################################################################
  
  # Header ----
  header <- dashboardHeader(
    title = "Investigating ER Overuse", 
    titleWidth = 300
    )
  
  #Side Navigation Bar ----
  sidebar <- dashboardSidebar(
    sidebarMenu(
      HTML(paste0(
        "<br>"
      )),
      #Add Image in Menu
      HTML(paste0(
        "<a href='datalab_logo.jpg' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='datalab_logo.jpg' width = '186'></a>",
        "<br>"
        )),
      
    # Add Tabs
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("Overview", tabName = "overview", icon = icon("home")),
    menuItem("Explore ER Overuse", tabName = "ER_Overuse", icon = icon("bar-chart-o"),
             menuSubItem("Map", tabName = "map"),
             menuSubItem("Demographics", tabName = "demo"),
             menuSubItem("Medical Services", tabName = "med_service"),
             menuSubItem("Conditions", tabName = "med_condition")),
    menuItem("Findings", tabName = "findings", icon = icon("tasks"))
    ))
  
  # Body ----
  body <- dashboardBody(
    tabItems(
      #Adding Markdowns to each tab (to edit text, go to file mentioned in includeMarkdown)
        tabItem(tabName = "home",
                includeMarkdown("www/home.Rmd")),
        tabItem(tabName = "overview",
                  includeMarkdown("www/overview.Rmd")),
        tabItem(tabName = "demo",
                includeMarkdown("www/demographics.Rmd")),
        tabItem(tabName = "map",
                includeMarkdown("www/er_overuse.Rmd"))
        ))
     
  
  # Compile UI ----
ui <- dashboardPage(header, sidebar, body, 
                    skin = "black")

################################################################################
#SERVER ------
################################################################################

# Define server logic required to draw a histogram
server <- function(input, output) {

   
}

# Run the application 
shinyApp(ui = ui, server = server)
