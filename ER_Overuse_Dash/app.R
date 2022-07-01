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

  
  # Header
  header <- dashboardHeader(
    title = "Investigating ER Overuse", 
    titleWidth = 300
    )
  
  #Side Navigation Bar
  sidebar <- dashboardSidebar(
    sidebarMenu(
      HTML(paste0(
        "<br>"
      )),
      HTML(paste0(
        "<a href='datalab_logo.jpg' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='datalab_logo.jpg' width = '186'></a>",
        "<br>"
        )),
      
    # Tabs
    menuItem("Overview", tabName = "Overview", icon = icon("home")),
    menuItem("ER Overuse", tabName = "ER_Overuse", icon = icon("bar-chart-o")),
    menuItem("Map", tabName = "Map", icon = icon("map")),
    menuItem("Explore", tabName = "explore", icon = icon("table")),
    menuItem("Conclusion", tabName = "con", icon = icon("tasks"))
    ))
  
  # Body
  body <- dashboardBody(
    tabItems(
      #Adding Rmarkdown welcome page
        tabItem(tabName = "Overview",
                includeMarkdown("www/overview.Rmd")),
        tabItem(tabName = "ER_Overuse",
                includeMarkdown("www/er_overuse.Rmd"))
        )
  )
    #Testing to see if using HTML would be good or if we should just use RMARK
     
  
  # Compile UI
ui <- dashboardPage(header, sidebar, body, 
                    skin = "red")
      
    




################################################################################
#SERVER ------
################################################################################
# Define server logic required to draw a histogram
server <- function(input, output) {

   
}

# Run the application 
shinyApp(ui = ui, server = server)
