################################################################################
# INTERACIVE DASHBOARD ------
# Description:


################################################################################
# Libraries ------

library(shiny)
library(bslib)

################################################################################
# UI ------
################################################################################
ui <- fluidPage(
  
  # Header
  header <- dashboardHeader(
    title = "Investigating ER Overuse", 
    titleWidth = 300
    ), 
  
  #Side Navigation Bar
  sidebar <- dashboardSidebar(
    sidebarMenu(
      HTML(paste0(
        "<br>",
        #TO DO: Photo
        #"<a href='https://mobile.twitter.com/sewaneedatalab' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='US-NationalParkService-Logo.svg' width = '186'></a>",
        "<br>",
        "<br>",
        "<br>",
        "<br>",
        #"<p style = 'text-align: center;'><small><a href='https://mobile.twitter.com/sewaneedatalab' target='_blank'>NPS logo disclaimer</a></small></p>",
        "<br>"
      )),
    # Tabs
    menuItem("Overview", tabName = "Overview", icon = icon("home")),
    menuItem("ER Overuse", tabName = "ER_Overuse", icon = icon("bar-chart-o")),
    menuItem("Map", tabName = "Map", icon = icon("map")),
    menuItem("Explore", tabName = "explore", icon = icon("table")),
    menuItem("Conclusion", tabName = "con", icon = icon("tasks"))
    )),
  
  # Body
  body <- dashboardBody(
    
    # tabItems(
    #   tabItem(tabName = "Overview",
    #           includeMarkdown("www/overview.Rmd")
    #   ),
    #Testing to see if using HTML would be good or if we should just use RMARK
    # )
  ),
  
  # Compile UI
  dashboardPage(header, sidebar, body, 
                skin = "red"),

        # Show a plot of the generated distribution
        mainPanel(
           tabPanel("Test")
        )
    )




################################################################################
#SERVER ------
################################################################################
# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
    })
   
}

# Run the application 
shinyApp(ui = ui, server = server)
