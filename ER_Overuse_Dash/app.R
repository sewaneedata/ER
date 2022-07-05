################################################################################
# INTERACIVE DASHBOARD ------
# Description: 
# This code is the working interactive dashboard that will be our final product.

################################################################################
# Libraries ------

library(shiny)
library(bslib)
library(markdown)
library(shinydashboard)
library(readr)
library(dplyr)
library(ggplot2)
library(DT)
library(leaflet) 

################################################################################
# UI ------
################################################################################
  
  # HEADER ----
  #####################
  # This is the header on the very top of the dashboard
  header <- dashboardHeader(
    title = "Investigating ER Overuse", 
    titleWidth = 300
    )
  
  #NAVIGATION BAR ----
  ####################
  # This is the side navigation bar that links each tab to its contents
  sidebar <- dashboardSidebar(
    sidebarMenu(
      #Navigation Image
      HTML(paste0(
        "<br>",
        "<a href='SCHN_Logo.png' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='SCHN_Logo.png' width = '186'></a>",
        "<br>"
        )),
    # Where tabs and subtabs are created, named, and given icons
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("Overview", tabName = "overview", icon = icon("file")),
    menuItem("Findings", tabName = "findings", icon = icon("book-medical")),
    menuItem("Explore ER Overuse", tabName = "ER_Overuse", icon = icon("hospital"),
             menuSubItem("Map", tabName = "map"),
             menuSubItem("Demographics", tabName = "demo"),
             menuSubItem("Medical Services", tabName = "med_service"),
             menuSubItem("Conditions", tabName = "med_condition"))
    ))
  
  # BODY ----
  ##########################
  body <- dashboardBody(
    tabItems(
      #Adds content to each tab
        
      #HOME TAB
        tabItem(tabName = "home",
                includeMarkdown("www/home.Rmd")),
      #OVERVIEW TAB
        tabItem(tabName = "overview", 
                includeMarkdown("www/overview.Rmd")),
      #ER OVERUSE TAB
        
      #DEMOGRAPHICS TAB
        tabItem(tabName = "demo",
                includeMarkdown("www/demographics.Rmd"),
                #Demo Widgets 
                selectInput("race", label = h3("Select Race"), 
                            choices = c("Test1", "Test2"), 
                            selected = 1), hr()),
      #MAP TAB
        tabItem(tabName = "map",
                includeMarkdown("www/er_overuse.Rmd"),
                leafletOutput("zipMap")), #MAYBE ADD SPINNER WHILE LOADING
      
      #MEDICAL SERVICE TAB
      
      
      #CONDITIONS TAB
      
      
      #FINDINGS TAB
        tabItem(tabName = "findings",
                includeMarkdown("www/findings.Rmd"),
                plotOutput("er_overuse"))
        ))
     
  
  # Compile UI ----
  ########################
  ui <- dashboardPage(header, sidebar, body, 
                    skin = "black")

  
################################################################################
#SERVER ------
################################################################################

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #Create Reactive Values
  rv <- reactiveValues()
  
  #OBSERVE ------
  ######################################
  observe({
    #ER_Overuse Graph DF
    rv$acs_nonemerg_other <- scp %>% 
      group_by(acs_primary, nonemerg_primary) %>% 
      tally %>% 
      ungroup() %>% 
      mutate(total = sum(n)) %>% 
      summarise(percentage = n/total*100, across(everything())) %>%
      mutate( type = case_when( !acs_primary & !nonemerg_primary ~ "Other",
                                acs_primary ~ "ACS", 
                                nonemerg_primary ~ "Non emergent" )) %>% 
      mutate(Condition = ifelse(type == 'Other', "Other", "ACS/Non emergent"))
    
  })
  
  #OUTPUTS ------
##############################################################################
  
  #OVERVIEW GRAPHS ------
  ##########################################
  #Main Overview Graph
  output$er_overuse <- renderPlot({
    ggplot(data = rv$acs_nonemerg_other, aes(x = Condition, 
                                          y = percentage/100, 
                                          fill = type)) +
      geom_col()+
      labs(title = "Comparison of Primary Diagnosis Conditions",
           y = "Percentage of Visits",
           x = '') +
      scale_fill_manual(values=c("#c6dbef",
                                 "#74a9cf",
                                 "#08306b"),
                        name = "Type of Condition") +
      scale_y_continuous(labels = scales::percent) + 
      labs(title = "Comparison of Primary Diagnosis Conditions",
           y = "Percentage of Visits to the ER")
  })
  ##############################################################################
  # EXPLORE ER TAB GRAPHS ------
  
  # MAPS TAB
  #################################
  output$zipMap <- renderLeaflet({
    leaflet(data = scp) %>% addProviderTiles(providers$CartoDB.Voyager)
  })

  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
