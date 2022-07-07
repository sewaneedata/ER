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
             menuSubItem("Primary Care Services", tabName = "care_service"),
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
      
      # EXPLORE ER OVERUSE DROPDOWN
      # MAP TAB
        tabItem(tabName = "map",
              includeMarkdown("www/er_overuse.Rmd"),
              leafletOutput("zipMap")), #MAYBE ADD SPINNER WHILE LOADING
       
      #DEMOGRAPHICS TAB
        tabItem(tabName = "demo",
                includeMarkdown("www/demographics.Rmd"),
                #Demo Filter Widgets
                fluidRow(
                  column(3,
                         radioButtons( inputId = "sex",
                                       label = h3("Select Sex"),
                                       choices = unique(scp$Patient_Sex),
                                       selected = 1)),
                  column(3,
                         radioButtons("race", label = h3("Select Race"),
                                     choices = unique(scp$Race_Chr), #Remove unknown
                                     selected = 1)),
                  column(3,
                         selectInput( inputId = "age",
                                       label = h3("Select Age Group"),
                                       choices = unique(scp$age_group),
                                       selected = 1))),
                         selectInput( inputId = "county",
                                      label = h3("Select County"),
                                      choices = unique(scp$county),
                                      selected = 1,
                                      multiple = TRUE),
                         selectInput( inputId = "zip",
                                      label = h3("Select ZipCodes"),
                                      choices = unique(scp$Patient_Zip),
                                      selected = 1,
                                      multiple = TRUE),
                         selectInput( inputId = "insurance",
                                      label = h3("Select Insurance Type"),
                                      choices = unique(scp$Primary_Payer_Class_Cd),
                                      selected = 1,
                                      multiple = TRUE),
               plotOutput("county_plot")),
      
      #PRIMARY CARE SERVICE TAB
        tabItem(tabName = "care_service",
                
                fluidRow(
                  column(6,
                         selectInput(inputId = "county1",
                                     label= h3("Select County"),
                                     choices = unique(scp$county),
                                     selected = "Grundy")),
                  column(6,
                         selectInput(inputId = "zip1",
                                     label = h3("Select Zip Code"),
                                     choices = unique(scp$Patient_Zip),
                                     selected = "A"))),
                br(),
                br(),
                fluidRow(column(6,  plotOutput("all_cond_county")),
                         column(6,  plotOutput("all_cond_zip")))),
      
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
    # rv$county_demo <- scp %>% 
    #   filter( Patient_Sex %in% input$sex,
    #           Race_Chr %in% input$race, 
    #           age_group %in% input$age,
    #           county %in% input$county) %>% 
    #   group_by(acs_primary, nonemerg_primary) %>% 
    #   tally %>% 
    #   ungroup() %>% 
    #   group_by(county) %>% 
    #   mutate(total = sum(n)) %>% 
    #   summarise(percentage = n/total*100, across(everything())) %>%
    #   mutate( type = case_when( !acs_primary & !nonemerg_primary ~ "Other",
    #                             acs_primary ~ "ACS", 
    #                             nonemerg_primary ~ "Non emergent" )) %>% 
    #   mutate(Condition = ifelse(type == 'Other', "Other", "ACS/Non emergent"))
  observe({
    rv$county <- scp %>%
      filter(county %in% input$county1) %>% 
      group_by(county, 
               acs_primary, 
               nonemerg_primary, 
               dental_primary, 
               mental_primary, 
               subabuse_primary) %>% 
      tally %>%
      ungroup() %>%
      group_by(county) %>% 
      summarise(percentage = (n/sum(n))*100, across(everything())) %>% 
      mutate(type = case_when(!dental_primary & !acs_primary & !subabuse_primary & !mental_primary & !nonemerg_primary ~ "Other",
                              dental_primary ~ "Dental",
                              acs_primary ~ "ACS",
                              subabuse_primary ~ "Substance Abuse",
                              mental_primary ~ "Mental Health",
                              nonemerg_primary ~ "Non emergent")) %>% 
      filter(type != "Other")
  })  
    
  observe({
    rv$zip <- scp %>%
      filter(Patient_Zip %in% input$zip1) %>% 
      group_by(Patient_Zip, 
               acs_primary, 
               nonemerg_primary, 
               dental_primary, 
               mental_primary, 
               subabuse_primary) %>% 
      tally %>%
      ungroup() %>%
      group_by(Patient_Zip) %>% 
      summarise(percentage = (n/sum(n))*100, across(everything())) %>% 
      mutate(type = case_when(!dental_primary & !acs_primary & !subabuse_primary & !mental_primary & !nonemerg_primary ~ "Other",
                              dental_primary ~ "Dental",
                              acs_primary ~ "ACS",
                              subabuse_primary ~ "Substance Abuse",
                              mental_primary ~ "Mental Health",
                              nonemerg_primary ~ "Non emergent")) %>% 
      filter(type != "Other")
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
           subtitle = "At the ER",
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
    leaflet(zipcodes) %>% 
      addPolygons(color = '#2166ac',
                  weight = 1,
                  smoothFactor = 0.25,
                  opacity = 1.0,
                  fillOpacity = 0.5,
                  highlightOptions = highlightOptions(color = "#b2182b", 
                                                      weight = 1.5,
                                                      bringToFront = TRUE),
                  label = ~paste0(ZCTA5CE10)) %>% 
      addTiles()
  })
  
  # DEMOGRAPHICS TAB
  #################################
  # output$county_plot <- renderPlot({
  #   ggplot(data = rv$county_demo, aes(x = Condition, y = percentage, color = type)) + geom_col()
  #   
  # })

  # PRIMARY CARE SERVICES TAB
  # county plot
  output$all_cond_county <- renderPlot({
    ggplot(data = rv$county, 
           aes(x = reorder(type, -percentage),
               y = percentage/100,
               fill = type)) +
      geom_col() +
      scale_y_continuous(labels = scales::percent) + 
      labs(x = " ",
           y = "Percentage of Patient Visits") +
      scale_fill_discrete(name = "Type of Condition") +
      theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank())
  })
  # zip code plot
  output$all_cond_zip <- renderPlot({
    ggplot(data = rv$zip, 
           aes(x = reorder(type, -percentage),
               y = percentage/100,
               fill = type)) +
      geom_col() +
      scale_y_continuous(labels = scales::percent) + 
      labs(x = " ",
           y = "Percentage of Patient Visits") +
      scale_fill_discrete(name = "Type of Condition") +
      theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
