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
    menuItem("About", tabName = "about", icon = icon("home")),
    menuItem("Background", tabName = "background", icon = icon("file")),
    menuItem("Overview", tabName = "overview", icon = icon("book-medical")),
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
      
      #ABOUT TAB
        tabItem(tabName = "about",
                includeMarkdown("www/home.Rmd")),
      #BACKGROUND TAB
        tabItem(tabName = "background", 
                includeMarkdown("www/overview.Rmd")),
      
      # EXPLORE ER OVERUSE DROPDOWN
      # MAP TAB
        tabItem(tabName = "map",
              includeMarkdown("www/er_overuse.Rmd"),
              fluidRow(
                column(6,
                       selectInput(inputId = 'zipcode',
                                   label = h3("Select Zip code"),
                                   choices = unique(hospitals$Patient_Zip),
                                   selected = 1)),
                column(6, 
                       leafletOutput("zipMap")))),
       
      #DEMOGRAPHICS TAB
        tabItem(tabName = "demo",
                includeMarkdown("www/demographics.Rmd"),
                #Demo Filter Widgets
                fluidRow(
                  column(3, #WIDGET -----
                         radioButtons( inputId = "sex",
                                       label = h3("Select Sex"),
                                       choices = c("Both", "M", "F"),
                                       selected = 'Both')),
                  column(3,
                         radioButtons("race", label = h3("Select Race"),
                                      choices = c("All", unique(scp$Race_Chr)), 
                                      selected = "All")),
                  column(3,
                         selectInput( inputId = "age",
                                      label = h3("Select Age Group"),
                                      choices = c('All', sort(unique(scp$age_group))),
                                      selected = "All"))),
                fluidRow(
                  column(6,
                selectInput( inputId = "county",
                             label = h3("Select County"),
                             choices = unique(scp$county),
                             selected = "Grundy"),
                plotOutput("county_plot")),
                column(6,
                         selectInput( inputId = "zip",
                                      label = h3("Select ZipCodes"),
                                      choices = unique(scp$Patient_Zip),
                                      selected = 1,
                                      multiple = TRUE),
                plotOutput("zip_plot"))),
                         selectInput( inputId = "insurance",
                                      label = h3("Select Insurance Type"),
                                      choices = unique(scp$Primary_Payer_Class_Cd),
                                      selected = 1,
                                      multiple = TRUE),
                plotOutput("insurance_plot")),
      
      #PRIMARY CARE SERVICE TAB
        tabItem(tabName = "care_service",
                # the below makes the "hr()" line black.
                tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;}"))),
                
                fluidRow(column(6, 
                                selectInput(inputId = "cond",
                                            label = h3("Select Condition Type"),
                                            choices = c("ACS" = "acs_perc",
                                                        "Non emergent" = "non_perc",
                                                        "Mental health" = "mental_perc",
                                                        "Dental" = "dental_perc",
                                                        "Substance abuse" = "sub_perc",
                                                        "Total ER visits" = "n"),
                                            selected = "acs_perc")),
                         column(6, 
                                leafletOutput("cond_map"))),
                br(),
                hr(),
                br(),
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
      tabItem(tabName= 'med_condition',
      fluidRow(
        column(4, 
               selectInput(inputId = 'county2',
                           label= h3 ('Select County'),
                           choices= unique(scp$county),
                           selected = 1)),
        column(4,
               
               # So that zip codes are dependent on county
               selectInput( inputId = 'zip2',
                            label= h3 ('Select Zip Code'),
                            choices= NULL
               )),
        
        column(4,
               
               selectInput( inputId = 'insurance2',
                            label= h3('Select Insurance Type'),
                            choices= unique(scp$Primary_Payer_Class_Cd),
                            selected=1))),
      
      # Putting the graph on its own row below
      fluidRow(
        column(6,
               
               #Displays the plot to the user
               #uiOutput('title')
               plotOutput('conditions_plot')))
      
    ),
    
      #FINDINGS TAB
        tabItem(tabName = "overview",
                includeMarkdown("www/findings.Rmd"),
                fluidRow(column(6,
                                plotOutput("er_overuse")),
                         column(6, 
                                plotOutput("county_graph"))),
                fluidRow(
                  plotOutput("insurance_overuse"),
                  plotOutput("admit_hr_graph"))
        )))
     
  
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
      mutate( type = case_when( !acs_primary & !nonemerg_primary ~ "Unpreventable",
                                acs_primary ~ "ACS", 
                                nonemerg_primary ~ "Non emergent" )) %>% 
      mutate(Condition = ifelse(type == "Unpreventable", "Unpreventable", "Preventable"))
  })  
  
  #DEMOGRAPHICS OBSERVE------
  observe({ #County DF
    #Make age_group column from factor to character
    scp$age_group <- as.character(scp$age_group)
    
    county_demo <- scp %>%
      filter(county %in% input$county) 
    
    # If Statements for all/both buttons
    if(input$sex != 'Both'){
      county_demo <- county_demo %>% filter(Patient_Sex == input$sex)
    }
    
    if(input$race != 'All'){
      county_demo <- county_demo %>% filter(Race_Chr == input$race)
    }
    
    if(input$age != 'All'){
      county_demo <- county_demo %>% filter(age_group == input$age)
    }
    
    # Continue DF Altering
    county_demo <- county_demo %>% 
      group_by(county, acs_primary, nonemerg_primary) %>%
      tally %>%
      ungroup() %>%
      group_by(county) %>%
      mutate(total = sum(n)) %>%
      summarise(percentage = (n/sum(n))*100, across(everything())) %>%
      mutate( type = case_when( !acs_primary & !nonemerg_primary ~ "Non-Preventable",
                                acs_primary ~ "ACS",
                                nonemerg_primary ~ "Non emergent" )) %>%
      mutate(Condition = ifelse(type == "Non-Preventable", "Non-Preventable", "Preventable"))
    
    # Make reactive DF
    rv$county_demo <- county_demo
  })
  observe({ #Zip DF
    
    #Make age_group column from factor to character
    scp$age_group <- as.character(scp$age_group)
    
    zip_demo <- scp %>%
      filter(Patient_Zip %in% input$zip) 
    
    # If Statements for all/both buttons
    if(input$sex != 'Both'){
      zip_demo <- zip_demo %>% filter(Patient_Sex == input$sex)
    }
    
    if(input$race != 'All'){
      zip_demo <- zip_demo %>% filter(Race_Chr == input$race)
    }
    
    if(input$age != 'All'){
      zip_demo <- zip_demo %>% filter(age_group == input$age)
    }
    
    # Continue DF Altering
    zip_demo <- zip_demo %>% 
      group_by(Patient_Zip, acs_primary, nonemerg_primary) %>%
      tally %>%
      ungroup() %>%
      group_by(Patient_Zip) %>%
      mutate(total = sum(n)) %>%
      summarise(percentage = (n/sum(n))*100, across(everything())) %>%
      mutate( type = case_when( !acs_primary & !nonemerg_primary ~ "Other",
                                acs_primary ~ "ACS",
                                nonemerg_primary ~ "Non emergent" )) %>%
      mutate(Condition = ifelse(type == 'Other', "Other", "ACS/Non emergent"))
    
    # Make reactive DF
    rv$zip_demo <- zip_demo
    
  })

    observe({ #Insurance DF
      
      #Make age_group column from factor to character
      scp$age_group <- as.character(scp$age_group)
      
      insurance_demo <- scp %>%
        filter(Primary_Payer_Class_Cd %in% input$insurance) 
      
      # If Statements for all/both buttons
      if(input$sex != 'Both'){
        insurance_demo <- insurance_demo %>% filter(Patient_Sex == input$sex)
      }
      
      if(input$race != 'All'){
        insurance_demo <- insurance_demo %>% filter(Race_Chr == input$race)
      }
      
      if(input$age != 'All'){
        insurance_demo <- insurance_demo %>% filter(age_group == input$age)
      }
      
      # Continue DF Altering
      insurance_demo <- insurance_demo %>% 
        group_by(Primary_Payer_Class_Cd, acs_primary, nonemerg_primary) %>%
        tally %>%
        ungroup() %>%
        group_by(Primary_Payer_Class_Cd) %>%
        mutate(total = sum(n)) %>%
        summarise(percentage = (n/sum(n))*100, across(everything())) %>%
        mutate( type = case_when( !acs_primary & !nonemerg_primary ~ "Other",
                                  acs_primary ~ "ACS",
                                  nonemerg_primary ~ "Non emergent" )) %>%
        mutate(Condition = ifelse(type == 'Other', "Other", "ACS/Non emergent"))
      
      # Make reactive DF
      rv$insurance_demo <- insurance_demo
      
    })
    
  
  #MEDICAL SERVICES OBSERVE----

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
  
  #ER Conditions 
  # This makes zip code options dependent on county selected 
  county2 <- reactive({
    filter(scp, county == input$county2)
  })
  
  observeEvent(county2(), {
    choices <- unique(county2()$Patient_Zip)
    updateSelectInput(inputId = "zip2", choices = choices)
  })
  
  ## Create reactive values
  # ^ Let us control which parts of your app update when, which prevents unnecessary computation that can slow down your app

  observe({
    rv$conditions <- scp %>% 
      filter(county %in% input$county2,
             Primary_Payer_Class_Cd %in% input$insurance2,
             Patient_Zip %in% input$zip2) %>% 
      group_by(Diag1, county_total) %>% 
      tally() %>% 
      arrange(desc(n))%>% 
      summarise(perc = n/county_total*100) %>% 
      arrange(desc(perc)) %>% 
      head(5)
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
      scale_fill_manual(values=c('#41b6c4',
                                 '#253494',
                                 '#fdcc8a'),
                        name = "Type of Condition") +
      scale_y_continuous(labels = scales::percent) + 
      labs(title = "Comparison of Primary Diagnosis Conditions",
           y = "Percentage of Visits to the ER") + 
      geom_text(aes(label = scales::percent(percentage/100)),
                position = position_stack(vjust = 1.1)) + 
      theme_light(base_size = 18) 
  })
  
  output$county_graph <- renderPlot({
    
    #Filter out "other" conditions
    tn_diags <- tn_diags %>% filter(county != "Other", Condition != "Other")
    
    #Graph
    ggplot(data = tn_diags, aes(x = county,y = precent/100, fill = type)) +
      geom_col(position = "dodge")+
      labs(title = "Comparison of Primary Diagnosis Conditions",
           y = "Percentage of Visits",
           x = '') +
      scale_y_continuous(labels = scales::percent) +
      labs(title = "Comparison of Primary Diagnosis Conditions",
           subtitle = "SCP Counties vs Williamson County",
           y = "% Visits to the ER") + 
      geom_text(aes(label = scales::percent(precent/100)),
                position = position_dodge(width = .9), 
                vjust = -.4)  + 
    theme_light(base_size = 18) + 
      scale_fill_manual(values=c('#41b6c4',
                                 '#253494'), 
                         name = "Type of Condition")
  })
  
  output$insurance_overuse <- renderPlot({
    # Overuse by insurance code
    insurance_overuse <- scp %>%
      filter(insurance == c("TennCare", "MediCare", "Self Paid", "Commercial")) %>% 
      group_by(insurance, acs_primary, nonemerg_primary) %>% 
      tally %>% 
      ungroup() %>% 
      group_by(insurance) %>% 
      mutate(total = sum(n)) %>%
      summarise(percentage = n/total*100, across(everything())) %>% 
      mutate(type = case_when(!acs_primary & !nonemerg_primary ~ "Appropriate Use", 
                              acs_primary ~ "ACS",
                              nonemerg_primary ~ "Nonemergent")) %>% 
      mutate(type2 = ifelse(type == "Appropriate Use", "Appropriate Use", "Overuse")) %>% 
      filter(type2 != "Appropriate Use")
    
    # Overuse by insurance 
    ggplot(data = insurance_overuse, aes(x = reorder(insurance, -percentage), y = percentage/100, fill = type)) +
      geom_col(position = 'dodge') +
      scale_fill_manual(name = 'Type of Condition',
                        values=c('#41b6c4',
                                 '#253494')) +
      scale_y_continuous(labels = scales::percent) + 
      labs(x = 'Insurance', y = 'Percentage of Visits',
           title = 'Severity of Overuse by Insurance Type') +
      theme_light(base_size = 18)
  })
  
  output$admit_hr_graph <- renderPlot({
    #Admit Hour Graph Code
    scp$Admit_Hr <- as.numeric(scp$Admit_Hr)
    overuse_hour <- scp %>% 
      group_by(Admit_Hr, acs_primary, nonemerg_primary) %>% 
      drop_na(Admit_Hr) %>% 
      mutate( type = case_when( !acs_primary & !nonemerg_primary ~ "Unpreventable",
                                acs_primary ~ "ACS", 
                                nonemerg_primary ~ "Non emergent" )) %>% 
      mutate(Condition = ifelse(type == "Unpreventable", "Unpreventable", "Preventable")) %>% 
      group_by(Admit_Hr, Condition) %>% tally()
    
    #Admit Hour Graph
    ggplot(data = overuse_hour) +
      geom_point(mapping = aes(x = Admit_Hr, y = n, color = Condition)) + 
      geom_line(aes(x = Admit_Hr, y = n, color = Condition)) +
      theme_light(base_size = 18) +
      scale_color_manual(values=c('#41B6C4',
                                 '#253494'),
                        name = "Type of Condition") +
      labs( x = "Patient Admit Hour",
            y = "# of Visits",
            title = "ER Visits by Admit Hour")
    
  })
  
  
  ##############################################################################
  # EXPLORE ER TAB GRAPHS ------
  
  # MAPS TAB
  #################################

  output$zipMap <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>%
      addPolygons(data = hospitals[hospitals$Patient_Zip %in% input$zipcode,]$geometry,
                  color = '#ca0020',
                  weight = 0.5,
                  smoothFactor = 0.25,
                  opacity = 0.5,
                  fillOpacity = 0.25,
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 1.5,
                                                      opacity = 0.5,
                                                      bringToFront = TRUE),
                  label = paste0("Zip code: ", input$zipcode)) %>%
    addMarkers(lat = hospitals[hospitals$Patient_Zip %in% input$zipcode,]$latitude,
               lng = hospitals[hospitals$Patient_Zip %in% input$zipcode,]$longitude)
  })
  
  # DEMOGRAPHICS TAB
  #################################
  
  #COUNTY PLOT
  output$county_plot <- renderPlot({
     ggplot(data = rv$county_demo, 
            aes(x = reorder(type, -percentage), 
                y = percentage/100, 
                fill = type)) + 
      geom_col(position = "dodge") +
      labs(title = "ER Overuse of Demographic",
         subtitle = "In County",
         y = "Percentage of Visits",
         x = '') +
      theme_light(base_size = 18) +
      scale_fill_manual(values=c('#fdcc8a',
                                 '#a1dab4',
                                 '#253494'),
                        name = "Type of Condition") +
      scale_y_continuous(labels = scales::percent) + 
      labs(title = "Comparison of Primary Diagnosis Conditions",
           y = "Percentage of Visits to the ER") +
      geom_text(aes(label = scales::percent(percentage/100)),
                position = position_dodge(width = 0.9), 
                vjust = -.5)
   })
  
  #ZIPCODE PLOT
  output$zip_plot <- renderPlot({
    ggplot(data = rv$zip_demo, 
           aes(x = factor(Patient_Zip),
              y = percentage/100,
              fill = type)) + 
      geom_col(position = "dodge") +
      labs(title = "ER Overuse of Demographic",
           subtitle = "In Zipcodes",
           y = "Percentage of Visits",
           x = '') +
      theme_light(base_size = 18) +
      scale_fill_manual(values=c('#fdcc8a',
                                 '#a1dab4',
                                 '#253494'),
                        name = "Type of Condition") +
      scale_y_continuous(labels = scales::percent) + 
      labs(title = "Comparison of Primary Diagnosis Conditions",
           y = "Percentage of Visits to the ER") +
      geom_text(aes(label = scales::percent(percentage/100)),
                position = position_dodge(width = 0.9), 
                vjust = -.5)
  })
  
  #INSURANCE PLOT
  output$insurance_plot <- renderPlot({
    ggplot(data = rv$insurance_demo, aes(x = factor(Primary_Payer_Class_Cd), 
                                         y = percentage/100, 
                                         fill = type)) + 
      geom_col(position = "dodge") +
      labs(title = "ER Overuse of Demographic",
           subtitle = "In Zipcodes",
           y = "Percentage of Visits",
           x = '') +
      theme_light(base_size = 18) +
      scale_fill_manual(values=c('#fdcc8a',
                                 '#a1dab4',
                                 '#253494'),
                        name = "Type of Condition") +
      scale_y_continuous(labels = scales::percent) + 
      labs(title = "Comparison of Primary Diagnosis Conditions",
           y = "Percentage of Visits to the ER") +
      geom_text(aes(label = scales::percent(percentage/100)),
                position = position_dodge(width = 0.9), 
                vjust = -.5)
  })

  # PRIMARY CARE SERVICES TAB
  ################################
  # condition map leaflet
  output$cond_map <- renderLeaflet({
    # if(input$cond == "acs_perc"){
    #   legend_title <- "ACS Conditions"}
    # 
    # if(input$cond == "non_perc"){
    #   legend_title <- "Non Emergent"}
    # 
    # if(input$cond == "mental_perc"){
    #   legend_title <- "Mental Health"}
    # 
    # if(input$cond == "dental_perc"){
    #   legend_title <- "Dental Conditions"}
    # 
    # if(input$cond == "sub_perc"){
    #   legend_title <- "Substance Abuse"}
    
    leaflet(combine) %>%
      addTiles() %>%
      addPolygons(color = ~pal(get(input$cond)),
                  weight = 1,
                  smoothFactor = 0.25,
                  opacity = 1.0,
                  fillOpacity = 0.5,
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 1.0,
                                                      bringToFront = TRUE),
                  label = ~lapply(paste0("Zip code: ", combine$Patient_Zip, ",",
                                         "<br/>",
                                         paste(round(get(input$cond), 2)), "%"), HTML)) %>%
      addLegend("bottomright",
                pal = pal,
                values = (combine %>% pull(!!input$cond) %>% range), #creates range of values within column selected
                title = "% of Visits",
                opacity = 1)
  })
  
  # county plot
  output$all_cond_county <- renderPlot({
    ggplot(data = rv$county, 
           aes(x = reorder(type, -percentage),
               y = percentage/100,
               fill = type)) +
      geom_col() +
      scale_y_continuous(labels = scales::percent) + 
      theme_light(base_size = 18) +
      labs(x = " ",
           y = "Percentage of Patient Visits") +
      scale_fill_manual(values=c('#fdcc8a',
                                 '#a1dab4',
                                 '#41b6c4',
                                 '#2c7fb8',
                                 '#253494'),
                        name = "Type of Condition") +
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
      theme_light(base_size = 18)+
      labs(x = " ",
           y = "Percentage of Patient Visits") +
      scale_fill_manual(values=c('#fdcc8a',
                                 '#a1dab4',
                                 '#41b6c4',
                                 '#2c7fb8',
                                 '#253494'),
                        name = "Type of Condition") +
      theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank())
  })


# ER Conditions Tab
#################################
output$conditions_plot <- renderPlot({
  ggplot(data = rv$conditions, aes(x= Diag1,
                                   y= perc, fill= Diag1)) +
    geom_col()+
    # This puts percent sign on y-axis
    scale_y_continuous(labels = scales::percent)+
    labs(title= 'Top 5 Primary Diagnoses', x= 'Specific Diagnosis', y= 'Percentage of Patients')+
    # Gives a minimalistic theme to the graph and changes the text size on the graph
    theme_light(base_size = 18)+
    scale_fill_manual(values= c('#fdcc8a',
                                '#a1dab4',
                                '#41b6c4',
                                '#2c7fb8',
                                '#253494'))
  
})

}

# Run the application 
shinyApp(ui = ui, server = server)
