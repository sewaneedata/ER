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
              leafletOutput("zipMap")), #MAYBE ADD SPINNER WHILE LOADING
       
      #DEMOGRAPHICS TAB
        tabItem(tabName = "demo",
                includeMarkdown("www/demographics.Rmd"),
                #Demo Filter Widgets
                fluidRow(
                  column(3,
                         radioButtons( inputId = "sex",
                                       label = h3("Select Sex"),
                                       choices = c("M", "F"),
                                       #choices = c("M", "F", "Both"),
                                       selected = 1)),
                  column(3,
                         radioButtons("race", label = h3("Select Race"),
                                     choices = unique(scp$Race_Chr), #Remove unknown
                                     selected = 1)),
                  column(3,
                         selectInput( inputId = "age",
                                       label = h3("Select Age Group"),
                                       choices = unique(scp$age_group),
                                       selected = "0-9"))),
                         selectInput( inputId = "county",
                                      label = h3("Select County"),
                                      choices = unique(scp$county),
                                      selected = 1,
                                      multiple = TRUE),
                plotOutput("county_plot"),
                         selectInput( inputId = "zip",
                                      label = h3("Select ZipCodes"),
                                      choices = unique(scp$Patient_Zip),
                                      selected = 1,
                                      multiple = TRUE),
                plotOutput("zip_plot"),
                         selectInput( inputId = "insurance",
                                      label = h3("Select Insurance Type"),
                                      choices = unique(scp$Primary_Payer_Class_Cd),
                                      selected = 1,
                                      multiple = TRUE),
                plotOutput("insurance_plot")),
      
      #PRIMARY CARE SERVICE TAB
        tabItem(tabName = "care_service",
                
                fluidRow(column(6, 
                                selectInput(inputId = "cond",
                                            label = h3("Select Condition Type"),
                                            choices = c("ACS visits" = "acs_perc",
                                                        "Non emergent visits" = "non_perc",
                                                        "Mental health visits" = "mental_perc",
                                                        "Dental visits" = "dental_perc",
                                                        "Substance abuse visits" = "sub_perc",
                                                        "Total ER visits" = "n"),
                                            selected = "Dental visits"))),
                fluidRow(column(6, 
                                leafletOutput("cond_map")),
                         br(),
                         hr(),
                         br()),
               
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
        tabItem(tabName = "overview",
                includeMarkdown("www/findings.Rmd"),
                plotOutput("er_overuse"),
                plotOutput("county_graph"),
                plotOutput("admit_hr_graph"))
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
  
  #DEMOGRAPHICS OBSERVE------
  
  observe({ #County
    rv$county_demo <- scp %>%
      filter(Race_Chr %in% input$race,
              age_group %in% input$age,
              county %in% input$county) %>% 
      # ifelse(input$sex !='Both', 
             #rv$county_demo <- rv$county_demo %>% filter(Patient_Sex == input$sex),  %>% 
      group_by( county, acs_primary, nonemerg_primary) %>%
      tally %>%
      ungroup() %>%
      group_by(county) %>%
      mutate(total = sum(n)) %>%
      summarise(percentage = (n/sum(n))*100, across(everything())) %>%
      mutate( type = case_when( !acs_primary & !nonemerg_primary ~ "Other",
                                acs_primary ~ "ACS",
                                nonemerg_primary ~ "Non emergent" )) %>%
      mutate(Condition = ifelse(type == 'Other', "Other", "ACS/Non emergent"))
    
    
    
    rv$zip_demo <- scp %>%
      filter( Patient_Sex %in% input$sex,
              Race_Chr %in% input$race,
              age_group %in% input$age,
              Patient_Zip %in% input$zip) %>%
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
    
    rv$insurance_demo <- scp %>%
      filter( Patient_Sex %in% input$sex,
              Race_Chr %in% input$race,
              age_group %in% input$age,
              Primary_Payer_Class_Cd %in% input$insurance) %>%
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
  })
  
  #MEDICAL SERVICES OBSERVE----
  observe({
    
    combine <- left_join(zipcodes, scp_map, by = "Patient_Zip")
    pal <- colorNumeric(palette = c('#0571b0','#92c5de', '#f7f7f7', '#f4a582', '#ca0020'),
                        domain = combine$input$cond)
  })
  
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
  
  output$county_graph <- renderPlot({
    #Read in TN ER Records
    tn_diags <- readr::read_csv("C:/Users/jplus/OneDrive/Documents/DataLab/ER_Usage/tn_conditions.csv")
    
    #Filter out "other" conditions
    tn_diags <- tn_diags %>% filter(county != "Other", Condition != "Other")
    
    #Graph
    ggplot(data = tn_diags, aes(x = county,y = precent/100, fill = type)) +
      geom_col(position = "dodge")+
      labs(title = "Comparison of Primary Diagnosis Conditions",
           y = "Percentage of Visits",
           x = '') +
      scale_fill_manual(values=c("#74A9CF",
                                 "#08306B"),
                        name = "Type of Condition") +
      scale_y_continuous(labels = scales::percent) +
      labs(title = "Comparison of Primary Diagnosis Conditions",
           subtitle = "SCP Counties vs Williamson County",
           y = "% Visits to the ER") + 
      geom_text(aes(label = scales::percent(precent/100)),
                position = position_dodge(width = .9), 
                vjust = -.4)
  })
  
  output$admit_hr_graph <- renderPlot({
    #Admit Hour Graph Code
    admit_hour <- scp %>% group_by(Admit_Hr) %>% 
      drop_na(Admit_Hr) %>% 
      tally()
    
    #Admit Hour Graph
    ggplot(data = admit_hour, aes(x = Admit_Hr,y = n)) +
      geom_line(aes(group=1), color = "#74a9cf", size = 2) +
      geom_point(color = "#08306b", size = 3) +
      labs( x = "Patient Admit Hour",
            y = "Number of Visits",
            title = "Number of Visits by Hour")
    
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
                  label = ~paste0(Patient_Zip)) %>% 
      addTiles()
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
      scale_fill_manual(values=c("#74a9cf",
                                 "#08306b",
                                 "#c6dbef"), #CAN ONLY SELECT 3 SINCE ONLY 3 COLORS
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
      scale_fill_manual(values=c("#74a9cf",
                                 "#08306b",
                                 "#c6dbef"),
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
      scale_fill_manual(values=c("#74a9cf",
                                 "#08306b",
                                 "#c6dbef"),
                        name = "Type of Condition") +
      scale_y_continuous(labels = scales::percent) + 
      labs(title = "Comparison of Primary Diagnosis Conditions",
           y = "Percentage of Visits to the ER") +
      geom_text(aes(label = scales::percent(percentage/100)),
                position = position_dodge(width = 0.9), 
                vjust = -.5)
  })

  # PRIMARY CARE SERVICES TAB
  #################################
  # condition map leaflet
  output$cond_map <- renderLeaflet({
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
                values = c(1, 2, 3),
                title = " ",
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
      labs(x = " ",
           y = "Percentage of Patient Visits") +
      scale_fill_manual(values=c("#d7191c",
                                 "#fdae61",
                                 "#ffffbf",
                                 "#abd9e9",
                                 "#2c7bb6"),
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
      labs(x = " ",
           y = "Percentage of Patient Visits") +
      scale_fill_manual(values=c("#d7191c",
                                 "#fdae61",
                                 "#ffffbf",
                                 "#abd9e9",
                                 "#2c7bb6"),
                        name = "Type of Condition") +
      theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
