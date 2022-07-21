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
library(shinyjs)

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
    menuItem("Background", tabName = "background", icon = icon("file")),
    menuItem("Primary Findings", tabName = "findings", icon = icon("book-medical")),
    menuItem("Explore ER Overuse", tabName = "ER_Overuse", icon = icon("hospital"),
             menuSubItem("Map", tabName = "map"),
             menuSubItem("Demographics", tabName = "demo"),
             menuSubItem("Types of Conditions", tabName = "care_service"),
             menuSubItem("Top Diagnoses", tabName = "med_condition")),
    menuItem("About", tabName = "about", icon = icon("home"))
    ))
  
  # BODY ----
  ##########################
  body <- dashboardBody(
    useShinyjs(),
    #CSS Edits to Dashboard header and nav bar
    tags$head(
      tags$style(HTML("         /*Change color of 'Investigating ER Overuse'*/
                                .skin-blue .main-header .logo {
                                background-color: #1b266b;
                                }
                                
                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #253494;}
                                
                                /* Color change for selected menu item */
                                .skin-blue .sidebar-menu > li.active > a,
                                .skin-blue .sidebar-menu > li:hover > a {
                                  border-left-color: #253494;}")
                 )),
    tabItems(
      #Adds content to each tab
      
      #ABOUT TAB ------
        tabItem(tabName = "about",
                HTML(paste0("<h1><b>About</h1></b>",
                  "<hr>",
                  "<p>During the Summer of 2022, DataLab fellows created this dashboard for the South Cumberland Health Network to provide them with the data analysis needed to fill the large primary care health service gaps that are prevelant on the South Cumberland Plateau.</p>",
                  "<h2><b>What is DataLab?</h2></b>",
                  "<p>Sewanee Datalab is a data science for social good program hosted at Sewanee: The University of The South. This program trains aspiring data scientists that work exclusively on social impact projects partnered with clients and organizations.</p>",
                  "<h2><b>Our Community Partner</h2></b>",
                  "<p>The South Cumberland Health Network (SCHN) is a non-profit organization that works to remediate barriers to health care access among residents of Grundy county and parts of  Franklin and Marion counties on the South Cumberland Plateau of Tennessee. The SCHN serves medically underserved, low-income, and minority populations. </p>",
                  "<h2><b>The Fellows</h2></b>")),
                fluidRow(column(2,
                                HTML(paste0("<img src='Ellie_Davis.JPG' alt='' width='150' height='150' align='left'>"))
                                ),
                column(10,
                       HTML(paste0("<h3><b>Ellie Davis</h3></b>",
                                   "<p>Senior at Sewanee: The University of the South; Class of 2023. Majoring in Politics & Women’s & Gender studies.</p>",
                                   "<a href='https://www.linkedin.com/in/elizabeth-davis-a96366230/'>LinkedIn</a>"))
                        )),
                br(),
                fluidRow(column(2,
                                HTML(paste0("<img src='Jenna_Lusk.JPG' alt='' width='150' height='150' align='left'>"))
                ),
                column(10,
                       HTML(paste0("<h3><b>Jenna Lusk</h3></b>",
                                   "<p>Sophomore at Purdue University; Class of 2025. Majoring in Computer Information Technology and minoring in Design & Innovation and Organizational Leadership.</p>",
                                   "<a href='https://www.linkedin.com/in/jenna-lusk-5849a1200'>LinkedIn</a>"))
                )),
                br(),
                fluidRow(column(2,
                                HTML(paste0("<img src='Kenedi_Clinton.JPG' alt='' width='150' height='150' align='left'>"))
                ),
                column(10,
                       HTML(paste0("<h3><b>Kenedi Clinton</h3></b>",
                                   "<p>Junior at Sewanee: The University of The South; Class of 2024. Majoring in Biology, minoring in Rhetoric, and receiving a Civic and Global Leadership certificate.</p>",
                                   "<a href='https://www.linkedin.com/in/kenedi-clinton-6ba5b61ba'>LinkedIn</a>"))
                ))), 
      
      #BACKGROUND TAB ------
        tabItem(tabName = "background", 
                includeMarkdown("www/background.Rmd")),
      
      # EXPLORE ER OVERUSE DROPDOWN
      # MAP TAB ------
        tabItem(tabName = "map",
                HTML(paste0("<h1><b>ER Overuse by Zip Code</b></h1>",
                            "<p>Exploring where ER overuse is coming from provides a great insight into what communities are facing medical gaps the most. The following maps use a scale of blue to red in order to show severity of overuse per county. The maps use blue markers to show the top 10 most visited hospitals by residents of the plateau. The red dots are urgent cares, and the green dots are primary care doctors.</p>")),
                # Line Code
                hr(),
                tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;}"))),
                fluidRow(column(8,
                              leafletOutput("top10_hospitals")),
                         column(4, HTML(paste0("<p> The map visualizes ER overuse on the Plateau broken 
                                               down by zip code. We found overuse is most prevalent in 
                                               Sequatchie, TN, with 43.7% of ER visits being instances 
                                               of overuse. This area is noted as having no urgent care 
                                               (red dots) nor primary care doctors (green dots). 
                                               The hospitals shown are the top 10 most visited by patients from the SCP.</p>",
                                               "<h3><b>Legend</b></h3>",
                                               "<img src='legend_map.PNG' width='150' height='125' align='center'>")),
                         )),
                hr(),
                fluidRow(
                  column(4,
                         HTML(paste0(
                           "<h3><b>Instructions</b></h3>",
                           "<p>Select a zip code for which you'd like to see 
                           the top three most visited hospitals from patients from the selected zip code. </p>"
                           )),
                         selectInput(inputId = 'zipcode',
                                     label = h3("Select Zip code"),
                                     choices = unique(hospitals$Patient_Zip),
                                     selected = 1),
                         HTML(paste0("<h3><b>Zip Code Key</b></h3>")),
                         fluidRow(column(4,
                                         HTML(paste0("<h6>37301 - Altamont</h6>",
                                                     "<h6>37305 - Beersheba Springs</h6>",
                                                     "<h6>37313 - Coalmont</h6>",
                                                     "<h6>37339 - Gruetli-Laager</h6>"
                                                     ))),
                                  column(4, HTML(paste0("<h6>37356 - Monteagle</h6>",
                                                        "<h6>37365 - Palmer</h6>",
                                                        "<h6>37366 - Pelham</h6>",
                                                        "<h6>37374 - Sequatchie</h6>"))),
                                  column(4, 
                                         HTML(paste0("<h6>37375 - Sewanee</h6>",
                                                     "<h6>37387 - Tracy City</h6>",
                                                     "<h6>37397 - Whitwell</h6>"))
                                         ))),
                  column(8,
                         leafletOutput("zipMap")))),
       
      #DEMOGRAPHICS TAB ------
        tabItem(tabName = "demo",
                HTML(paste0("<h1><b>ER Overuse by Demographics</b></h1>",
                            "<p>Analyzing patient demographics and ER overuse can reveal trends regarding what groups of people face medical gaps or if there is a specific demographic that is significantly underserved. The following graphs show overuse by ACSC and non-emergent by the selected demographics.</p>")),
                hr(),
                HTML(paste0("<h3><b>Instructions</b></h3>",
                            "<p>Begin by selecting a sex, race, and age range*. Then below the black line, you may generate graphs for county, zip code, and insurance type by selecting an option from the drop down menus. You may select multiple zip codes to show on the graph.</p>")),
                #Demo Filter Widgets
                fluidRow(
                  column(2),
                  fluidRow(
                    column(8,
                  box( status = "primary",
                  column(4, 
                         #WIDGET -----
                         radioButtons( inputId = "sex",
                                       label = h3("Select Sex"),
                                       choices = c("Both", "M", "F"),
                                       selected = 'Both')),
                  column(4,
                         radioButtons("race", label = h3("Select Race"),
                                      choices = c("All", unique(scp$Race_Chr)), 
                                      selected = "All")),
                  column(4,
                         selectInput( inputId = "age",
                                      label = h3("Select Age Group"),
                                      choices = c('All', sort(unique(scp$age_group))),
                                      selected = "All")),
                  width = 10))),
                  column(2)),
                  HTML(paste0("<h6> * = Ages 70-99 are grouped into one due to Federal Law</h6>")),
                hr(),
                fluidRow(
                  column(6,
                         box( status = "primary", 
                selectInput( inputId = "county",
                             label = h3("Select County"),
                             choices = unique(scp$county),
                             selected = "Grundy"),
                plotOutput("county_plot"), width = 14)),
                column(6,
                       box( status = "primary",
                         selectInput( inputId = "zip",
                                      label = h3("Select ZipCodes"),
                                      choices = unique(scp$Patient_Zip),
                                      selected = 1,
                                      multiple = TRUE),
                plotOutput("zip_plot"), width = 14))),
                br(),
                br(),
                fluidRow(column(2),
                         column(8,
                                box( status = "primary", width = 15, 
                                selectInput( inputId = "insurance",
                                              label = h3("Select Insurance Type"),
                                              choices = c('MediCare', 'TennCare', 'Self Pay', "Commercial"),
                                              selected = 'Medicare'),
                                plotOutput("insurance_plot"))),
                         column(2))),
      
      #Types of Conditions TAB ------
        tabItem(tabName = "care_service",
                HTML(paste0("<h1><b>ER Overuse by Types of Conditions</b></h1>",
                            "<p>Looking into what kinds of ER overuse conditions can help pinpoint what kind of healthcares are lacking in a community. The map and graphs below show the percentage of ER visits that are for ACSC and non-emergent conditions, and also analyze dental, mental health, and substance use conditions to find lack of services for these specific healthcare needs.</p>")),
                hr(),
                fluidRow(column(8,
                                plotOutput("scp_conditions")),
                         column(4, 
                                HTML(paste0("<p>The graph indicates overuse for several 
                                            different kinds of conditions. These conditions 
                                            can all be treated in other healthcare facilities. 
                                            This raises questions about a possible deficiency 
                                            in services within the SCP communities. </p>"))
                                )),
                # the below makes the "hr()" line black.
                tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;}"))),
                hr(),
                HTML(paste0("<h3><b>Instructions</b></h3>",
                            "<p>The map below visualizes the percentage of ER visits that were under certain conditions from each county in the SCP. Select what kind of condition is wanted for analysis and the map will generate. Be mindful that the graph may take a few seconds to load.</p>")),
                fluidRow(column(2),
                         column(8,
                fluidRow(box(status = "primary", width = 15, 
                             column(4, 
                                selectInput(inputId = "cond",
                                            label = h3("Select Condition Type"),
                                            choices = c("ACS" = "acs_perc",
                                                        "Non emergent" = "non_perc",
                                                        "Mental health" = "mental_perc",
                                                        "Dental" = "dental_perc",
                                                        "Substance abuse" = "sub_perc"),
                                            selected = "acs_perc")),
                         column(8, 
                                leafletOutput("cond_map"))))),
                column(2)),
                br(),
                hr(),
                HTML(paste0("<h3><b>Instructions</b></h3>",
                            "<p>The graphs below depict the percentage of ER visits were for different kinds of overuse conditions. Select by county, zip code, or insurance types, and the corresponding graph will generate.</p>")),
                br(),
                fluidRow(
                      column(6,
                        box(status = "primary", width = 15,
                         selectInput(inputId = "county1",
                                     label= h3("Select County"),
                                     choices = unique(scp$county),
                                     selected = "Grundy"),
                         plotOutput("all_cond_county"))),
                      column(6,
                        box(status = "primary", width = 15,
                         selectInput(inputId = "zip1",
                                     label = h3("Select Zip Code"),
                                     choices = unique(scp$Patient_Zip),
                                     selected = "A"),
                         plotOutput("all_cond_zip")))),
                br(),
                br(),
                fluidRow(column(2), 
                         column(8,
                                box( status = "primary", width = 15,
                                selectInput(inputId = "ins",
                                            label = h3("Select Insurance"),
                                            choices = c('MediCare', 'TennCare', 'Self Pay', "Commercial"),
                                            selected = 'Medicare'),
                         plotOutput("all_cond_insurance"))),
                         column(2))),
      
      #DIAGNOSIS TAB -------
      tabItem(tabName= 'med_condition',
              HTML(paste0("<h1><b>ER Overuse by Diagnoses</b></h1>",
                          "<p>Analyzing what specific ICD-10 codes are most common amongst the SCP and specific demographics give an exact explanation about what overuse conditions residents are using the ER for.</p>")),
              hr(),
              fluidRow(
                column(9, plotOutput('icdscp_plot')),
                column(3, 
                       HTML(paste0("<p>This graph displays the top 10 diagnoses found in ER visits from SCP residents. These diagnoses are all conditions that could have been treated with primary care or urgent care. This provides insight into the needed health care accessibility for the community to decrease these instances of ER overuse.</p>"))
                       )),
              hr(),
              HTML(paste0("<h3><b>Instructions</b></h3>",
                          "<p>The following graphs depicts the top 5 ICD-10 codes for the selected sex, insurance type, and county/zip code. To begin, select what sex and what insurance type. Select if you would like to view the graph by county or zip code. Then, select which county/zip code you would like the graph to analyze.</p>")),
                fluidRow(
                  column(1),
                column(6,
                        box( status = "primary", width = 10,
                        column(3,
                                 radioButtons(inputId = 'sex2',
                                              label= h3('Select Sex'),
                                              choices= c('Both', 'M', 'F'),
                                              selected= 'Both')),
                        column(5, 
                               radioButtons(inputId = 'insurance2',
                                            label= h3('Select Insurance Type'),
                                            choices= c('All','TennCare','MediCare', 'Commercial', 'Self Pay'))
                        ),
                        column(3,
                               radioButtons(inputId = "filter_by",
                                            label=h3("Filter by"),
                                            choices = c("County", "ZIP code")),
                       ))),
                column(4,
                         box( status = "primary", width = 13,
                                     selectInput(inputId = 'county2',
                                                 label= h3 ('Select County'),
                                                 choices= unique(scp$county),
                                                 selected = 1),
                              selectInput( inputId = 'zip2',
                                                  label= h3 ('Select Zip Code'),
                                                  choices= unique(scp$Patient_Zip)
                                     )
                              )),
                column(1)), 
              HTML(paste0("<h6>NOTE: Click the following link to look up what a specific ICD-10 code means: <a href='https://www.icd10data.com/'>ICD-10 Code Search Engine</a></h6>")),
              hr(),
                fluidRow(
                  column(2),
                  column(8,
                    plotOutput('conditions_plot')),
                column(2))
    ),
    
      #FINDINGS TAB ----
        tabItem(tabName = "findings",
                # Make hr() lines black
                HTML(paste0("<h1><b>Primary Findings</b></h1>",
                            "<hr>")),
                fluidRow(column(8,plotOutput("er_overuse")), 
                         column(4,
                                HTML(paste0("<h2><b>ER Overuse</b></h2>",
                                            "<p>Our data analysis revealed that <b>40.9%</b> of ER visits 
                                            from patients in the SCP were instances of ER Overuse, 
                                            double the national average of 19%.</p>")),
                                )),

                tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;}"))),

                
                hr(),
                fluidRow(column(8, 
                                plotOutput("county_graph")),
                         column(4,
                                HTML(paste0("<h2><b>Overuse by County</b></h2>",
                                            "<p>Williamson county is the healthiest county in 
                                            Tennessee. Grundy county, which makes up a large part 
                                            of the South Cumberland Plateau, is among the least healthy
                                            counties in all of Tennessee. This graph compares ER overuse
                                            in the South Cumberland Plateau counties with ER overuse in 
                                            Williamson county. The graph shows that ER overuse is higher 
                                            in the counties that make up the South Cumberland Plateau 
                                            than it is in Williamson county. </p>")))),
                hr(),
                fluidRow(column(8, 
                                plotOutput("insurance_overuse")),
                         column(4, 
                                HTML(paste0("<h2><b>Overuse by Insurance Type</b></h2>",
                                "<p>The data analysis reveals that patients 
                                            with state/federal government insurance or who self-pay (uninsured) 
                                            use the ER to treat ACS conditions at a greater rate than those with 
                                            commercial insurance. The findings may be related to the fact that ERs 
                                            must treat patients regardless of insurance or lack thereof unlike primary 
                                            and urgent care clinics.</p>")))),
                hr(),
                fluidRow(column(8,
                                plotOutput("admit_hr_graph")),
                         column(4,
                                HTML(paste0("<h2><b>Overuse by Admit Hour</b></h2>",
                                            "<p>One of our partner’s initial hypotheses was 
                                            that there would potentially be a difference in 
                                            the time of day that patients visited the ER for 
                                            “appropriate” versus “overuse” reasons. This graph 
                                            does not support that hypothesis. This lack of evidence 
                                            supporting the initial hypothesis may indicate that the 
                                            problem(s) influencing ER overuse on the Plateau lie 
                                            elsewhere in the system. For instance, is the issue that 
                                            urgent cares or primary care clinics will not accept certain 
                                            types of insurance?  Are urgent cares in the area too far 
                                            away, or much farther away than certain ERs? Do the primary 
                                            care clinics on the Plateau offer all needed health care 
                                            services?</p>"))))
                
        )))
     
  
  # Compile UI ----
  ########################
  ui <- dashboardPage( header, sidebar, body)

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
      mutate( type = case_when( !acs_primary & !nonemerg_primary ~ "Appropriate Use",
                                acs_primary ~ "ACS", 
                                nonemerg_primary ~ "Non emergent" )) %>% 
      mutate(Condition = ifelse(type == "Appropriate Use", "Appropriate Use", "Overuse"))
  })  
  
  #DEMOGRAPHICS OBSERVE------
  observe({ #County DF
    #Make age_group column from factor to character
    scp$age_group <- as.character(scp$age_group)
    
    county_demo <- scp %>%
      filter(county %in% input$county) 
    
    # If Statements for all/both buttons
    if(input$sex != 'Both'){
      county_demo <- county_demo %>% filter(Patient_Sex %in% input$sex)
    }
    
    if(input$race != 'All'){
      county_demo <- county_demo %>% filter(Race_Chr %in% input$race)
    }
    
    if(input$age != 'All'){
      county_demo <- county_demo %>% filter(age_group %in% input$age)
    }
    
    # Continue DF Altering
    county_demo <- county_demo %>% 
      group_by(county, acs_primary, nonemerg_primary) %>%
      tally %>%
      ungroup() %>%
      group_by(county) %>%
      mutate(total = sum(n)) %>%
      summarise(percentage = (n/sum(n))*100, across(everything())) %>%
      mutate( type = case_when( !acs_primary & !nonemerg_primary ~ "Appropriate Use",
                                acs_primary ~ "ACS",
                                nonemerg_primary ~ "Non emergent" )) %>%
      mutate(Condition = ifelse(type == "Appropriate Use", "Appropriate Use", "Overuse"))
    
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
      zip_demo <- zip_demo %>% filter(Patient_Sex %in% input$sex)
    }
    
    if(input$race != 'All'){
      zip_demo <- zip_demo %>% filter(Race_Chr %in% input$race)
    }
    
    if(input$age != 'All'){
      zip_demo <- zip_demo %>% filter(age_group %in% input$age)
    }
    
    # Continue DF Altering
    zip_demo <- zip_demo %>% 
      group_by(Patient_Zip, acs_primary, nonemerg_primary) %>%
      tally %>%
      ungroup() %>%
      group_by(Patient_Zip) %>%
      mutate(total = sum(n)) %>%
      summarise(percentage = (n/sum(n))*100, across(everything())) %>%
      mutate( type = case_when( !acs_primary & !nonemerg_primary ~ "Appropriate Use",
                                acs_primary ~ "ACS",
                                nonemerg_primary ~ "Non emergent" )) %>%
      mutate(Condition = ifelse(type == "Appropriate Use", "Appropriate Use", "Overuse"))
    
    # Make reactive DF
    rv$zip_demo <- zip_demo
    
  })

    observe({ #Insurance DF
      
      #Make age_group column from factor to character
      scp$age_group <- as.character(scp$age_group)
      
      insurance_demo <- scp %>%
        filter(insurance %in% input$insurance) 
      
      # If Statements for all/both buttons
      if(input$sex != 'Both'){
        insurance_demo <- insurance_demo %>% filter(Patient_Sex %in% input$sex)
      }
      
      if(input$race != 'All'){
        insurance_demo <- insurance_demo %>% filter(Race_Chr %in% input$race)
      }
      
      if(input$age != 'All'){
        insurance_demo <- insurance_demo %>% filter(age_group %in% input$age)
      }
      
      # Continue DF Altering
      insurance_demo <- insurance_demo %>% 
        filter(insurance %in% input$insurance) %>% 
        group_by(insurance, acs_primary, nonemerg_primary) %>%
        tally %>%
        ungroup() %>%
        group_by(insurance) %>%
        mutate(total = sum(n)) %>%
        summarise(percentage = (n/sum(n))*100, across(everything())) %>%
        mutate( type = case_when( !acs_primary & !nonemerg_primary ~ "Appropriate Use",
                                  acs_primary ~ "ACS",
                                  nonemerg_primary ~ "Non emergent" )) %>%
        mutate(Condition = ifelse(type == "Appropriate Use", "Appropriate Use", "Overuse"))
      
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
      filter(type != "Other") %>% 
      filter(percentage >= 0.02)
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
      filter(type != "Other") %>%
      filter(percentage >= 0.02)
  })
  
  observe({
    rv$ins <- scp %>% 
      filter(insurance %in% input$ins) %>%
      group_by(insurance,
               acs_primary,
               nonemerg_primary,
               dental_primary,
               mental_primary,
               subabuse_primary) %>%
      tally %>%
      ungroup() %>%
      group_by(insurance) %>%
      summarise(percentage = (n/sum(n))*100, across(everything())) %>%
      mutate(type = case_when(!dental_primary & !acs_primary & !subabuse_primary & !mental_primary & !nonemerg_primary ~ "Other",
                              dental_primary ~ "Dental",
                              acs_primary ~ "ACS",
                              subabuse_primary ~ "Substance Abuse",
                              mental_primary ~ "Mental Health",
                              nonemerg_primary ~ "Non emergent")) %>%
      filter(type != "Other") %>% 
      filter(percentage >= 0.02)
  })
  
 ##ER Conditions Observe
  # This makes zip code options dependent on county selected 
  observeEvent(input$filter_by, {
    if('County' %in% input$filter_by){
      shinyjs::disable(id = 'zip2') 
      shinyjs::enable(id = 'county2')
    }
    if('ZIP code' %in% input$filter_by){
      shinyjs::disable(id = 'county2')
      shinyjs::enable(id = 'zip2')
    }
  })

  ## Create reactive values
  # ^ Let us control which parts of your app update when, which prevents unnecessary computation that can slow down your app
  
  rv <- reactiveValues() 
  observe({
    if('Both' %in% input$sex2)
    {if('All' %in% input$insurance2){
      rv$countyicd <- scp %>% 
        filter(county %in% input$county2) %>% 
        group_by(Diag1 ) %>% 
        tally()%>% 
        ungroup() %>% 
        mutate(total=sum(n)) %>% 
        group_by(Diag1) %>% 
        summarise(perc=n/total*100) %>% 
        arrange(desc(perc)) %>% 
        head(5)}
      
      else{rv$countyicd <- scp %>% 
        filter(county %in% input$county2,
               insurance %in% input$insurance2) %>% 
        group_by(Diag1 ) %>% 
        tally()%>% 
        ungroup() %>% 
        mutate(total=sum(n)) %>% 
        group_by(Diag1) %>% 
        summarise(perc=n/total*100) %>% 
        arrange(desc(perc)) %>% 
        head(5)}}
    else{
      if('All' %in% input$insurance2){
        rv$countyicd <- scp %>% 
          filter(county %in% input$county2,
                 Patient_Sex %in% input$sex2) %>% 
          group_by(Diag1 ) %>% 
          tally()%>% 
          ungroup() %>% 
          mutate(total=sum(n)) %>% 
          group_by(Diag1) %>% 
          summarise(perc=n/total*100) %>% 
          arrange(desc(perc)) %>% 
          head(5)}
      
      else{rv$countyicd <- scp %>% 
        filter(county %in% input$county2,
               insurance %in% input$insurance2,
               Patient_Sex %in% input$sex2) %>% 
        group_by(Diag1 ) %>% 
        tally()%>% 
        ungroup() %>% 
        mutate(total=sum(n)) %>% 
        group_by(Diag1) %>% 
        summarise(perc=n/total*100) %>% 
        arrange(desc(perc)) %>% 
        head(5)}
    }
    
    
  })
  
  observe({
    if('Both' %in% input$sex2){
      if('All' %in% input$insurance2){
        rv$zipicd <- scp %>% 
          filter(Patient_Zip %in% input$zip2) %>% 
          group_by(Diag1 ) %>% 
          tally()%>% 
          ungroup() %>% 
          mutate(total=sum(n)) %>% 
          group_by(Diag1) %>% 
          summarise(perc=n/total*100) %>% 
          arrange(desc(perc)) %>% 
          head(5)}
      
      else{rv$zipicd <- scp %>% 
        filter(Patient_Zip %in% input$zip2,
               insurance %in% input$insurance2) %>% 
        group_by(Diag1 ) %>% 
        tally()%>% 
        ungroup() %>% 
        mutate(total=sum(n)) %>% 
        group_by(Diag1) %>% 
        summarise(perc=n/total*100) %>% 
        arrange(desc(perc)) %>% 
        head(5)}
      
    }
    else{
      if('All' %in% input$insurance2){
        rv$zipicd <- scp %>% 
          filter(Patient_Zip %in% input$zip2,
                 Patient_Sex %in% input$sex2) %>% 
          group_by(Diag1 ) %>% 
          tally()%>% 
          ungroup() %>% 
          mutate(total=sum(n)) %>% 
          group_by(Diag1) %>% 
          summarise(perc=n/total*100) %>% 
          arrange(desc(perc)) %>% 
          head(5)}
      
      else{rv$zipicd <- scp %>% 
        filter(Patient_Zip %in% input$zip2,
               insurance %in% input$insurance2,
               Patient_Sex %in% input$sex2) %>% 
        group_by(Diag1 ) %>% 
        tally()%>% 
        ungroup() %>% 
        mutate(total=sum(n)) %>% 
        group_by(Diag1) %>% 
        summarise(perc=n/total*100) %>% 
        arrange(desc(perc)) %>% 
        head(5)} 
    }})
  #OUTPUTS ------
##############################################################################
  
  #FINDINGS GRAPHS ------
  ##########################################
  #Main Findings Graph
  
  output$er_overuse <- renderPlot({
    
    ggplot(data = rv$acs_nonemerg_other, 
           aes(x = Condition,
               y = percentage/100,
               fill = type)) +
      geom_col()+
      labs(title = "Comparison of Primary Diagnosis Conditions",
           subtitle = "In SCP Counties",
           y = "% of ER Visits",
           x = '') +
      scale_fill_manual(values=c('#41b6c4', 
                                 '#fdcc8a',
                                 '#253494'),
                        name = "Type of Condition") +
      scale_y_continuous(labels = scales::percent) + 
      labs(title = "Comparison of Primary Diagnosis Conditions",
           y = "% of ER Visits") + 
      geom_text(aes(label = scales::percent(percentage/100)),
                position = position_stack(vjust = 1.1)) + 
      theme_light(base_size = 18) 
  })
  
  output$county_graph <- renderPlot({
    
    #Filter out "other" conditions
    tn_diags <- tn_diags %>% filter(county != "Other", Condition != "Other")
    
    tn_diags <- tn_diags %>%
      mutate(order = reorder(county, -precent))
    #Graph
    ggplot(data = tn_diags, aes(x = county, y = precent/100, fill = type)) +
      geom_col(position = "dodge") +
      scale_y_continuous(labels = scales::percent) +
      labs(title = "Comparison of Primary Diagnosis Conditions",
           subtitle = "SCP Counties vs Williamson County",
           x = "County",
           y = "% of ER Visits") + 
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
      filter(insurance %in% c("TennCare", "MediCare", "Self Pay", "Commercial")) %>% 
      group_by(insurance, acs_primary, nonemerg_primary) %>% 
      tally %>% 
      ungroup() %>% 
      group_by(insurance) %>% 
      mutate(total = sum(n)) %>%
      summarise(percentage = n/total*100, across(everything())) %>% 
      mutate(type = case_when(!acs_primary & !nonemerg_primary ~ "Appropriate Use", 
                              acs_primary ~ "ACS",
                              nonemerg_primary ~ "Non emergent")) %>% 
      mutate(type2 = ifelse(type == "Appropriate Use", "Appropriate Use", "Overuse")) %>% 
      filter(type2 != "Appropriate Use")
    

    
    # Overuse by insurance 
    ggplot(data = insurance_overuse, aes(x = reorder(insurance, -percentage), 
                                         y = percentage/100, fill = type)) +
      geom_col(position = 'dodge') +
      scale_fill_manual(name = 'Type of Condition',
                        values=c('#41b6c4', 
                                 '#253494')) +
      scale_y_continuous(labels = scales::percent) + 
      labs(x = 'Insurance', y = '% of ER Visits',
           title = 'ER Overuse by Insurance Type',
           subtitle = "In SCP Counties",) +
      theme_light(base_size = 17) +
      geom_text(aes(label = scales::percent(x = percentage/100, accuracy = 0.01)),
                position = position_dodge(width = 0.9),
                vjust = -.1)
  })
  
  output$scp_conditions <- renderPlot({
    # code for plot
    scp_conditions <- scp %>%
      group_by(acs_primary,
               nonemerg_primary,
               dental_primary,
               mental_primary,
               subabuse_primary) %>%
      tally %>%
      ungroup() %>%
      summarise(percentage = (n/sum(n))*100, across(everything())) %>%
      mutate(type = case_when(!dental_primary & !acs_primary & !subabuse_primary & !mental_primary & !nonemerg_primary ~ "Other",
                              dental_primary ~ "Dental",
                              acs_primary ~ "ACS",
                              subabuse_primary ~ "Substance Abuse",
                              mental_primary ~ "Mental Health",
                              nonemerg_primary ~ "Non emergent")) %>%
      filter(type != "Other") %>% 
      arrange(desc(percentage))
    
    scp_conditions <- scp_conditions[-6,]
    
    scp_conditions <- scp_conditions %>%
      mutate(type = reorder(type, -percentage))
    
    #plot
    ggplot(data = scp_conditions, 
           aes(x = type,
               y = percentage/100,
               fill = type)) +
      geom_col() +
      scale_y_continuous(labels = scales::percent) + 
      theme_light(base_size = 18) +
      labs(title = "ER Visits by Condition Type",
           subtitle = "In SCP Counties",
           x = "Type of Condition",
           y = "% of ER Visits") +
      scale_fill_manual(values=c('#fdcc8a',
                                 '#a1dab4',
                                 '#41b6c4',
                                 '#2c7fb8',
                                 '#253494'),
                        name = "Type of Condition") +
      theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank()) +
      geom_text(aes(label = scales::percent(percentage/100)),
                position = position_dodge(width = 0.9),
                vjust = -.1)
    
  })
  
  output$admit_hr_graph <- renderPlot({
    #Admit Hour Graph Code
    scp$Admit_Hr <- as.numeric(scp$Admit_Hr)
    overuse_hour <- scp %>% 
      group_by(Admit_Hr, acs_primary, nonemerg_primary) %>% 
      drop_na(Admit_Hr) %>% 
      mutate( type = case_when( !acs_primary & !nonemerg_primary ~ "Appropriate Use",
                                acs_primary ~ "ACS", 
                                nonemerg_primary ~ "Non emergent" )) %>% 
      mutate(Condition = ifelse(type == "Appropriate Use", "Appropriate Use", "Overuse")) %>% 
      group_by(Admit_Hr, Condition) %>% tally()
    
    #Admit Hour Graph
    ggplot(data = overuse_hour) +
      geom_point(mapping = aes(x = Admit_Hr, y = n, color = Condition)) + 
      geom_line(aes(x = Admit_Hr, y = n, color = Condition)) +
      theme_light(base_size = 18) +
      scale_color_manual(values=c('#fdcc8a',
                                  '#253494'),
                        name = "Type of Condition") +
      labs( x = "Patient Admit Hour",
            y = "Number of Patient Visits",
            title = "ER Visits by Admit Hour",
            subtitle = "In SCP Counties",)
    
  })
  
  
  ##############################################################################
  # EXPLORE ER TAB GRAPHS ------
  
  # MAPS TAB
  #################################
output$top10_hospitals <- renderLeaflet({
    
  # Code for setting up map
bigpic <- scp %>% 
      group_by(Patient_Zip, acs_primary, nonemerg_primary) %>% 
      tally %>% 
      ungroup() %>% 
      group_by(Patient_Zip) %>% 
      mutate(total = sum(n)) %>% 
      summarise(percentage = n/total*100, across(everything())) %>%
      mutate(type = case_when( !acs_primary & !nonemerg_primary ~ "Appropriate Use",
                               acs_primary ~ "ACS", 
                               nonemerg_primary ~ "Non emergent" )) %>% 
      mutate(status = ifelse(type == "Appropriate Use", "Appropriate Use", 'ER Overuse')) %>% 
      filter(type != "Appropriate Use") %>% 
      mutate(overuse_perc = sum(n)/total*100) %>% 
      group_by(Patient_Zip, overuse_perc) %>% 
      tally
# joining variable above with zipcodes file    
bigpic_zip <- inner_join(bigpic, zipcodes, by = "Patient_Zip")
# read in google sheet with town names and patient zips
towns <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1200giV6UYXfolA1UkUJ--6MUG5FUavJUGGolS_lRxks/edit?usp=sharing")
# join google sheet above with 'bigpic_zip'
bigpic_zip <- inner_join(bigpic_zip, towns, by = 'Patient_Zip')
    
# Google sheets for hospital, urgent care, and doctor's offices lat/long data
bigpic_hosp <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1JzmcNpV1bYoW3N6sg7bYVgL33LERLbS__CtSYB1_bX4/edit?usp=sharing")
    
doctor <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1ZqRk8NK4qp43bA30Q0VyBEWVX9Z_Zd_bgk6_Mt_dDW8/edit?usp=sharing")
    
urgent_care <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1_tTNbY0YQKAVF51rQcULq0qInGoMIz9thJieHbbfXfs/edit?usp=sharing")
# Color palette for shading the zip codes by severity of ER overuse    
palette <- colorNumeric(palette = c('#0571b0','#92c5de',  '#f7f7f7', '#f4a582', '#ca0020'),
                            domain = bigpic_zip$overuse_perc)
# Map
leaflet() %>% 
      addTiles() %>% 
      addMarkers(lat = bigpic_hosp$latitude,
                 lng = bigpic_hosp$longitude,
                 popup = bigpic_hosp$name) %>% 
      addPolygons(data = bigpic_zip$geometry,
                  color = 'white',
                  fillColor = palette(bigpic_zip$overuse_perc),
                  weight = 0.5,
                  smoothFactor = 0.25,
                  opacity = 0.25,
                  fillOpacity = .75,
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 1.5,
                                                      opacity = 1.0),
                  label = paste0(bigpic_zip$town,", ",
                                 unique(bigpic_zip$Patient_Zip),
                                 ' | ER overuse = ',
                                 round(bigpic_zip$overuse_perc, 1),'%')) %>%
      addCircleMarkers(lat = doctor$lat,
                       lng = doctor$lng,
                       radius =2,
                       color = "green",
                       opacity = 0.75) %>%
      addCircleMarkers(lat = urgent_care$lat,
                       lng = urgent_care$lng,
                       radius = 2,
                       color = "red",
                       opacity = 0.75) %>%
      addLegend("bottomright",
                pal = palette,
                values = bigpic_zip$overuse_perc,
                opacity = 0.75,
                title = "% Overuse")
  })
  
  output$zipMap <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>%
      addPolygons(data = hospitals[hospitals$Patient_Zip %in% input$zipcode,]$geometry,
                  color = '#ca0020',
                  weight = 0.5,
                  smoothFactor = 0.25,
                  opacity = 0.25,
                  fillOpacity = 0.25,
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 1.5,
                                                      opacity = 1.0),
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
      theme_light(base_size = 18) +
      scale_fill_manual(values=c('#fdcc8a',
                                 '#a1dab4',
                                 '#41b6c4'),
                        name = "Type of Condition") +
      scale_y_continuous(labels = scales::percent) + 
      labs(y = "% of ER Visits",
           x = ' ') +
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
      labs(y = "% of ER Visits",
           x = '') +
      theme_light(base_size = 18) +
      scale_fill_manual(values=c('#fdcc8a',
                                 '#a1dab4',
                                 '#41b6c4'),
                        name = "Type of Condition") +
      scale_y_continuous(labels = scales::percent) + 
      geom_text(aes(label = scales::percent(percentage/100)),
                position = position_dodge(width = 0.9), 
                vjust = -.5)
  })
  
  #INSURANCE PLOT
  output$insurance_plot <- renderPlot({
    ggplot(data = rv$insurance_demo, aes(x = reorder(type, -percentage), 
                                         y = percentage/100, 
                                         fill = type)) + 
      geom_col(position = "dodge") +
      theme_light(base_size = 18) +
      scale_fill_manual(values=c('#fdcc8a',
                                 '#a1dab4',
                                 '#41b6c4'),
                        name = "Type of Condition") +
      scale_y_continuous(labels = scales::percent) + 
      labs(y = "% of ER Visits",
           x = "") +
      geom_text(aes(label = scales::percent(x = percentage/100, accuracy = 0.01)),
                position = position_dodge(width = 0.9),
                vjust = -.1) 
  })

  # PRIMARY CARE SERVICES TAB
  ################################
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
                values = (combine %>% pull(!!input$cond) %>% range), #creates range of values within column selected
                title = "% of ER Visits",
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
           y = "% of ER Visits") +
      scale_fill_manual(values=c('#fdcc8a',
                                 '#a1dab4',
                                 '#41b6c4',
                                 '#2c7fb8',
                                 '#253494'),
                        name = "Type of Condition") +
      theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank()) +
      geom_text(aes(label = scales::percent(x = percentage/100, accuracy = 0.01)),
                position = position_dodge(width = 0.9),
                vjust = -.1) 
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
           y = "% of ER Visits") +
      scale_fill_manual(values=c('#fdcc8a',
                                 '#a1dab4',
                                 '#41b6c4',
                                 '#2c7fb8',
                                 '#253494'),
                        name = "Type of Condition") +
      theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank()) +
      geom_text(aes(label = scales::percent(x = percentage/100, accuracy = 0.01)),
                position = position_dodge(width = 0.9),
                vjust = -.1) 
  })
  
# Insurance plot
output$all_cond_insurance <- renderPlot({

    ggplot(data = rv$ins, 
           aes(x = reorder(type, -percentage),
               y = percentage/100,
               fill = type)) +
      geom_col() +
      scale_y_continuous(labels = scales::percent) + 
      theme_light(base_size = 18)+
      labs(x = " ",
           y = "% of ER Visits") +
      scale_fill_manual(values=c('#fdcc8a',
                                 '#a1dab4',
                                 '#41b6c4',
                                 '#2c7fb8',
                                 '#253494'),
                        name = "Type of Condition") +
      theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank()) +
    geom_text(aes(label = scales::percent(x = percentage/100, accuracy = 0.01)),
              position = position_dodge(width = 0.9),
              vjust = -.1) 
})

# ER Conditions Tab
#################################
observe(output$conditions_plot <- if(input$filter_by == "County"){
  renderPlot({
    
    ggplot(data = rv$countyicd, aes(x= Diag1,
                                    y= perc, fill= Diag1)) +
      geom_col()+
      # This puts percent sign on y-axis
      labs(x= 'Diagnostic Codes', y= '% of ER Visits', fill= 'Diagnosis')+
      # Gives a minimalistic theme to the graph and changes the text size on the graph
      theme_light(base_size = 18)+
      scale_fill_manual(values= c('#fdcc8a',
                                  '#a1dab4',
                                  '#41b6c4',
                                  '#2c7fb8',
                                  '#253494'))
    
  })}
  # If the filter_by input is not county then plot this... and we only have two varibles so if it is vs. if not
  else{renderPlot({
    ggplot(data= rv$zipicd, aes(x= Diag1,
                                y= perc, fill= Diag1))+
      geom_col()+
      labs(x= 'Diagnostic Codes', y= 'Percentage', fill= 'Diagnosis')+
      # Gives a minimalistic theme to the graph and changes the text size on the graph
      theme_light(base_size = 18)+
      scale_fill_manual(values= c('#fdcc8a',
                                  '#a1dab4',
                                  '#41b6c4',
                                  '#2c7fb8',
                                  '#253494'))
  })})

## Static Graph with top 10 ICD 10 codes at the top of the tab
output$icdscp_plot <- renderPlot({
  icdscp <- scp %>% 
    filter(acs_primary == 'TRUE' | nonemerg_primary== 'TRUE') %>% 
    group_by(Diag1, acs_primary, nonemerg_primary) %>% 
    tally() %>%
    ungroup() %>% 
    mutate(total = sum(n)) %>%
    group_by(Diag1) %>% 
    summarise(perc= n/total*100, across(everything())) %>% 
    arrange(desc(perc)) %>% 
    head(10)
  
  
  # Arrange data set greatest to least
  icdscp <- icdscp %>% 
    mutate( Diag1 = reorder(Diag1, -perc))
  
  # Bar graph for top 10 ICD 10 codes for all of SCP 
  ggplot(data= icdscp) +
    labs(title= ' Top 10 Diagnoses in Instances of ER Overuse ',
         subtitle= '   In SCP Counties',
         x= 'Diagnostic Code', y= 'Percentage', fill= 'Diagnosis')+
    geom_col(aes(x= Diag1, y= perc, fill= Diag1))+
    theme_light(base_size = 18)+
    # Allows legend labels to be renamed
    scale_fill_manual(values= c('#fdcc8a',
                                '#feb24c',
                                '#fd8d3c',
                                '#c7e9b4',
                                '#7fcdbb',
                                '#41b6c4',
                                '#1d91c0',
                                '#225ea8',
                                '#253494',
                                '#081d58'),
                      labels=c('Urinary Tract Infection', 'Acute Upper Respiratory Infection',
                               'Obstructive Pulmonary Disease', 'Acute Pharyngitis',
                               'Influenza', 'Gastroenteritis', 'Strep Throat', 'Periapical Abscess',
                               'Hypertension', 'Acute Bronchitis'))
})

}
# Run the application 
shinyApp(ui = ui, server = server)
