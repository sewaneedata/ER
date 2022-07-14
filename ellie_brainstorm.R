# Preliminary investigation of shrunken data set
#########################################
library(dplyr)
library(readr)
library(tidyverse)
library(ggplot2)
library(gsheet)

# scp_data set
scp <- readr::read_csv("scp_data2")

# See "shared_help_code.R" for vectors and necessary code to set up data mining.

########################################
# Making a Pie chart
# 1. ACS conditions
  # table
perc_acsc <- scp %>% 
  group_by(acs_primary) %>%
  tally %>% 
  mutate(total = sum(n)) %>%
  group_by(acs_primary, n, total) %>% 
  summarise(perc = n/total*100)

  # pie chart
x <- c(78.1, 21.9)

pie(x,
    labels = paste0(x, "%"), 
    col = c('light blue', 'red'),
    radius = .9,
    main = "Primary Diagnoses at the ER")
legend("bottomleft", legend = c('Other', 'ACSC'),
       fill =  c("light blue", "red"), title = "Condition")


#####################
# making "big picture" map
top_ten <- scp %>% 
  group_by(JARID) %>% 
  tally %>% 
  arrange(desc(n)) %>% 
  head(10)

bigpic <- scp %>% 
  group_by(Patient_Zip, acs_primary, nonemerg_primary) %>% 
  tally %>% 
  ungroup() %>% 
  group_by(Patient_Zip) %>% 
  mutate(total = sum(n)) %>% 
  summarise(percentage = n/total*100, across(everything())) %>%
  mutate(type = case_when( !acs_primary & !nonemerg_primary ~ "Unpreventable",
                            acs_primary ~ "ACS", 
                            nonemerg_primary ~ "Non emergent" )) %>% 
  mutate(status = ifelse(type == 'Unpreventable', 'Unpreventable', 'ER Overuse')) %>% 
  filter(type != 'Unpreventable') %>% 
  mutate(overuse_perc = sum(n)/total*100) %>% 
  group_by(Patient_Zip, overuse_perc) %>% 
  tally
  
bigpic_zip <- inner_join(bigpic, zipcodes, by = "Patient_Zip")

bigpic_hosp <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1JzmcNpV1bYoW3N6sg7bYVgL33LERLbS__CtSYB1_bX4/edit?usp=sharing")

doctor <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1ZqRk8NK4qp43bA30Q0VyBEWVX9Z_Zd_bgk6_Mt_dDW8/edit?usp=sharing")
  
urgent_care <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1_tTNbY0YQKAVF51rQcULq0qInGoMIz9thJieHbbfXfs/edit?usp=sharing")

palette <- colorNumeric(palette = c('#0571b0','#92c5de',  '#f7f7f7', '#f4a582', '#ca0020'),
                        domain = bigpic_zip$overuse_perc)


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
              label = paste0("Zip: ", unique(bigpic_zip$Patient_Zip), 
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



################################

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
          nonemerg_primary ~ "Non emergent")) %>% 
  mutate(type2 = ifelse(type == "Appropriate Use", "Appropriate Use", "Overuse")) %>% 
  filter(type2 != "Appropriate Use")

# Dashboard graph                    
ggplot(data = insurance_overuse, aes(x = insurance, y = percentage/100, fill = type)) +
  geom_col(position = 'dodge') +
  scale_fill_manual(name = 'Type of Condition',
                    values=c('#41b6c4',
                             '#253494')) +
  scale_y_continuous(labels = scales::percent) + 
  labs(x = ' ', y = 'Percentage of Visits',
       title = 'Severity of Overuse by Insurance Type') +
  theme_light(base_size = 18)

# poster graph
ggplot(data = insurance_overuse, aes(x = insurance, y = percentage/100, fill = type)) +
  geom_col(position = 'dodge') +
  scale_fill_manual(name = 'Type of Condition',
                    values=c('#7B3294',
                             '#C2A5CF')) +
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Insurance', y = 'Percentage of Visits',
       title = 'ER Overuse by Insurance Type') +
  theme_light(base_size = 12) +
  geom_text(aes(label = scales::percent(percentage/100)),
            position = position_dodge(width = 0.9),
            vjust = -.1)


# graph for overview tab of conditions in SCP

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

# dashboard, conditions
  ggplot(data = scp_conditions, 
         aes(x = reorder(type, -percentage),
             y = percentage/100,
             fill = type)) +
    geom_col() +
    scale_y_continuous(labels = scales::percent) + 
    theme_light(base_size = 10) +
    labs(title = "ER Visits by Condition Type",
         subtitle = "On the South Cumberland Plateau",
      x = "Type of Condition",
         y = "Percentage of Patient Visits") +
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

  
scp_conditions <- scp_conditions %>% 
  mutate(type = reorder(type, -percentage))

# poster, conditions
  ggplot(data = scp_conditions, 
         aes(x = type,
             y = percentage/100,
             fill = type)) +
    geom_col() +
    scale_y_continuous(labels = scales::percent) + 
    theme_light(base_size = 12) +
    labs(title = "ER Visits by Condition Type",
         x = "Type of Condition",
         y = "Percentage of Patient Visits") +
    scale_fill_manual(values=c('#7B3294',
                               '#C2A5CF',
                               '#008837',
                               '#A6DBA0',
                               '#636363'),
                      name = "Type of Condition") +
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank()) +
    geom_text(aes(label = scales::percent(percentage/100)),
              position = position_dodge(width = 0.9),
              vjust = -.1)
  
# poster, insurance
  ggplot(data = insurance_overuse, aes(x = insurance, y = percentage/100, fill = type)) +
    geom_col(position = 'dodge') +
    scale_fill_manual(name = 'Type of Condition',
                      values=c('#7B3294',
                               '#C2A5CF')) +
    scale_y_continuous(labels = scales::percent) + 
    labs(x = 'Insurance', y = 'Percentage of Visits',
         title = 'ER Overuse by Insurance Type') +
    theme_light(base_size = 12) +
    geom_text(aes(label = scales::percent(x = percentage/100, accuracy = 0.01)),
              position = position_dodge(width = 0.9),
              vjust = -.1) 
  
  

