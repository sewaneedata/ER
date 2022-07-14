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

#############
# Trends in Race
# table
race <- scp %>%
  filter(Race_Chr == c("White", "Black", "Native American")) %>% 
  group_by(Race_Chr, 
           acs_primary, 
           nonemerg_primary, 
           mental_primary, 
           dental_primary, 
           subabuse_primary) %>%
  tally %>%
  ungroup()%>%
  group_by(Race_Chr) %>% 
  summarise(percentage = (n/sum(n))*100, across(everything())) %>%
  mutate( type = case_when( !acs_primary & !nonemerg_primary & !mental_primary & !dental_primary & !subabuse_primary ~ "Other",
                            dental_primary ~ "Dental",
                            acs_primary ~ "ACS",
                            subabuse_primary ~ "Substance Abuse",
                            mental_primary ~ "Mental Health",
                            nonemerg_primary ~ "Non emergent"))

ggplot(data = race, aes(x = type, y = percentage, fill = type)) +
  geom_col() +
  labs(title = "Trends") +
  theme(legend.position = 'bottom') +
  scale_fill_discrete(name = "Condition") +
  facet_wrap(~Race_Chr) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

ggplot(data = race, aes(x = condition, y = percentage, fill = type)) +
  geom_col() +
  labs(title = "Trends") +
  theme(legend.position = 'bottom') +
  scale_fill_discrete(name = "Condition") +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

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
  mutate(status = ifelse( type == 'Unpreventable', 'Unpreventable', 'ER Overuse')) %>% 
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
             lng = bigpic_hosp$longitude) %>% 
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
  
  #label = ~lapply(paste0("Zip code: ", bigpic_zip$Patient_Zip, ",",
  #               "<br/>",
  #               paste(round(get(bigpic_zip$overuse_perc), 2)), "%"), HTML)) %>% 
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

test <- scp %>%
  filter(insurance == c("TennCare", "MediCare", "Self Paid", "Commercial")) %>% 
  group_by(insurance, acs_primary, nonemerg_primary) %>% 
  tally %>% 
  ungroup() %>% 
  group_by(insurance) %>% 
  mutate(total = sum(n)) %>%
  summarise(percentage = n/total*100, across(everything())) %>% 
  mutate(type = case_when(!acs_primary & !nonemerg_primary ~ "Appropriate Use", 
          acs_primary ~ "ACS Conditions",
          nonemerg_primary ~ "Nonemergent Conditions")) %>% 
  mutate(type2 = ifelse(type == "Appropriate Use", "Appropriate Use", "Overuse")) %>% 
  filter(type2 != "Appropriate Use")
# Dashboard graph                    
ggplot(data = test, aes(x = insurance, y = percentage/100, fill = type)) +
  geom_col(position = 'dodge') +
  scale_fill_manual(name = ' ',
                    values=c('#41b6c4',
                             '#253494')) +
  scale_y_continuous(labels = scales::percent) + 
  labs(x = ' ', y = 'Percentage of Visits',
       title = 'Severity of Overuse by Insurance Type') +
  theme_light(base_size = 18) +
  theme(legend.position = 'bottom')

# poster graph
ggplot(data = test, aes(x = insurance, y = percentage/100, fill = type)) +
  geom_col(position = 'dodge') +
  scale_fill_manual(name = ' ',
                    values=c('#af89fa',
                             '#7fbf7b')) +
  scale_y_continuous(labels = scales::percent) + 
  labs(x = ' ', y = 'Percentage of Visits',
       title = 'Severity of Overuse by Insurance Type') +
  theme_light(base_size = 12) +
  theme(legend.position = 'bottom')

