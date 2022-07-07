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
###############
# Code for Map

#libraries
library(sf)
library(leaflet)
library(raster)


zipcodes <- st_read("tl_2019_us_zcta510.shp")

# filter down to only include SCP zipcodes

zipcodes <- zipcodes %>% 
  filter(ZCTA5CE10 %in% c("37301",
                        "37305", 
                        "37313", 
                        "37339", 
                        "37356", 
                        "37365", 
                        "37366",
                        "37374",
                        "37375",
                        "37383",
                        "37387",
                        "37397"))

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


