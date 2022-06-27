# Preliminary investigation of shrunken data set
#########################################

library(dplyr)
library(readr)
library(tidyverse)
library(ggplot2)

# Upload scp_data set
df <- readr::read_csv("Dropbox/DATALAB/ER_Project/scp_data")

# Investigating percentages by race
perc_by_race <- df %>% 
  group_by(Race) %>% 
  tally %>%
  mutate(total = sum(n)) %>% 
  group_by(Race, n) %>% 
  summarise(percentage = n/total*100)

# Sorting into zip codes
df %>%
  group_by(Patient_Zip) %>% 
  tally

# Seeing percentage of cases from each zip code
perc_zip <- df %>%
  group_by(Patient_Zip) %>% 
  tally %>% 
  mutate(total = sum(n)) %>% 
  group_by(Patient_Zip, n) %>% 
  summarise(perc = n/total*100)


# Trying to look at percentage of each race from each zip code
race_zip <- df %>%
  group_by(Patient_Zip, Race) %>% 
  tally
  
  
  mutate(total = sum(n)) %>% 
  group_by(Patient_Zip, n) %>% 
  summarise(perc = n/total*100)
  
  # Made a vector of the ACSC ICD-10-CM codes:
    # insert google sheet
  
library(gsheet)
url <- "https://docs.google.com/spreadsheets/d/14fZ-1PFInHdL8OIaLUSHhc7ulFwSf9i9O32SPitCkLU/edit?usp=sharing" 
  # create an object out of it
acs <- gsheet2tbl(url)  
  # create a vector out of the object
acs <- as.vector(unlist(acs$'ICD_10_code'))  


  