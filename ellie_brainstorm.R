# Preliminary investigation of shrunken data set
#########################################
library(dplyr)
library(readr)
library(tidyverse)
library(ggplot2)
library(gsheet)

# scp_data set
scp <- readr::read_csv("Dropbox/DATALAB/ER_Project/scp_data")

# See "shared_help_code.R" for vectors and necessary code to set up data mining.

######################################################
# GRAPH WORK/MESSING AROUND
######################################################
# Making a graph that expresses demographics (sex and age) of who used the ER:
  # First, make a new variable that can later be plotted

age_sex_ER <- scp %>%
  filter(ER_Record_Flag == 'Y') %>% # filter for ER visits
  filter(Age < 999) %>% # remove 2 random ppl with 999 as age
  drop_na(Patient_Sex) %>% #drop the N/As from Patient_Sex
  mutate(Age_Group = cut(Age, 
                         breaks = 10, 
                         labels = c('0-9', '10-19', '20-29', '30-39', '40-49',
                                    '50-59', '60-69', '70-79', '80-89', '90-99'))) %>%
  # use cut function to make a new column of "age groups" (by decade).
  group_by(Age_Group, Patient_Sex) %>% 
  tally %>%
  ungroup() %>% 
  mutate(total = sum(n)) %>% 
  group_by(Age_Group, Patient_Sex) %>% 
  summarise(perc = n/total*100)

  # Then, plot to show percentage on y, age groups on x, and facet wrap by Sex. 
ggplot(age_sex_ER, aes(x = Age_Group, y = perc, fill = Age_Group)) +
  geom_histogram(stat = 'identity')+
  theme(legend.position = 'none')+
  facet_wrap(~Patient_Sex, ncol = TRUE)+
  labs(title = 'Demographics of Dataset',
       subtitle = 'By Age Group & Sex',
       x = 'Age Group',
       y = 'Percentage of ER Visits')

# Investigating percentages by race
df %>% 
  group_by(Race, Patient_Zip) %>% 
  tally %>%
  mutate(total = sum(n)) %>% 
  group_by(Race, Patient_Zip, n) %>% 
  summarise(percentage = n/total*100)

df %>% 
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
df %>%
  group_by(Patient_Zip) %>% 
  tally %>% 
  mutate(total = sum(n)) %>% 
  group_by(Patient_Zip, n) %>% 
  summarise(perc = n/total*100)

# Graph of percentages of patients by race and zip code
races_vec <- c("White", "Black", "Native American") # create vector that names races
df<- df %>% 
  mutate(Race_Chr = ifelse(Race == 9, "Unkown", races_vec[Race]))

perc_by_race_zip <- df %>% 
  group_by(Patient_Zip, Race_Chr) %>% 
  tally %>%
  mutate(total = sum(n)) %>% 
  group_by(Patient_Zip, Race_Chr, n) %>% 
  summarise(percentage = n/total*100) 

ggplot(data = perc_by_race_zip, aes(x = Race_Chr, y = percentage, fill = Race_Chr)) +
  geom_bar(stat = 'identity')+
  facet_wrap(~Patient_Zip)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = 'bottom')

#  

#####################################################

# How many visits had an 'ER_Record_Flag'?
View(scp %>% 
  group_by(ER_Record_Flag) %>% 
  tally)

# This variable shows only visits that have a "Y" (yes) response under the 
# "ER_Record_Flag" column, indicating which visits from the "scp" data were
# to the ER.
scp %>%
  filter(ER_Record_Flag == "Y")

scp_long %>%
  filter(ER_Record_Flag == "Y")
# NOTE: if we determine that ER_Record_Flag means ER visit, as we suspect, then
  # I will add the above code to line 19 of "shared_help_code.R" to filter the data 
  # down to only ER visits. 

# Looking at how many visits to the ER had a primary diagnosis of an acsc.
# Also calculating the percentage out of total ER visits
ER_acs_prim <- scp %>% 
  filter(ER_Record_Flag == "Y") %>%
  mutate(primary = grepl(acs, Diag1)) %>% 
  group_by(visit) %>%
  summarize(code_sum = sum(primary),
            acsprim_YN = ifelse(code_sum > 0, "Yes", "No")) %>% 
  group_by(acsprim_YN) %>% 
  tally %>%
  mutate(total = sum(n)) %>% 
  group_by(acsprim_YN, n) %>% 
  summarise(percentage = n/total*100)
  
# How many visits to the ER had any acsc diagnosis & calculating percentage out of
# total ER visits.
ER_acs_diag <- scp_long %>% 
  filter(ER_Record_Flag == 'Y') %>% 
  mutate(`acs?` = grepl(acs, value)) %>% 
  group_by(visit) %>%
  summarize(code_sum = sum(`acs?`),
            acs_YN = ifelse(code_sum > 0, "Yes", "No")) %>% 
  group_by(acs_YN) %>% 
  tally %>%
  mutate(total = sum(n)) %>% 
  group_by(acs_YN, n) %>% 
  summarise(percentage = n/total*100)

# plot the above two findings:

# Visits to the ER for acsc vs non-acsc PRIMARY diagnosis


# Visits to the ER for acsc vs non-acsc
ggplot(data = ER_acs_diag, aes(x = acs_YN, y = percentage, fill = acs_YN)) +
  geom_bar(stat = 'identity')+
  labs(x = 'ACSC Diagnosis',
       y = 'Percentage of ER Visits') +
  theme(legend.position = 'none')







