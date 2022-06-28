# Preliminary investigation of shrunken data set
#########################################

library(dplyr)
library(readr)
library(tidyverse)
library(ggplot2)
library(gsheet)

# Upload scp_data set
df <- readr::read_csv("Dropbox/DATALAB/ER_Project/scp_data")

# Made a vector of the ACSC ICD-10-CM codes:
  # insert google sheet
url <- "https://docs.google.com/spreadsheets/d/14fZ-1PFInHdL8OIaLUSHhc7ulFwSf9i9O32SPitCkLU/edit?usp=sharing" 

  # create an object out of it
acs <- gsheet2tbl(url)  

  # create a vector out of the object
acs <- acs$'ICD_10_code'

acs

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

# Graph of percentages of patients by race and zipcode
races_vec <- c("White", "Black", "Native American")
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

# make a graph of percentage of patients by age and sex:
  # make age groups by decade and make new object
df_age <-  df %>%
  filter(Age < 999, Patient_Sex == c('F','M')) %>% 
  mutate(Age_Group = cut(Age, 
                         breaks = 10, 
                         labels = c('0-9', '10-19', '20-29', '30-39', '40-49',
                                    '50-59', '60-69', '70-79', '80-89', '90-99'))) %>%
           group_by(Age_Group, Patient_Sex) %>% 
           tally %>%
           ungroup() %>% 
  mutate(total = sum(n)) %>% 
           group_by(Age_Group, Patient_Sex) %>% 
           summarise(perc = n/total*100)

  #plot it 
ggplot(df_age, aes(x = Age_Group, y = perc, fill = Age_Group)) +
  geom_histogram(stat = 'identity')+
  theme(legend.position = 'none')+
  facet_wrap(~Patient_Sex, ncol = TRUE)+
  labs(title = 'Demographic of Dataset',
       subtitle = 'By Age Group & Sex',
       x = 'Age Group',
       y = 'Percentage of Dataset')

################################################
# you can use grepl() to search for anything in the data that contains the value in " "
  # this does not work for a labeled vector. we will potentially 
View(scp %>%
  filter(grepl("M17", Diag1)))

######################################## 
# vector for substance abuse ICD10 codes --- 
sub_abuse <- c()

for(code in 11:19){
  new_value <- paste('F', as.character(code), sep = "")
  sub_abuse <- c(sub_abuse, new_value)
}
sub_abuse
# vector for mental health ICD10 codes
mental <- c()

for(code in 01:99){
  new_value <- paste('F', as.character(code), sep = "")
  mental_health <- c(mental_health, new_value)
}

######################################
# SOLUTION FOR COLUMN READING PROBLEM
  # vector for non-emergent ICD10 codes

# 1. Read google sheet of ICD-10 codes into R
non_emerg <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/14m7RGYPh17lv1EQ5Tv9gbpj1kvzlVmeUQRZs0MkUyUw/edit#gid=0")

# 2. Turn dataframe into vector
non_emerg <- as.vector(unlist(non_emerg$'ICD-10'))

#3. Read in data
scp <- readr::read_csv("Dropbox/DATALAB/ER_Project/scp_data")

#4. Make diag into one columns with multiple rows per patient
scp <- scp %>% pivot_longer(starts_with("Diag"))

#5. Filter by patient ID to see what patients have a ICD 10 code in the vector (OPTIONAL)
scp %>% group_by(Patient_ID) %>% filter(value %in% non_emerg)

#6. Filter by unique ids (OPTIONAL)
ids <- pull(scp, Patient_ID) %>% unique(ids)

#7. Make Diag 1-18 cols again, n/a unless diag is in ICD vector
  # NOTE: if did step #6, rows are of only patients with at least one ICD 10 code in vector, 
  # so patients with none are removed. Columns with values that are not in 
  # vector are changed to N/A as a result of the sort and pivot wider.
  scp <- scp %>% pivot_wider(names_from = name, values_from = value)

# Searching the data frame for all the values in our 'acs' vector:
  View(scp %>%
         group_by(value) %>% 
         filter(value %in% acs) %>% 
         tally)
    # NOTE - PROBLEM: This does not search the data frame for all values that START WITH every 
    # value in our vector, must fix.

# SUCCESS - This code allows us to search the data frame for all values that ARE or START WITH 
  # any value in our vector.
acs_combined <- paste0( acs, collapse = "|^" ) # "collapse" squishes the vector into "___ or ___ or ___.." 
                                                # statement that grepl() can read. 
                                              # "^" tells R to search for anything that STARTS WITH the value
                                                # the follows. 
  
acs_combined <- paste0("^", acs_combined) # This adds the "^" to the first value, b/c it didnt do it in 
                                            # the last code for some reason. 

# View all rows of data that show an acs icd 10 code,not very helpful
  # just practice.
View(scp_long %>% 
  filter(grepl(acs, value)))

#######################################
# DIAG 1 is is the primary diagnosis, so how many hospital visits had a primary
  # diagnosis that was an acs condition?

acs_primdiag <- scp %>%
  mutate(primary = grepl(acs, Diag1)) %>% 
  group_by(...1) %>% 
  summarize(code_sum = sum(primary),
            acsprim_YN = ifelse(code_sum > 0, "Yes", "No"))

View(test %>% 
  group_by(acsprim_YN) %>%
  tally)
################
# How many visits had an 'ER_Record_Flag'?
View(scp %>% 
  group_by(ER_Record_Flag) %>% 
  tally)







