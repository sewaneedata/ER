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
acs <- as.vector(unlist(acs$'ICD_10_code'))  

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
perc_zip <- df %>%
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
# Trying to search for ACS conditions in the diag columns using a vector fo ACS codes:
diag <- df %>% 
  select(c(Diag1, Diag2, Diag3, Diag4, Diag5, Diag6, Diag7, Diag8, Diag9, Diag10, Diag11,
           Diag12, Diag13, Diag14, Diag15, Diag16, Diag17, Diag18))
  
diag %>%
  filter(grepl("M17", Diag1))

diag %>%
  filter(Diag1 == 'acs')

acs

for(code in acs){
  diag %>% 
    filter(grepl(code, Diag1))
}

diag %>% 
  filter(grepl("F11", Diag1))

######################################## 
# vector for substance abuse ICD10 codes --- 
sub_abuse <- c()

for(code in 11:19){
  new_value <- paste('F', as.character(code), sep = "")
  sub_abuse <- c(sub_abuse, new_value)
}
sub_abuse
# vector for mental health ICD10 codes


# vector for non-emergent ICD10 codes
url <- "https://docs.google.com/spreadsheets/d/14m7RGYPh17lv1EQ5Tv9gbpj1kvzlVmeUQRZs0MkUyUw/edit?usp=sharing"

non_emergent <- gsheet2tbl(url)

non_emerg <- as.vector(unlist(non_emergent$'ICD-10')) 




