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
scp <- scp %>%
  filter(ER_Record_Flag == "Y")

# How many visits to the ER had any acsc diagnosis & calculating percentage out of
# total ER visits.
ER_acs_diag <- scp %>%
  mutate(`acs?` = grepl(acs, Diag1)) %>% 
  group_by(visit) %>%
  summarize(code_sum = sum(`acs?`),
            acs_YN = ifelse(code_sum > 0, "Yes", "No")) %>% 
  group_by(acs_YN) %>% 
  tally %>%
  mutate(total = sum(n)) %>% 
  group_by(acs_YN, n) %>% 
  summarise(percentage = n/total*100)

# PLOT
ggplot(data = ER_acs_diag, aes(x = acs_YN, y = percentage, fill = acs_YN)) +
  geom_bar(stat = 'identity')+
  labs(x = 'ACSC Diagnosis',
       y = 'Percentage of ER Visits') +
  theme(legend.position = 'none')


# How many unique hospitals in the scp data?  
scp %>% 
  group_by(JARID) %>% 
  tally %>% 
  arrange(desc(n))
########################################
# LOOKING INTO HOW TO EXPRESS ER OVERUSE (VISUALLY & WITH TABLES)

# Primary diagnosis investigation:

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

# 2. non emergent
  # table
perc_nonemerg <- scp %>% 
  group_by(nonemerg_primary) %>%
  tally %>% 
  mutate(total = sum(n)) %>%
  group_by(nonemerg_primary, n, total) %>% 
  summarise(perc = n/total*100)

  # pie chart
x <- c(82.7, 17.3)

pie(x, 
    labels = paste0(x, "%"), 
    col = c('light blue', 'red'),
    radius = .7,
    main = "Primary Diagnoses at the ER")
legend("bottomleft", legend = c('Other', 'Non Emergent'),
       fill =  c("light blue", "red"), title = "Condition")

#########
# Investigating comparisons between acsc, non emerg, and other conditions.
  # table
acs_nonemerg_other <- scp %>% 
  group_by(acs_primary, nonemerg_primary) %>% 
  tally %>% 
  ungroup() %>% 
  mutate(total = sum(n)) %>% 
  summarise(percentage = n/total*100, across(everything())) %>%
  mutate( type = case_when( !acs_primary & !nonemerg_primary ~ "Other",
                                 acs_primary ~ "ACS", 
                                 nonemerg_primary ~ "Non emergent" )) %>% 
  mutate(Condition = ifelse(type == 'Other', "Other", "ACS/Non emergent"))

  # Bar chart
ggplot(data = acs_nonemerg_other, 
       aes(x = Condition,
           y = percentage/100,
           fill = type)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent) + 
  labs(title = "Comparison of Primary Diagnosis Conditions",
       y = "Percentage of Visits") +
  scale_fill_discrete(name = "Type of Condition")

#############
# Look into specific zip codes at different conditions
zipcode_primary <- scp %>% 
  group_by(Patient_Zip, acs_primary, nonemerg_primary) %>%
  tally %>%
  ungroup() %>% 
  group_by(Patient_Zip) %>% 
  summarise(percentage = (n/sum(n))*100, across(everything())) %>%
  mutate( type = case_when( !acs_primary & !nonemerg_primary ~ "Other",
                            acs_primary ~ "ACS", 
                            nonemerg_primary ~ "Non emergent" )) %>% 
  mutate(Condition = ifelse(type == 'Other', "Other", "ACS/Non emergent"))
  
# graph of the above variable
ggplot(data = zipcode_primary, 
       aes(x = percentage/100,
           y = Patient_Zip,
           fill = type)) +
  geom_col() +
  scale_x_continuous(labels = scales::percent) + 
  labs(title = "Comparison of Primary Diagnosis Conditions",
       x = "Percent",
       y = "Zip Code") +
  scale_fill_discrete(name = "Type of Condition") +
  theme(legend.position = 'bottom')

# same data, different graph
ggplot(data = zipcode_primary, 
       aes(x = Patient_Zip,
           y = percentage/100,
           fill = type)) +
  geom_col(position = 'dodge') +
  scale_y_continuous(labels = scales::percent) + 
  labs(title = "Primary Diagnoses Across Different Zipcodes",
       x = "Percent",
       y = "Zip Code") +
  scale_fill_discrete(name = "Type of Condition") +
  theme(legend.position = 'bottom')

# Trends in specific zip codes
  # Zip A:
zipA <- scp %>% 
  filter(Patient_Zip == 'A') %>% 
  group_by(acs_primary, nonemerg_primary, mental_primary, subabuse_primary, dental_primary) %>%
  tally %>%
  ungroup() %>%
  summarise(percentage = (n/sum(n))*100, across(everything())) %>%
  mutate( type = case_when( !acs_primary & !nonemerg_primary & !mental_primary & !subabuse_primary &!dental_primary~ "Other",
                            dental_primary ~ "Dental",
                            acs_primary ~ "ACS",
                            subabuse_primary ~ "Substance Abuse",
                            mental_primary ~ "Mental Health",
                            nonemerg_primary ~ "Non emergent"))

zipB <- scp %>% 
  filter(Patient_Zip == 'B') %>% 
  group_by(acs_primary, nonemerg_primary, mental_primary, subabuse_primary, dental_primary) %>%
  tally %>%
  ungroup() %>%
  summarise(percentage = (n/sum(n))*100, across(everything())) %>%
  mutate( type = case_when( !acs_primary & !nonemerg_primary & !mental_primary & !subabuse_primary &!dental_primary~ "Other",
                            dental_primary ~ "Dental",
                            acs_primary ~ "ACS",
                            subabuse_primary ~ "Substance Abuse",
                            mental_primary ~ "Mental Health",
                            nonemerg_primary ~ "Non emergent"))

zipC <- scp %>% 
  filter(Patient_Zip == 'C') %>% 
  group_by(acs_primary, nonemerg_primary, mental_primary, subabuse_primary, dental_primary) %>%
  tally %>%
  ungroup() %>%
  summarise(percentage = (n/sum(n))*100, across(everything())) %>%
  mutate( type = case_when( !acs_primary & !nonemerg_primary & !mental_primary & !subabuse_primary &!dental_primary~ "Other",
                            dental_primary ~ "Dental",
                            acs_primary ~ "ACS",
                            subabuse_primary ~ "Substance Abuse",
                            mental_primary ~ "Mental Health",
                            nonemerg_primary ~ "Non emergent"))


zipE <- scp %>% 
  filter(Patient_Zip == 'E') %>% 
  group_by(acs_primary, nonemerg_primary, mental_primary, subabuse_primary, dental_primary) %>%
  tally %>%
  ungroup() %>%
  summarise(percentage = (n/sum(n))*100, across(everything())) %>%
  mutate( type = case_when( !acs_primary & !nonemerg_primary & !mental_primary & !subabuse_primary &!dental_primary~ "Other",
                            dental_primary ~ "Dental",
                            acs_primary ~ "ACS",
                            subabuse_primary ~ "Substance Abuse",
                            mental_primary ~ "Mental Health",
                            nonemerg_primary ~ "Non emergent"))
#######################################
# trends in Race
# make vector of names so that 1,2,3 etc don't show up on the graph
races_vec <- c("White", "Black", "Native American") 

# create a new column that has race in characters rather than values
scp <- scp %>% 
  mutate(Race_Chr = ifelse(Race == 9, "Unkown", races_vec[Race]))

# table
race <- scp %>%
  filter(Race_Chr == c("White", "Black", "Native American")) %>% 
  group_by(Race_Chr, acs_primary, nonemerg_primary, mental_primary) %>%
  tally %>%
  ungroup()%>%
  group_by(Race_Chr) %>% 
  summarise(percentage = (n/sum(n))*100, across(everything())) %>%
  mutate( type = case_when( !acs_primary & !nonemerg_primary & !mental_primary ~ "Other",
                            acs_primary ~ "ACS",
                            mental_primary ~ "Mental Health",
                            nonemerg_primary ~ "Non emergent")) %>%
  mutate(condition = ifelse(type == "Other", "Other", "ACS/Mental health/Non emergent"))

ggplot(data = race, aes(x = condition, y = percentage, fill = type)) +
  geom_col() +
  labs(title = "Trends")+
  theme(legend.position = 'bottom') +
  scale_fill_discrete(name = "Condition") +
  facet_wrap(~Race_Chr) +
  x.axis.ticks = element_blank()


