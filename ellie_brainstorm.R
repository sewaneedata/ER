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

bigpic_hosp <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1JzmcNpV1bYoW3N6sg7bYVgL33LERLbS__CtSYB1_bX4/edit?usp=sharing")


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
  mutate(overuse_perc = sum(n)/total*100)
  
bigpic_zip <- inner_join(bigpic, zipcodes, by = "Patient_Zip")





