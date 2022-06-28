# CODE FOR OUR DELIVERABLES .... NOT INDIVIDUAL BRAINSTORMING
################################

# Making a graph that expresses demographics (sex and age) of who used the ER:
  # First, make a new variable that can later be plotted
    
age_sex_ER <- scp %>%
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
       y = 'Percentage of dataset')
#######################################



