# CODE FOR OUR DELIVERABLES .... NOT INDIVIDUAL BRAINSTORMING
################################

# Making a graph that expresses demographics of our dataset by Sex and Age group:
  # First, make variable of age groups (by decade) with each age group broken down into sex.
    # removed unnecessary rows (age = 999 and sex=N/A)

df_age <-  df %>%
  filter(Age < 999) %>%
  drop_na(Patient_Sex) %>% 
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

#33333