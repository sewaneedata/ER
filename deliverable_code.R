################################
# CODE FOR OUR DELIVERABLES .... NOT INDIVIDUAL BRAINSTORMING
################################

# Code for basic ER overuse table & plot:
# table & variable
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

# bar chart
ggplot(data = acs_nonemerg_other, aes(x = Condition, 
                                      y = percentage, 
                                      fill = type)) +
  geom_col()+
  labs(title = "Comparison of Primary Diagnosis Conditions",
       y = "Percentage of Visits") +
  scale_fill_discrete(name = "Type of Condition")

######
