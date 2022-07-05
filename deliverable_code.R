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
                                      y = percentage/100, 
                                      fill = type)) +
  geom_col()+
  labs(title = "Comparison of Primary Diagnosis Conditions",
       y = "Percentage of Visits",
       x = '') +
  scale_fill_manual(values=c("#c6dbef",
                               "#74a9cf",
                               "#08306b"),
                      name = "Type of Condition") +
  scale_y_continuous(labels = scales::percent) + 
  labs(title = "Comparison of Primary Diagnosis Conditions",
       y = "Percentage of Visits to the ER")
######
