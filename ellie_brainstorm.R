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
################################
# Static graphs
insurance_overuse <- scp %>%
  filter(insurance %in% c("TennCare", "MediCare", "Self Paid", "Commercial")) %>% 
  group_by(insurance, acs_primary, nonemerg_primary) %>% 
  tally %>% 
  ungroup() %>% 
  group_by(insurance) %>% 
  mutate(total = sum(n)) %>%
  summarise(percentage = n/total*100, across(everything())) %>% 
  mutate(type = case_when(!acs_primary & !nonemerg_primary ~ "Appropriate Use", 
          acs_primary ~ "ACS",
          nonemerg_primary ~ "Non emergent")) %>% 
  mutate(type2 = ifelse(type == "Appropriate Use", "Appropriate Use", "Overuse")) %>% 
  filter(type2 != "Appropriate Use")

# Dashboard graph                    
ggplot(data = insurance_overuse, aes(x = reorder(insurance, -percentage), y = percentage/100, fill = type)) +
  geom_col(position = 'dodge') +
  scale_fill_manual(name = 'Type of Condition',
                    values=c('#41b6c4',
                             '#253494')) +
  scale_y_continuous(labels = scales::percent) + 
  labs(x = ' ', y = 'Percentage of Visits',
       title = 'Severity of Overuse by Insurance Type') +
  theme_light(base_size = 18)+
  geom_text(aes(label = scales::percent(percentage/100)),
            position = position_dodge(width = 0.9),
            vjust = -.1)

# poster graph
ggplot(data = insurance_overuse, aes(x = insurance, y = percentage/100, fill = type)) +
  geom_col(position = 'dodge') +
  scale_fill_manual(name = 'Type of Condition',
                    values=c('#7B3294',
                             '#C2A5CF')) +
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Insurance', y = 'Percentage of Visits',
       title = 'ER Overuse by Insurance Type') +
  theme_light(base_size = 10) +
  geom_text(aes(label = scales::percent(percentage/100)),
            position = position_dodge(width = 1),
            vjust = -.05)


# graph for overview tab of conditions in SCP

scp_conditions <- scp %>%
    group_by(acs_primary,
             nonemerg_primary,
             dental_primary,
             mental_primary,
             subabuse_primary) %>%
    tally %>%
    ungroup() %>%
    summarise(percentage = (n/sum(n))*100, across(everything())) %>%
    mutate(type = case_when(!dental_primary & !acs_primary & !subabuse_primary & !mental_primary & !nonemerg_primary ~ "Other",
                            dental_primary ~ "Dental",
                            acs_primary ~ "ACS",
                            subabuse_primary ~ "Substance Abuse",
                            mental_primary ~ "Mental Health",
                            nonemerg_primary ~ "Non emergent")) %>%
  filter(type != "Other") %>% 
  arrange(desc(percentage))

scp_conditions <- scp_conditions[-6,]

# dashboard, conditions
  ggplot(data = scp_conditions, 
         aes(x = reorder(type, -percentage),
             y = percentage/100,
             fill = type)) +
    geom_col() +
    scale_y_continuous(labels = scales::percent) + 
    theme_light(base_size = 10) +
    labs(title = "ER Visits by Condition Type",
         subtitle = "On the South Cumberland Plateau",
      x = "Type of Condition",
         y = "Percentage of Patient Visits") +
    scale_fill_manual(values=c('#fdcc8a',
                               '#a1dab4',
                               '#41b6c4',
                               '#2c7fb8',
                               '#253494'),
                      name = "Type of Condition") +
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank()) +
    geom_text(aes(label = scales::percent(percentage/100)),
                position = position_dodge(width = 0.9),
                vjust = -.1)

  
scp_conditions <- scp_conditions %>% 
  mutate(type = reorder(type, -percentage))

# poster, conditions
  ggplot(data = scp_conditions, 
         aes(x = type,
             y = percentage/100,
             fill = type)) +
    geom_col() +
    scale_y_continuous(labels = scales::percent) + 
    theme_light(base_size = 12) +
    labs(title = "ER Visits by Condition Type",
         x = "Type of Condition",
         y = "Percentage of Patient Visits") +
    scale_fill_manual(values=c('#7B3294',
                               '#C2A5CF',
                               '#008837',
                               '#A6DBA0',
                               '#636363'),
                      name = "Type of Condition") +
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank()) +
    geom_text(aes(label = scales::percent(percentage/100)),
              position = position_dodge(width = 0.9),
              vjust = -.1)
  
# poster, insurance
  ggplot(data = insurance_overuse, aes(x = insurance, y = percentage/100, fill = type)) +
    geom_col(position = 'dodge') +
    scale_fill_manual(name = 'Type of Condition',
                      values=c('#7B3294',
                               '#C2A5CF')) +
    scale_y_continuous(labels = scales::percent) + 
    labs(x = 'Insurance', y = 'Percentage of Visits',
         title = 'ER Overuse by Insurance Type') +
    theme_light(base_size = 12) +
    geom_text(aes(label = scales::percent(x = percentage/100, accuracy = 0.01)),
              position = position_dodge(width = 0.9),
              vjust = -.1) 
  
  
  scp %>% 
    group_by(age_group) %>% 
    tally

test %>% 
  group_by(age_group) %>% 
  tally
  scp %>% 
    filter(Age %in% c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)) %>% 
    group_by(Age) %>% 
    tally %>% 
    summarise(total = sum(n))
  
  
  
  
  
test <- scp %>% 
  filter(insurance == "MediCare") %>% 
  group_by(insurance, 
           acs_primary,
           nonemerg_primary,
           dental_primary,
           mental_primary,
           subabuse_primary) %>%
  tally %>%
  ungroup() %>%
  summarise(percentage = (n/sum(n))*100, across(everything())) %>%
  mutate(type = case_when(!dental_primary & !acs_primary & !subabuse_primary & !mental_primary & !nonemerg_primary ~ "Other",
                          dental_primary ~ "Dental",
                          acs_primary ~ "ACS",
                          subabuse_primary ~ "Substance Abuse",
                          mental_primary ~ "Mental Health",
                          mental_primary & nonemerg_primary ~ "n",
                          nonemerg_primary ~ "Non emergent")) %>%
  filter(type != "Other") %>% 
  filter(percentage >= 0.02) %>% 
  arrange(desc(percentage))





