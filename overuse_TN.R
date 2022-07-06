# TN OVERUSE AVERAGES
# Description: In this script, we read in the initial data set and keep all ER visits 
# in total as well as their Diag and zipcode in order to compare the SCP overuse % to 
# a county that has better healthcare (Williamson) and the entirety of TN

##################################3
# NEW CSV ------
###################################
#SQLITE in RSTUDIO example!

library(RSQLite)

# Reading in sqlite
portaldb <- dbConnect(SQLite(), "discharges_phi")

# List tables in database
dbListTables(portaldb)

# Column names
dbListFields(portaldb, "discharges_phi")

# Query description
# Where I selected all rows I wanted to keep since couldnt write a csv with all columns
res<-"SELECT Patient_Zip, Diag1, ER_Record_Flag FROM discharges_phi WHERE ER_Record_Flag = 'Y'"
# Creating query
ex<-dbGetQuery(portaldb, res)

# Writing query data
write.csv(ex, "tn_diag_data.csv")
# One time thing

# Disconnect from sqlite
dbDisconnect(portaldb)


# FILTER NEW CSV ------------------

library(dplyr)
library(gsheet)
library(ggplot2)
library(readr)
library(tidyverse)

#Load in new CSV
tn_diags <- readr::read_csv("tn_diag_data.csv")

#ACSC List
acs <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/14fZ-1PFInHdL8OIaLUSHhc7ulFwSf9i9O32SPitCkLU/edit?usp=sharing")
acs <- as.vector(unlist(acs$'ICD_10_code'))
acs <- paste0( acs, collapse = "|^" )
acs <- paste0("^", acs)

#NON EMERGENT LIST
non_emerg <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/14m7RGYPh17lv1EQ5Tv9gbpj1kvzlVmeUQRZs0MkUyUw/edit#gid=0")
non_emerg <- as.vector(unlist(non_emerg$'ICD_10_code'))
non_emerg <- paste0( non_emerg, collapse = "|^" )
non_emerg <- paste0("^", non_emerg)

grundy_zip <- c("37301",
                "37305",
                "37313",
                "37339",
                "37356",
                "37365",
                "37366",
                "37387")

franklin_zip <- c("37324",
                  "37375",
                  "37383")

marion_zip <- c("37397",
                "37405",
                "37419")

williamson_zip <- c("37014",
                    "37024",
                    "37027",
                    "37046",
                    "37062",
                    "37064",
                    "37065",
                    "37067",
                    "37068",
                    "37069",
                    "37135",
                    "37179",
                    "38476")
tn_diags <- tn_diags %>% mutate(acs_primary = grepl(acs, Diag1),
         nonemerg_primary = grepl(non_emerg, Diag1),
         county = ifelse(Patient_Zip %in% grundy_zip,
                         "Grundy",
                         ifelse( Patient_Zip %in% franklin_zip,
                                 "Franklin",
                                 ifelse(Patient_Zip %in% marion_zip,
                                        "Marion", 
                                        ifelse(Patient_Zip %in% williamson_zip,
                                                         "Williamson",
                                        "Other")))))

# Assign % for each county and what is ACS/NONEMERGENT/Other
tn_diag2 <- tn_diags %>% group_by(county, acs_primary, nonemerg_primary) %>%
  tally() %>%
  ungroup() %>%
  group_by(county) %>%
  summarize(across(everything()), precent = (n/sum(n))*100) %>%
  mutate( type = case_when( !acs_primary & !nonemerg_primary ~ "Other",
                            acs_primary ~ "ACS",
                            nonemerg_primary ~ "Non emergent" )) %>%
  mutate(Condition = ifelse(type == 'Other', "Other", "ACS/Non emergent"))

write.csv(tn_diag2) #############

#Filter out "other" conditions
tn_diag3 <- tn_diag2 %>% filter(Condition != "Other", county != "Other")

#Plot of % by county
ggplot(data = tn_diag3, aes(x = county,y = precent/100, fill = type)) +
  geom_col(position = "dodge")+
  labs(title = "Comparison of Primary Diagnosis Conditions",
       y = "Percentage of Visits",
       x = '') +
  scale_fill_manual(values=c("#74A9CF",
                             "#08306B"),
                    name = "Type of Condition") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Comparison of Primary Diagnosis Conditions",
       y = "Percentage of Visits to the ER")


###################################3
# SCP vs. WILLIAMSON vs. OTHER-------
###################################3


# Code to make graph by SCP and Williamson
tn_diag3 <- tn_diag3 %>% mutate(scp = ifelse(county == "Franklin" | county == "Marion" | county == "Grundy", "SCP", 
                                             ifelse(county == "Williamson", "Williamson", "Other"))) %>% 
  group_by(scp, type) %>% 
  summarize(mean_prec = mean(precent))

#Plot of % by SCP or Williamson (DODGE GRAPH)
ggplot(data = tn_diag3, aes(x = scp,y = mean_prec/100, fill = type)) +
  geom_col(position = "dodge")+
  labs(title = "Comparison of Primary Diagnosis Conditions",
       y = "Percentage of Visits",
       x = '') +
  scale_fill_manual(values=c("#74A9CF",
                             "#08306B"),
                    name = "Type of Condition") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Comparison of Primary Diagnosis Conditions",
       y = "Percentage of Visits to the ER") + geom_text(aes(label = scales::percent(mean_prec/100)),position = position_dodge(width = 0.9), vjust = -.5)



                                       
