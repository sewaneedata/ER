################################################################################
# TN Overuse

# Description: 
# This file uses the initial database to measure overuse across TN. Used to compare selected
# counties to other counties in TN. Runs SQL to select ER visits and specific columns to 
# measure overuse and then general analysis to create comparison. 
################################################################################

##################################3
# FILTER ER ------
# SQLite
###################################

#Library
library(RSQLite)

# Reading in sqlite
portaldb <- dbConnect(SQLite(), "discharges_phi")

# List tables in database
dbListTables(portaldb)

# Column names
dbListFields(portaldb, "discharges_phi")

# Query description
# Selected ER visits and their Diag1 columns
res<-"SELECT Patient_Zip, Diag1, ER_Record_Flag FROM discharges_phi WHERE ER_Record_Flag = 'Y'"

# Creating query
ex<-dbGetQuery(portaldb, res)

# Writing query data
write.csv(ex, "tn_diag_data.csv")

# Disconnect from sqlite
dbDisconnect(portaldb)

#######################
# OVERUSE TN ------
#######################

#Libraries
library(dplyr)
library(gsheet)
library(ggplot2)
library(readr)
library(tidyverse)

#Load in CSV
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

# County zip codes
grundy_zip <- c("37301",
                "37305",
                "37313",
                "37339",
                "37356",
                "37365",
                "37366",
                "37387")

franklin_zip <- c("37375",
                  "37383")

marion_zip <- c("37397",
                "37374")

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

# Assign county
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


# Create CSV for graph generation
write.csv(tn_conditions)
