library(dplyr)
library(readr)
library(tidyverse)
library(ggplot2)

#Read in Data
scp <- read.csv("C:/Users/jplus/OneDrive/Documents/DataLab/ER_Usage/ER/scp_data")

#Remove random count row that generated when opened CSV
scp <- select( scp, -X)

#Arrange by Patient Visit Type (emergency vs. non emergency)
scp %>% group_by(Type_ER_Visit) %>% tally() %>% arrange(desc(n))

#Group by most frequented hospital
scp %>% group_by(Hospital_ID) %>% tally() %>% arrange(desc(n))

#Hospital IDs 3320, 2622, 3332 all have most patient from SCP zips

scp %>% 
  filter(Hospital_ID == 3320) %>% 
  drop_na(Type_ER_Visit) %>% 
  group_by(Type_ER_Visit) %>% 
  tally() %>%
  summarize( across(everything()), total = sum(n), percent = n/total)

# Looking at Non-Emergent ICD10

#Read google sheet into R
icd <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/14m7RGYPh17lv1EQ5Tv9gbpj1kvzlVmeUQRZs0MkUyUw/edit#gid=0")

#Turn dataframe into vector
icd <- as.vector(unlist(ne_icd_10$'ICD-10'))

#Attempt to select diag columns only, ERROR
test <- scp %>% filter(select(scp, contains("Diag")) %in% icd) %>% tally()


#Attempt to make a new column which reads all diag and determine if have any non-emergency ICD10 codes
#(TRUE) or not (FALSE), this was just a test to get the act of looping through columns and not
#an attempt of data analysis because some emergency visits have non-emergency ICD10 codes
  scp<- scp %>% mutate(non_emergency = ifelse(Diag1 %in% icd | Diag2 %in% icd | Diag3 %in% icd | Diag4 %in% icd |
                                          Diag5 %in% icd | Diag6 %in% icd | Diag7 %in% icd | Diag8 %in% icd| 
                                          Diag9 %in% icd | Diag10 %in% icd |Diag11 %in% icd | Diag12 %in% icd | 
                                          Diag13 %in% icd | Diag14 %in% icd |Diag15 %in% icd | Diag16 %in% icd | 
                                          Diag17 %in% icd | Diag18 %in% icd, "TRUE", "FALSE"))





