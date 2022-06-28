#Libraries -----
library(dplyr)
library(readr)
library(tidyverse)
library(ggplot2)

################################################################################
# Diag Column -----
################################################################################
# SOLUTION FOR COLUMN READING PROBLEM

# 1. Read google sheet of ICD-10 codes into R
non_emergent <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/14m7RGYPh17lv1EQ5Tv9gbpj1kvzlVmeUQRZs0MkUyUw/edit#gid=0")

# 2. Turn dataframe into vector
non_emergent <- as.vector(unlist(icd$'ICD-10'))

#3. Read in data
scp <- read.csv("C:/Users/jplus/Downloads/scp_data")

#4. Make diag into one columns with multiple rows per patient
scp <- scp %>% pivot_longer(starts_with("Diag"))

#5. Filter by patient ID to see what patients have a ICD 10 code in the vector
scp <- scp %>% group_by(Patient_ID) %>% filter(value %in% non_emergent)

#Filter by unique ids (OPTIONAL)
#ids <- pull(scp, Patient_ID) %>% unique(ids)

#6. Make Diag 1-18 cols again, n/a unless diag is in ICD vector
# NOTE: Rows are of only patients with at least one ICD 10 code in vector, 
# so patients with none are removed. Columns with values that are not in 
# vector are changed to N/A as a result of the sort and pivot wider.
scp <- scp %>% pivot_wider(names_from = name, values_from = value)

################################################################################
################################################################################
################################################################################
#General Brainstorming-----

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
  
#SOLUTION is now at top

  #1. Read in data
  scp <- read.csv("C:/Users/jplus/Downloads/scp_data")
  
  #2. Make diag into one columns with multiple rows per patient
  scp <- scp %>% pivot_longer(starts_with("Diag"))
  
  #3. Filter by patient ID to see what patients have a ICD 10 code in the vector
 scp <- scp %>% group_by(Patient_ID) %>% filter(value %in% icd)
 
 #4. Filter by unique ids (OPTIONAL)
 #ids <- pull(scp, Patient_ID) %>% unique(ids)
 
 #Make Diag cols again, n/a unless diag is in ICD vector
 scp <- scp %>% pivot_wider(names_from = name, values_from = value)





