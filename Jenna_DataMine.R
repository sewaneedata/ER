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
non_emergent <- as.vector(unlist(non_emergent$'ICD-10'))

#3. Read in data
scp <- read.csv("C:/Users/jplus/OneDrive/Documents/DataLab/ER_Usage/Transform_Data/scp_data_1")

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
#scp <- scp %>% pivot_wider(names_from = name, values_from = value)


################################################################################
#DATA MINE ----
################################################################################
#What number of patients has a non-emergent ICD-10 code as their primary diag.
scp %>% filter(name == 'Diag1', value %in% non_emergent) %>% 
  tally()

#What number of outpatient visits IN THE ER had a primary diag that was non-emergent
scp %>% 
  filter(name == 'Diag1', value %in% non_emergent, File_Type == 'O', ER_Record_Flag == "Y") %>% 
  tally()

# Filtering for what diag where primary diag non-emergency
prec_non <- scp %>% filter(name == 'Diag1', value %in% non_emergent, ER_Record_Flag == "Y") %>% 
  mutate(primary_non = "T")

#Of all non-emergency primary diag, what precentages are in and out patinet
prec_non %>% 
  group_by(File_Type) %>% 
  tally() %>% 
  summarize( across(everything()), total = sum(n), percent = (n/total)*100)

#What ICD-10 codes are reoccuring for nno-emergcny outpatient primary diag in ER OUTPATIENT?
prec_non %>% filter(File_Type == "O") %>% group_by(value) %>% tally() %>% arrange(desc(n))
#FOUND: UTI, High Blood Pressure

#What gender has the highest % of sex has non emergency primary diag that was in ER
prec_non %>% group_by(Patient_Sex) %>% tally() %>% arrange(desc(n))
#FOUND: More females than males

#Sort by Zip
prec_non %>% group_by(Patient_Zip) %>% tally() %>% arrange(desc(n))
#FOUND: Top zips are around major city do may be a population problem, looking at %

#Find num of people from TN_County
prec_non %>% group_by(TN_Co_Res) %>% tally() %>% arrange(desc(n))

#Find most frequented hospitals by JARID
prec_non %>% group_by(JARID) %>% tally() %>% arrange(desc(n))
#FOUND: 26224 -Southern TN Medical Center is most used

# Precent non but looking at % by visits from that zip


# prec_non <- scp %>% filter(name == 'Diag1', value %in% non_emergent, ER_Record_Flag == "Y") %>%
#   mutate(primary_non = (ifelse((name == 'Diag1', value %in% non_emergent, ER_Record_Flag == "Y"), 
#                 "T", 
#                 "F")))

#Of all non-emergency primary diag, what precentages are in and out patinet
prec_non %>%
  group_by(Patient_Zip, primary_non) %>% 
  tally() %>% 
  summarize(total = sum(n), percent = (n/total)*100)

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
 
 ###############################################################################
 #Demographic plotting ------
 ###############################################################################
 
 library(ggplot2)
 library(ggthemes)
 
 #Sex comparison over all data set
 sex_i_o <- scp %>% drop_na(Patient_Sex) %>% 
   group_by(File_Type, Patient_Sex) %>% tally() 

 require(scales)
 io <- c("I" = "Inpatient", "O" = "Outpatient")
 ggplot(data = sex_i_o, aes(x = Patient_Sex, y = n, fill = Patient_Sex)) + 
   geom_col() +
   facet_wrap(~File_Type, labeller = as_labeller(io)) + 
   scale_y_continuous(labels = comma) + 
   labs(x = "Patient Sex",
        y = "# of Patients",
        title = "Number of Patients from SCP",
        subtitle = "By Sex",
        caption = "DataLab 2022") + theme(legend.position = 'NONE')
 scp %>% filter(ER_Record_Flag == "Y") %>% group_by(File_Type) %>% tally()
 
 
 #What day of week is ER most frequented?
 #What hour is ER most frequesnted
 #Adults vs. Kids?
 #Age Range selection

 ###############################################################################
 # Data mining -----
 ###############################################################################
 
 test <- scp %>% filter(value == "I10", ER_Record_Flag == "Y")
#3. 267965076, 5. 207253173, 6. 451409435 
 
 patient_test <- scp %>% filter(Patient_ID == 207253173)
 #Z6841 BMI with Pinched nerve as main DIAG
 
 patient_test <- scp %>% filter(Patient_ID == 267965076)
 
 #Young patient with I10 as a Diag: 424097447
 patient_test <- scp %>% filter(Patient_ID == 424097447)
 
 #Young patient with I10 as a Diag: 725071289 and 461986790
 patient_test <- scp %>% filter(Patient_ID == 725071289)
 patient_test <- scp %>% filter(Patient_ID == 461986790)
 
 
 
 ###############################################################################
 # DATA MINING IN ACSC INFO --------------
 ###############################################################################
 library(dplyr)
 library(readr)
 library(tidyverse)
 library(ggplot2)
 library(gsheet)
 
 ###########################
 # VECTORS of ICD-10 codes:
 
 # 1. ACSC codes vector: 
 # NOTE: dental conditions are included in ACSC code vector, but we also have a separate
 # vector of just dental codes so we can look at those separately.
 
 
 # create an object out of our google sheet with codes
 acs <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/14fZ-1PFInHdL8OIaLUSHhc7ulFwSf9i9O32SPitCkLU/edit?usp=sharing")  
 
 # create a vector out of the object
 acs <- as.vector(unlist(acs$'ICD_10_code'))
 
 # make vector readable by grepl():
 acs <- paste0( acs, collapse = "|^" ) 
 # NOTES for why I did this ^ :
 # "collapse" squishes the vector into "___ or ___ or ___.." statement that grepl() can read. 
 # "^" tells R to search for anything that STARTS WITH the value
 # the follows. 
 
 acs <- paste0("^", acs) # This adds the "^" to the first value, b/c it didn't do it in 
 # the last code for some reason.
 
 # 2. NON EMERGENT codes vector:
 # Do the exact same as we did for ACSC codes vector
 non_emerg <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/14m7RGYPh17lv1EQ5Tv9gbpj1kvzlVmeUQRZs0MkUyUw/edit#gid=0")
 
 non_emerg <- as.vector(unlist(non_emerg$'ICD_10_code'))
 
 non_emerg <- paste0( non_emerg, collapse = "|^" )
 
 non_emerg <- paste0("^", non_emerg)
 
 
 ################################################
 # SEARCH THROUGH ALL DIAGNOSES COLUMNS AT ONCE:
 
 #1. Squish all Diag columns into one column with multiple rows per patient:
 scp_long <- scp %>% pivot_longer(starts_with("Diag"))
 # rename new column so it's easier to understand it's purpose
 scp_long <- rename(scp_long, visit = X)
 
 #2.To avoid counting one visit/patient as multiple visits if they have multiple 
 # ICD 10 codes in one visit, do the following...
 
 # ACSC VISITS
 acs_visit <- scp_long %>% 
   mutate(`acs?` = grepl(acs,value)) %>% # create T/F column to show if acs code is present.
   group_by(visit) %>% # group by visit so the next step can work.
   summarize(code_sum = sum(`acs?`),# create column to show total acs codes from each visit.
             acs_YN = ifelse(code_sum > 0, "Yes", "No")) # create column to show if each visit had an acs code or not.
 acs_visit <- scp_long %>% 
   mutate(`noner?` = grepl(non_emerg,value)) %>% # create T/F column to show if acs code is present.
   group_by(visit) %>% # group by visit so the next step can work.
   summarize(code_sum = sum(`noner?`), # create column to show total acs codes from each visit.
             acs_YN = ifelse(code_sum > 0, "Yes", "No"))
 
 
 acs_visit <- scp_long %>% 
   mutate(`acs?` = grepl(acs,value), `noner?` = grepl(non_emerg,value)) %>% # create T/F column to show if acs code is present.
   group_by(visit) %>% # group by visit so the next step can work.
   summarize(code_sum = sum(`acs?`), code_sum2 = sum(`noner?`),# create column to show total acs codes from each visit.
             acs_YN = ifelse(code_sum > 0, "Yes", "No"), non_yn = ifelse(code_sum2 > 0, "Yes", "No"))
 
 emergency_visits <- acs_visit %>% filter(acs_YN != "Yes", non_yn != "Yes")
 
 #How many distinct Hospitals did Grundy people visit?
 scp %>% distinct(JARID) %>% tally()
 
 View(acs_visit %>% 
        group_by(acs_YN) %>%
        tally) # Look at how many visits were for acs conditions vs non acs conditions.
 
 ###############################################################################
 ###############################################################################
 
 # ACSC VISITS
 acs_visit_count <- scp_long %>% 
   filter(ER_Record_Flag == "Y") %>% #if visit was in ER
   drop_na(value) %>% #Remove N/A diags
   mutate(`acs?` = grepl(acs,value)) %>% # create T/F column to show if acs code is present.
   group_by(visit) %>% #Group by visit
   summarize(total = n(), #count total
             code_sum = sum(`acs?`), # get total number of diag per visit, sum up T in acs?
             acs_prec = (code_sum/total)*100, #get precentage
             acs_marker = ifelse(acs_prec > 20.0, "Yes", "No")) %>% #assign yes or no if acs_prec is above 25%
   group_by(acs_marker) %>% #group by acs_marker
   tally() #count up total
             
ggplot(data = acs_visit_count, aes( x = acs_marker, y = n)) + geom_col() #ggplot of above
#It does not really show much, so I want to look into precentages now

acs_visit_count <- scp_long %>% 
  filter(ER_Record_Flag == "Y") %>% #if visit was in ER
  drop_na(value) %>% #Remove N/A diags
  mutate(`acs?` = grepl(acs,value)) %>% # create T/F column to show if acs code is present.
  group_by(visit) %>% #Group by visit
  summarize(total = n(), #count total
            code_sum = sum(`acs?`), # get total number of diag per visit, sum up T in acs?
            acs_prec = (code_sum/total)*100, #get precentage
            acs_marker = ifelse(acs_prec > 20.0, "Yes", "No")) %>% #assign yes or no if acs_prec is above 25%
  ungroup() %>% 
  group_by(acs_marker) %>% #group by acs_marker
  tally() %>%  #count up total
  summarise(across(everything()), total1 = sum(n), prec = (n/total1)*100) #get % of ER visits with 20% or more ACSC diags 
#FOUND: 28.5% of all ER visits have 20% or more ACSC Diags. 

#Now looking into avg % of diags with ACSC conditions from ER
avg_acsc_visit <- scp_long %>% 
  filter(ER_Record_Flag == "Y") %>% #if visit was in ER
  drop_na(value) %>% #Remove N/A diags
  mutate(`acs?` = grepl(acs,value)) %>% # create T/F column to show if acs code is present.
  group_by(visit) %>% #Group by visit
  summarize(total = n(), #count total
            code_sum = sum(`acs?`), # get total number of diag per visit, sum up T in acs?
            acs_prec = (code_sum/total)*100) %>%
  summarize(avg_prec = mean(acs_prec))
#FOUND: Mean % of diags that were acsc among all patients is 16% 

################################################################################
# Exploring Weekday Info

library(lubridate)

#Make new DF that translates date into a readable format
date_scp <- scp %>% 
  mutate(datetime = mdy(creation_dt), 
         week_day = weekdays(datetime), #make a weekday value for each row
         month = month(datetime)) #make a month value for each row

#Inpatient and OUtpatient visits over the course of the whole year
date_scp1 <- date_scp %>% group_by(month, File_Type) %>% tally()

#Make Plot
require(scales)
ggplot(data = date_scp1, aes(x = month, y = n, color = File_Type)) + 
  geom_point() + 
  geom_line() +
  scale_x_continuous(breaks = 1:12)

# Make a new graph looking into how ACSC correlates by month
date_scp2 <- scp_long %>% 
  mutate(`acs?` = grepl(acs,value), 
         datetime = mdy(creation_dt), 
         week_day = weekdays(datetime),
         month = month(datetime)) %>% 
  group_by(visit, month, File_Type) %>% 
  summarize(code_sum = sum(`acs?`),# create column to show total acs codes from each visit.
         acs_YN = ifelse(code_sum > 0, "Yes", "No")) %>% 
  group_by( month, File_Type, acs_YN) %>% 
  tally()

#Inpatient and Outpatient showing total visits by month showing what num of visits ACSC
ggplot( data = date_scp2, aes( x = month, y = n, color = acs_YN)) +
  geom_point() + 
  geom_line() +
  facet_wrap(~File_Type) +
  scale_x_continuous(breaks = 1:12)

#FILKTER BY ER FLAG TO SEE HOW THAT CHANGES ^^^^^

################################################################################
#ACSC Monthly ER Admissions ----
# How how does ER I/O Admissions look by month and by ACSC indicator
date_scp3 <- scp_long %>% 
  mutate(`acs?` = grepl(acs,value), 
         datetime = mdy(creation_dt), 
         week_day = weekdays(datetime),
         month = month(datetime)) %>% 
  filter(ER_Record_Flag == "Y") %>% 
  group_by(visit, month, File_Type) %>% 
  summarize(code_sum = sum(`acs?`),# create column to show total acs codes from each visit.
            acs_YN = ifelse(code_sum > 0, "Yes", "No")) %>% 
  group_by( month, File_Type, acs_YN) %>% 
  tally()

#Inpatient and Outpatient showing total visits by month showing what num of visits ACSC
ggplot( data = date_scp3, aes( x = month, y = n, color = acs_YN)) +
  geom_point() + 
  geom_line() +
  facet_wrap(~File_Type) +
  scale_x_continuous(breaks = 1:12)
#FOUND: more Outpatient visits and large fluctuation in ER ACSC vs. Emergency Visits

################################################################################

# ACSC Primary Diag I/O ER Admission by month ------

month_acsc <- scp_long %>% 
  filter(name == "Diag1") %>% 
  mutate(`acs?` = grepl(acs,value), 
         datetime = mdy(creation_dt), 
         week_day = weekdays(datetime),
         month = month(datetime)) %>% 
  filter(ER_Record_Flag == "Y") %>% 
  group_by(visit, month, File_Type) %>% 
  summarize(code_sum = sum(`acs?`),# create column to show total acs codes from each visit.
            acs_YN = ifelse(code_sum > 0, "Yes", "No")) %>% 
  group_by( month, File_Type, acs_YN) %>% 
  tally()

#Inpatient and Outpatient showing total visits by month showing what num of visits ACSC
ggplot( data = month_acsc, aes( x = month, y = n, color = acs_YN)) +
  geom_point() + 
  geom_line() +
  facet_wrap(~File_Type) +
  scale_x_continuous(breaks = 1:12)

################################################################################
# Hour Data Analysis -----

# ER Admission overall by hour (by inpatient and outpatient)
hour_df <- scp %>% 
  drop_na(Admit_Hr) %>% 
  filter(ER_Record_Flag == "Y") %>% 
  group_by(File_Type, Admit_Hr) %>% 
  tally()

#To Make that graph ^
ggplot( data = hour_df, aes (x = Admit_Hr, y = n, color = File_Type)) + 
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = 0:23) #to get hours into integers 


################################################################################
# Data mining with new ACSC ------

#Test how many primary diags are ACSC with new ACSC 
test_graph <-  scp_long %>% 
  filter(name == "Diag1", ER_Record_Flag == "Y") %>% 
  mutate(`acs?` = grepl(acs,value)) %>% # create T/F column to show if acs code is present.
  group_by(visit) %>% # group by visit so the next step can work.
  summarize(code_sum = sum(`acs?`),# create column to show total acs codes from each visit.
            acs_YN = ifelse(code_sum > 0, "Yes", "No")) %>% 
  group_by(acs_YN) %>% 
  tally() #get count

ggplot( data = test_graph, aes(x = acs_YN, y = n)) + geom_col()
#FOUND: ~22% of all er VISITS HAVE A PRIMARY ACSC DIAG

################################################################################

# % of ACS and Non-Emerg Primary Diag by County
county_df <- scp %>% group_by(county, acs_primary, nonemerg_primary) %>% 
  tally() %>%
  ungroup() %>% 
  group_by(county) %>% 
  summarize(across(everything()), precent = (n/sum(n))*100) %>% 
  mutate( type = case_when( !acs_primary & !nonemerg_primary ~ "Other",
                            acs_primary ~ "ACS", 
                            nonemerg_primary ~ "Non emergent" )) %>% 
  mutate(Condition = ifelse(type == 'Other', "Other", "ACS/Non emergent"))

ggplot(data = county_df, aes(x = type, y = precent, fill = county)) + 
  geom_bar(stat='identity', position='dodge') + 
  labs(x = "Condition",
       y = "% of Visits",
       )


# % of ACS Primary Diag by County 
county_df <- scp %>% group_by(county, acs_primary) %>% tally() %>% 
  summarize(across(everything()), precent = (n/sum(n))*100) 

ggplot(data = county_df, aes(x = county, y = precent, fill = acs_primary)) + 
  geom_bar(stat='identity', position='dodge')


test <- scp %>% filter(Diag1 == "O80")

scp <- readr::read_csv("C:/Users/jplus/OneDrive/Documents/DataLab/ER_Usage/scp_data2")


###########################################
# FINDINGS TAB WORK ----

# % of overuse for SCP vs Williamson

#Read in TN ER Records
tn_diags <- readr::read_csv("C:/Users/jplus/OneDrive/Documents/DataLab/ER_Usage/tn_conditions.csv")

#Filter out "other" conditions
tn_diags <- tn_diags %>% filter(county != "Other", Condition != "Other")

#Plot of % by county (DODGE BY COUNTY)
ggplot(data = tn_diags, aes(x = county,y = precent/100, fill = type)) +
  geom_col(position = "dodge")+
  labs(title = "Comparison of Primary Diagnosis Conditions",
       y = "Percentage of Visits",
       x = '') +
  scale_fill_manual(values=c("#74A9CF",
                             "#08306B"),
                    name = "Type of Condition") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Comparison of Primary Diagnosis Conditions",
       subtitle = "SCP Counties vs Williamson County",
       y = "% Visits to the ER") + 
  geom_text(aes(label = scales::percent(precent/100)),
            position = position_dodge(width = .9), 
            vjust = -.4)

#SCP vs Williamson Code
tn_diag_scp <- tn_diags %>% mutate(scp = ifelse(county == "Franklin" | county == "Marion" | county == "Grundy", "SCP", 
                                             ifelse(county == "Williamson", "Williamson", "Other"))) %>% 
  group_by(scp, type) %>% 
  summarize(mean_prec = mean(precent))

#Plot of % by SCP or Williamson (DODGE GRAPH)
ggplot(data = tn_diag_scp, aes(x = scp,y = mean_prec/100, fill = type)) +
  geom_col(position = "dodge")+
  labs(title = "Comparison of Primary Diagnosis Conditions",
       y = "Percentage of Visits",
       x = '') +
  scale_fill_manual(values=c("#c6dbef", 
                             "#74A9CF",
                             "#08306B"),
                    name = "Type of Condition") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Comparison of Primary Diagnosis Conditions",
       y = "Percentage of Visits to the ER") + 
  geom_text(aes(label = scales::percent(mean_prec/100)),
            position = position_dodge(width = 0.9), 
            vjust = -.5)


#Admit Hour Graph Code

admit_hour <- scp %>% group_by(Admit_Hr) %>% 
  drop_na(Admit_Hr) %>% 
  tally() %>% ungroup() %>% mutate(total = sum(n))
  # mutate(percentage = n/total*100, across(everything()))

#Admit Hour Graph
ggplot(data = admit_hour, aes(x = Admit_Hr,y = n)) +
  geom_line(aes(group=1), color = "#74a9cf", size = 2) +
  geom_point(color = "#08306b", size = 3) +
  labs( x = "Patient Admit Hour",
        y = "Number of Visits",
        title = "Number of Visits by Hour")


# Zips ER Overuse

zip_test <- scp %>%
  group_by(Patient_Zip, acs_primary, nonemerg_primary) %>%
  tally %>%
  ungroup() %>%
  group_by(Patient_Zip) %>%
  mutate(total = sum(n)) %>%
  summarise(percentage = (n/sum(n))*100, across(everything())) %>%
  mutate( type = case_when( !acs_primary & !nonemerg_primary ~ "Other",
                            acs_primary ~ "ACS",
                            nonemerg_primary ~ "Non emergent" )) %>%
  mutate(Condition = ifelse(type == 'Other', "Other", "ACS/Non emergent"))


ggplot(data = zip_test, aes(x = factor(Patient_Zip), y = percentage/100, fill = type)) + 
  geom_col(position = "dodge") +
  labs(title = "ER Overuse of Demographic",
       subtitle = "In Zipcodes",
       y = "Percentage of Visits",
       x = '') +
  scale_fill_manual(values=c("#74a9cf",
                             "#08306b",
                             "#c6dbef"),
                    name = "Type of Condition") +
  scale_y_continuous(labels = scales::percent) + 
  labs(title = "Comparison of Primary Diagnosis Conditions",
       y = "Percentage of Visits to the ER") +
  geom_text(aes(label = scales::percent(percentage/100)),
            position = position_dodge(width = 0.9), 
            vjust = -.5)


#################################################################################


library(waffle)
library(extrafont)

extrafont::font_import (path="C:/Users/jplus/OneDrive/Documents/DataLab/ER_Usage/ER/Test", pattern = "fa-", prompt =  FALSE)

loadfonts(device = "win")

extrafont::fonttable() %>% 
  dplyr::as_tibble() %>% 
  dplyr::filter(grepl("Awesom", FamilyName)) %>% 
  select(FamilyName, FontName, fontfile)


waffle(c(50, 30, 15, 5), rows = 5, title = "Your basic waffle chart")

 

 





