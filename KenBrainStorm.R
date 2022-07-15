library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(gsheet)

##############################################################################################################################################################################################################################################################################################################################
##############################################################################################################################################################################################################################################################################################################################

## Read in Hospital discharge data
scp_data2 <- read_csv("scp_data2")
scp <- scp_data2

  # Rename weird column
scp <- rename(scp, visit = ...1)

scp <- scp %>% filter(ER_Record_Flag == "Y")

##############################################################################################################################################################################################################################################################################################################################
##############################################################################################################################################################################################################################################################################################################################
##############################################################################################################################################################################################################################################################################################################################

## VECTORS of ICD-10 codes:

  # ACSC codes vector: 
    # NOTE: dental conditions are included in ACSC code vector, but we also have a separate
    # vector of just dental codes so we can look at those separately.

  # Create an object out of our Google sheet with codes
acs <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/14fZ-1PFInHdL8OIaLUSHhc7ulFwSf9i9O32SPitCkLU/edit?usp=sharing")  

  # Create a vector out of the object
acs <- as.vector(unlist(acs$'ICD_10_code'))

  # Make vector readable by grepl()
acs <- paste0( acs, collapse = "|^" ) 

# NOTES for why I did this ^ :

# "collapse" squishes the vector into "___ or ___ or ___.." statement that grepl() can read. 
# "^" tells R to search for anything that IS or STARTS WITH the value that it precedes. 

acs <- paste0("^", acs) # This adds the "^" to the first value, b/c it didn't do it in 
# the last code for some reason.

##############################################################################################################################################################################################################################################################################################################################
##############################################################################################################################################################################################################################################################################################################################

## NON EMERGENT codes vector:
  # Do the exact same as we did for ACSC codes vector
non_emerg <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/14m7RGYPh17lv1EQ5Tv9gbpj1kvzlVmeUQRZs0MkUyUw/edit#gid=0")

non_emerg <- as.vector(unlist(non_emerg$'ICD_10_code'))

non_emerg <- paste0( non_emerg, collapse = "|^" )

non_emerg <- paste0("^", non_emerg)

##############################################################################################################################################################################################################################################################################################################################
##############################################################################################################################################################################################################################################################################################################################

## MENTAL HEALTH codes vector:
  # NOTE: substance abuse codes are included in "mental" vector, but we also have a separate
  # Vector of just sub abuse codes so we can look at those separately.

  # Can make this vector differently than ACSC & NON_EMERG vectors b/c the codes are simpler

  # Make an empty vector:
mental <- c()

  # Fill with 
for(code in 01:99){
  if (code < 10){ #here we had to do an if else statement so that the vector would say
    # F01, F02, etc, instead of F1, F2, etc.
    new_value <- paste('F0', as.character(code), sep = "")
  }
  else{
    new_value <- paste('F', as.character(code), sep = "")
  }
  mental <- c(mental, new_value)
}

  # now do paste0 function same as ACSC & NON EMERGENT vectors 
mental <- paste0( mental, collapse = "|^" )
mental <- paste0("^", mental)

##############################################################################################################################################################################################################################################################################################################################
##############################################################################################################################################################################################################################################################################################################################

## SUBSTANCE ABUSE codes vector
sub_abuse <- c()

for(code in 11:19){
  new_value <- paste('F', as.character(code), sep = "")
  sub_abuse <- c(sub_abuse, new_value)
}

sub_abuse <- paste0( sub_abuse, collapse = "|^" )
sub_abuse <- paste0("^", sub_abuse)

##############################################################################################################################################################################################################################################################################################################################
##############################################################################################################################################################################################################################################################################################################################

##DENTAL codes vector:
  # same process as ACSC and NON EMERGENT vectors
  # these are included in the acsc vector, but this vector allows us to look only at dental services

dental <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1m2ifrfx9yczoLSTzlOwOuxoqXpEk8LNHa2MbhC6m-AQ/edit?usp=sharing")

dental <- as.vector(unlist(dental$'ICD_10_code'))

dental <- paste0( dental, collapse = "|^" )
dental <- paste0("^", dental)

##############################################################################################################################################################################################################################################################################################################################
##############################################################################################################################################################################################################################################################################################################################

## Making vectors for insurance
# When those characters are read they will be under a new name
Tenn_care <- c('8', '10', 'J', 'Q', 'T' )
Tri_care <- ('C')
Medi_care <- c('K','M')
Commercial_care <- c('14', '15', '16', '17', 'B', 'L')
Medi_caid <- ('D')
Unknown_insurance <- c('O','H','13') # 'H' & '13' is in here because there is no code 
Self_Paid_insurance <- ('P')
Uninsured <- ('Z')
Work_Comp <- ('W')
Self_Insured <- ('S')
Prisoner <- ('N')
Cover_Kids <- ('12')
Cover_TN <- ('11')

# When the patient insurance is read from the new vector it will be called the new insurance in a different column
scp <- scp %>% 
  mutate(insurance= case_when(Primary_Payer_Class_Cd %in% Tenn_care ~ 'TennCare',
                              Primary_Payer_Class_Cd %in% Tri_care ~ 'TriCare',
                              Primary_Payer_Class_Cd %in% Medi_care ~ 'MediCare',
                              Primary_Payer_Class_Cd %in% Commercial_care ~ 'Commercial',
                              Primary_Payer_Class_Cd %in% Medi_caid ~ 'MediCaid',
                              Primary_Payer_Class_Cd %in% Unknown_insurance ~ 'Unknown',
                              Primary_Payer_Class_Cd %in% Self_Paid_insurance ~ 'Self Pay',
                              Primary_Payer_Class_Cd %in% Uninsured ~ 'Uninsured',
                              Primary_Payer_Class_Cd %in% Work_Comp ~ 'Work Comp',
                              Primary_Payer_Class_Cd %in% Self_Insured ~ 'Self Insured',
                              Primary_Payer_Class_Cd %in% Prisoner ~ 'Prisoner',
                              Primary_Payer_Class_Cd %in% Cover_Kids ~ 'CoverKids',
                              Primary_Payer_Class_Cd %in% Cover_TN ~ 'CoverTenn' ))

##############################################################################################################################################################################################################################################################################################################################
##############################################################################################################################################################################################################################################################################################################################

# Run the following for an updated 'scp' data frame that has:

# New column "county" for county names.
# Vectors for each county
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
##############################################################################################################################################################################################################################################################################################################################
##############################################################################################################################################################################################################################################################################################################################

scp <- scp %>%
  # Removes N/As for Patient_Sex
  drop_na(Patient_Sex) %>% 
  # New columns that indicate whether or not the primary diagnosis was ACSC, mental, dental, etc.,
  mutate(acs_primary = grepl(acs, Diag1),
         nonemerg_primary = grepl(non_emerg, Diag1),
         mental_primary = grepl(mental, Diag1),
         subabuse_primary = grepl(sub_abuse, Diag1),
         dental_primary = grepl(dental, Diag1),
         # New column called 'Age_Group' that sorts patients into groups by decade
         age_group = cut(Age,
                         breaks = 10,
                         labels = c('0-9', '10-19', '20-29', '30-39', '40-49',
                                    '50-59', '60-69', '70-79', '80-89', '90-99')),
         # New column "county" for county names.
         
         county = ifelse(Patient_Zip %in% grundy_zip, 
                         "Grundy", 
                         ifelse( Patient_Zip %in% franklin_zip,
                                 "Franklin",
                                 ifelse(Patient_Zip %in% marion_zip,
                                        "Marion",
                                        "NULL"))))
##############################################################################################################################################################################################################################################################################################################################
##############################################################################################################################################################################################################################################################################################################################

## Run the following to create a new column "county_total" that will track
  # the total number of ER visits in each county.
  # Useful for calculating percentages etc.; makes code more easily reproducible.

  # Variable showing total # of ER visits for each county.
county_visits <- scp %>%
  filter(ER_Record_Flag == 'Y') %>%
  group_by(county) %>%
  tally()

  # Join 'county_visits' with 'scp' to make a new column
scp <- inner_join(scp, county_visits, by = 'county') %>%
  dplyr::rename(county_total= 'n')

## Next, run the following to add a new column that says Race in characters rather
  # than in values:

  # Make a vector of names (in order, 1 = white, 2 = Black, etc.)
races_vec <- c("White", "Black", "Native American") 

  # Create the new column called 'Race_Chr'
scp <- scp %>% 
  mutate(Race_Chr = ifelse(Race == 9, "Unkown", races_vec[Race]))

########################################################################################################################################################################################################################################################################################################################################################
##############################################################################################################################################################################################################################################################################################################################

## Code for overview graph comparing SCP to Willamson county

  # 1. Download tn_conditions.csv from google drive
  # 2. Read it in as 'tn_diags'
tn_diags <- read_csv("tn_conditions.csv")

  # 3. Filter out "other" conditions
tn_diags <- tn_diags %>% filter(county != "Other", Condition != "Other")

##############################################################################################################################################################################################################################################################################################################################
##############################################################################################################################################################################################################################################################################################################################

## Code for "condition type" map

  # Read in the shape file (remember to run ALL libraries at top of page)
zipcodes <- st_read("Tennesse_Long:Lat_Data/tl_2019_us_zcta510.shp")

# NOTE: the name of your file will change depending on where the shape file is 
# on your computer. So the "___" will change, but keep the name of the variable as
# "zipcodes" so that it matches the rest of the code/shiny app.

# Filter down to only include SCP zip codes
zipcodes <- zipcodes %>% 
  filter(ZCTA5CE10 %in% c("37301","37305","37313","37339","37356","37365", 
                          "37366","37374","37375","37383","37387","37397"))


## Create new column in 'scp' called 'zip_total' that counts up total # of ER visits
  # in each zip so we can create reproducible percentages.
zip_visits <- scp %>%
  group_by(Patient_Zip) %>%
  tally()

  # Combine the small data frame into the scp data
scp <- inner_join(scp, zip_visits, by = 'Patient_Zip') %>%
  dplyr::rename(zip_total = 'n')


  # Use new column 'zip_total' to create new variable 'scp_map' with percentages.
scp_map <- scp %>% 
  group_by(Patient_Zip) %>%
  summarise(acs_perc = (sum(acs_primary))/zip_total*100, 
            non_perc = (sum(nonemerg_primary))/zip_total*100,
            mental_perc = (sum(mental_primary))/zip_total*100,
            dental_perc = (sum(dental_primary))/zip_total*100,
            sub_perc = (sum(subabuse_primary))/zip_total*100,
            across(everything())) %>% 
  ungroup() %>% 
  group_by(Patient_Zip, acs_perc, non_perc, mental_perc, dental_perc,sub_perc) %>% 
  tally

## Rename the column in "zipcodes" that holds zip codes so I can join "zipcodes" 
  # with "scp_map" by "Patient_Zip".
zipcodes <- rename(zipcodes, Patient_Zip = ZCTA5CE10)

  # In 'scp_map', delete the row "[10]"  (it's zip should be 37383)
scp_map <- scp_map[-10,]

  # Join 'scp_map' and 'zipcodes' now that they have the same # of rows.

## Convert 'Patient_Zip' in 'zipcodes' from type character to type double/numeric 
  # so that it matches 'Patient_Zip' in 'scp_map'.
zipcodes <- zipcodes %>% 
  mutate(Patient_Zip = as.numeric(Patient_Zip))

  # Join 'zipcodes' and 'scp_map' to make a new varible called 'map'!
combine <- left_join(zipcodes, scp_map, by = "Patient_Zip")

  # Creating a palette to shade in the zip codes
pal <- colorNumeric(palette = c('#0571b0','#92c5de',  '#f7f7f7', '#f4a582', '#ca0020'), 
                    domain = combine$input$cond)

########################################################################################################################################################################################################################################################################################################################################################
##############################################################################################################################################################################################################################################################################################################################

## Code for STATIC map in map tab

  # Create variable that shows percentage of ER overuse (acs + nonemergent) per zip code.
bigpic <- scp %>% 
  group_by(Patient_Zip, acs_primary, nonemerg_primary) %>% 
  tally %>% 
  ungroup() %>% 
  group_by(Patient_Zip) %>% 
  mutate(total = sum(n)) %>% 
  summarise(percentage = n/total*100, across(everything())) %>%
  mutate(type = case_when( !acs_primary & !nonemerg_primary ~ "Appropriate Use",
                           acs_primary ~ "ACS", 
                           nonemerg_primary ~ "Non emergent" )) %>% 
  mutate(status = ifelse(type == "Appropriate Use", "Appropriate Use", 'ER Overuse')) %>% 
  filter(type != "Appropriate Use") %>% 
  mutate(overuse_perc = sum(n)/total*100) %>% 
  group_by(Patient_Zip, overuse_perc) %>% 
  tally

  # Joining variable above with 'zipcodes' data frame    
bigpic_zip <- inner_join(bigpic, zipcodes, by = "Patient_Zip")

  # Read in google sheet with town names and patient zips
towns <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1200giV6UYXfolA1UkUJ--6MUG5FUavJUGGolS_lRxks/edit?usp=sharing")

  # Join google sheet above with 'bigpic_zip'
bigpic_zip <- inner_join(bigpic_zip, towns, by = 'Patient_Zip')

  # Google sheets for hospital, urgent care, and doctor's offices lat/long data
bigpic_hosp <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1JzmcNpV1bYoW3N6sg7bYVgL33LERLbS__CtSYB1_bX4/edit?usp=sharing")

doctor <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1ZqRk8NK4qp43bA30Q0VyBEWVX9Z_Zd_bgk6_Mt_dDW8/edit?usp=sharing")

urgent_care <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1_tTNbY0YQKAVF51rQcULq0qInGoMIz9thJieHbbfXfs/edit?usp=sharing")

  # Color palette for shading the zip codes by severity of ER overuse    
palette <- colorNumeric(palette = c('#0571b0','#92c5de',  '#f7f7f7', '#f4a582', '#ca0020'),
                        domain = bigpic_zip$overuse_perc)

########################################################################################################################################################################################################################################################################################################################################################
##############################################################################################################################################################################################################################################################################################################################

## Code for INTERACTIVE map in map tab

  #Read in Google sheet of hospital lat/long information:
hosp_location <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1EHHze7ABHXTTDUvaxbAIz5efFT67MR1VMXrMSkokMQs/edit?usp=sharing")

  # 2. Create new data frame called 'top_hosp' that only has the top 3 most visited
  # hospitals from each zip code.
  # a. Arrange most visited hospitals in descending order and make a new variable.
test <- scp %>% 
  group_by(Patient_Zip, JARID) %>%
  tally %>%
  arrange(desc(n))

  # Use the above variable to create a new data frame with top 3 hospitals in each zip.  
top_hosp <- Reduce(rbind,
                   by(test,
                      test["Patient_Zip"],
                      head,
                      n = 3))

  # Make JARID in 'top_hosp' type double rather than type character.
top_hosp <- top_hosp %>% 
  mutate(JARID = as.numeric(JARID))

  # Join 'top_hosp' with 'hosp_location' using the 'JARID' column.
h <- inner_join(top_hosp, hosp_location, by = "JARID")

  # Join varible made above (h) with 'zipcodes' variable (from other map code).
hospitals <- inner_join(h, zipcodes, by = "Patient_Zip")


##############################################################################################################################################################################################################################################################################################################################
##############################################################################################################################################################################################################################################################################################################################
##############################################################################################################################################################################################################################################################################################################################

# Percentages
  
  # What percentage of all visits to the ER had a primary diagnosis of ACSC?
perc_acsc <- scp %>% 
  group_by(acs_primary) %>%
  tally %>% 
  mutate(total = sum(n)) %>%
  group_by(acs_primary, n, total) %>% 
  summarise(perc = n/total*100)

    # The column name in group_by() can be replaced to see trends for other conditions
    # (non emergent, mental, dental, substance abuse, etc.)

    # Shows non emergent trends
perc_nonemerg <- scp %>% 
  group_by(nonemerg_primary) %>%
  tally %>% 
  mutate(total = sum(n)) %>%
  group_by(nonemerg_primary, n, total) %>% 
  summarise(perc = n/total*100)

# What percentage of all visits to the ER had a primary diagnosis of mental health? 
perc_mental <- scp %>% 
  group_by(mental_primary) %>%
  tally %>% 
  mutate(total = sum(n)) %>%
  group_by(mental_primary, n, total) %>% 
  summarise(perc = n/total*100)

# What percentage of all visits to the ER had a primary diagnosis of dental? 
perc_dental <- scp %>% 
  group_by(dental_primary) %>%
  tally %>% 
  mutate(total = sum(n)) %>%
  group_by(dental_primary, n, total) %>% 
  summarise(perc = n/total*100)

# What percentage of all visits to the ER had a primary diagnosis of substance abuse? 
perc_subabuse <- scp %>% 
  group_by(subabuse_primary) %>%
  tally %>% 
  mutate(total = sum(n)) %>%
  group_by(subabuse_primary, n, total) %>% 
  summarise(perc = n/total*100)

##############################################################################################################################################################################################################################################################################################################################
##############################################################################################################################################################################################################################################################################################################################
##############################################################################################################################################################################################################################################################################################################################

## BRAINSTORM

# How many ER Flags are Yes or No
table(scp$ER_Record_Flag) 

# How many Type of ER visits are there
ER  <- scp %>% 
  select(Type_ER_Visit)
table(ER)

# What are the types of ACS conditions that we are seeing in the emergency room? 
  # Which ones are we seeing most often?
acs_types <- scp %>%
  filter(acs_primary== TRUE) %>% 
  group_by(Diag1) %>% 
  tally()

  # From which zip codes?
acs_types_zipcode <- scp %>% 
  filter(acs_primary == TRUE) %>% 
  group_by(Patient_Zip, Diag1) %>% 
  tally()

  # Graph the ACS types by zip code to see if there is an interesting trend among zip codes
ggplot(data= acs_types_zipcode) +
  geom_point(aes(x= Diag1, y= n, color= Patient_Zip))

  # How many I10 codes are there by zip code?
I10 <- scp %>% 
  filter(Diag1 == 'I10') %>% 
  group_by(Patient_Zip) %>% 
  tally()

  # How many people per zip code are going to certain hospitals for ACS conditions?
acs_zip_hospital <- scp %>% 
  filter(acs_primary == TRUE) %>% 
  group_by(Patient_Zip, JARID) %>% 
  tally()
    
    # For which conditions?
acs_grundy_a <- scp %>% 
  filter(Patient_Zip == 'A') %>% 
  group_by(JARID, Diag1) %>% 
  tally()

####################################################################################################################################################################################################################
####################################################################################################################################################################################################################
####################################################################################################################################################################################################################
## R Shiny Code


 # Top ICD 5 codes for each county, zip, and insurance
  # We already have a column with 'county_visits' so that will be used for percentages as our sum
    # What are the top ICD 10 codes of a certain county? Percentage wise

## ICD 10 codes for county
  # What are the top ICD 10 codes for Marion county?
marion_icd_perc <- scp %>% 
  filter(county== 'Marion', ER_Record_Flag == 'Y') %>% 
  group_by(Diag1, county_total) %>% 
  tally() %>% 
  arrange(desc(n))%>% 
  summarise(perc = n/county_total*100) %>% 
  arrange(desc(perc)) %>% 
  head(5)

  # Make a graph for top 5 ICD codes for Marion county
ggplot(data= marion_icd_perc) +
  geom_col(aes(x= Diag1, y= perc, fill=Diag1))+
  theme_light(base_size = 18)+
  scale_fill_manual(values= c('#fdcc8a',
    '#a1dab4',
    '#41b6c4',
    '#2c7fb8',
    '#253494'))

## ICD 10 codes for zip codes
  # Top 5 ICD 10 codes for 37301
icd_zipcode <- scp %>% 
  filter(Patient_Zip== '37301', ER_Record_Flag== 'Y') %>% 
  group_by(Diag1, zip_total) %>% 
  tally() %>%
  arrange(desc(n)) %>% 
  summarise(perc= n/zip_total*100) %>% 
  head(5)

  # Make a graph
ggplot(data= icd_zipcode) +
  geom_col(aes(x= Diag1, y= perc, fill=Diag1)) +
  theme_light(base_size = 18) +
  scale_fill_manual(values= c('#fdcc8a',
                              '#a1dab4',
                              '#41b6c4',
                              '#2c7fb8',
                              '#253494'))

## Make a stagnant graph of people going to the ER for ACS/Non-emergent reasons by insurance
eruse_insurance <- scp %>% 
  filter(insurance %in% c('TennCare', 'MediCare', 'Commercial', 'Self Pay')) %>%
  group_by(insurance, acs_primary, nonemerg_primary) %>% 
  tally() %>% 
  ungroup() %>% 
  group_by(insurance) %>% 
  mutate(total= sum(n)) %>% 
  summarise(perc= n/total*100, across(everything())) %>% 
    # Creates a column identifying the type of ER visit; ACS condition, non-emerg, or appropriate visit
  mutate(type= case_when(!acs_primary & !nonemerg_primary ~ 'Appropriate Use',
                         acs_primary ~ "ACS",
                         nonemerg_primary ~ "Nonemerg")) %>% 
    # Creates another column identifying either 'overuse' or 'appropriate use' 
  mutate(type2= ifelse(type== 'Appropriate Use', 'Appropriate Use', "Overuse"))
  

## What are the differences of ER visits by insurance type? (Difference between ACS, nonemerg, approp. by insurance)
  # position 'dodge' allows multiple bars on the x- axis
  # [order(-...)] allows the bars to go in descending order
  # 'guides' allows 222a legend title change
ggplot(data= eruse_insurance) +
  geom_col(aes(x= insurance, y= perc, fill= type2),position= 'dodge') +
  labs(x =  'Insurance', y='Percentage') +
  guides(fill=guide_legend(title= 'Insurance Type'))+
  theme_light(base_size= 18) +
  scale_fill_manual(values= c('#a1dab4',
                              '#253494'
                              ))

  # Stagnant graph for top ICD 10 codes (acs, non-emerg) for all of SCP by 
icdscp <- scp %>% 
  filter(acs_primary == 'TRUE' | nonemerg_primary== 'TRUE') %>% 
  group_by(Diag1, acs_primary, nonemerg_primary) %>% 
  tally() %>%
  ungroup() %>% 
  mutate(total = sum(n)) %>%
  group_by(Diag1) %>% 
  summarise(perc= n/total*100, across(everything())) %>% 
  arrange(desc(perc)) %>% 
  head(10)


  # Arrange data set greatest to least
icdscp <- icdscp %>% 
  mutate( Diag1 = reorder(Diag1, -perc))

  # Bar graph for top 10 ICD 10 codes for all of SCP 
ggplot(data= icdscp) +
  labs(title= ' Top 10 Diagnoses in Instances of ER Overuse ',
       subtitle= '   In The South Cumberland Plateau',
         x= 'Diagnostic Code', y= 'Percentage', fill= 'Diagnosis')+
  geom_col(aes(x= Diag1, y= perc, fill= Diag1))+
  theme_light(base_size = 18)+
  # Allows legend labels to be renamed
  scale_fill_manual(values= c('#fdcc8a',
    '#feb24c',
    '#fd8d3c',
    '#c7e9b4',
    '#7fcdbb',
    '#41b6c4',
    '#1d91c0',
    '#225ea8',
    '#253494',
    '#081d58'),
                    labels=c('Urinary Tract Infection', 'Acute Upper Respiratory Infection',
                             'Obstructive Pulmonary Disease', 'Acute Pharyngitis',
                             'Influenza', 'Gastroenteritis', 'Strep Throat', 'Periapical Abscess',
                             'Hypertension', 'Acute Bronchitis'))

## Making a graph for Top 5 icd codes by county
  # Make data set for Shiny
countyicd <- scp %>% 
  filter(acs_primary== TRUE | nonemerg_primary== TRUE) %>% 
  group_by(county, Diag1 ) %>% 
  tally()%>% 
  ungroup() %>% 
  mutate(total=sum(n)) %>% 
  group_by(Diag1) %>% 
  summarise(perc=n/total*100) %>% 
  arrange(desc(perc)) %>% 
  head(5)

  # Make the data set stats go from greatest to least
countyicd <- countyicd %>% 
  mutate( Diag1 = reorder(Diag1, -perc))

  # Make a graph for this data set
ggplot(data= countyicd) +
  geom_col(aes(x= Diag1, y= perc, fill= Diag1)) +
  labs(x= 'Diagnostic Codes', y= 'Percentage', fill= 'Diagnosis')+
  theme_light(base_size = 18) +
  scale_fill_manual(values= c('#fdcc8a',
                              '#a1dab4',
                              '#41b6c4',
                              '#2c7fb8',
                              '#253494'))

## Make a graph for top 5 ICD 10 codes for zip code
  # Make a data set 
  zipicd <- scp %>% 
    filter(acs_primary==TRUE | non_emerg==TRUE) %>% 
    group_by(Patient_Zip, Diag1) %>% 
    tally() %>% 
    ungroup() %>% 
    mutate(total=sum(n)) %>% 
    group_by(Diag1) %>% 
    summarise(perc=n/total*100) %>% 
    arrange(desc(perc)) %>% 
    head(5)
  
  zipicd <- zipicd %>% 
    mutate( Diag1 = reorder(Diag1, -perc))
  
    # Make a graph
  ggplot(data= zipicd) +
    geom_col(aes(x= Diag1, y= perc, fill= Diag1)) +
    labs(x= 'Diagnostic Codes', y= 'Percentage', fill= 'Diagnosis') +
    theme_light(base_size = 18) +
    scale_fill_manual(values= c('#fdcc8a',
                                '#a1dab4',
                                '#41b6c4',
                                '#2c7fb8',
                                '#253494'))


                            
##############################################################################################################################################################################################################################################################################################################################
##############################################################################################################################################################################################################################################################################################################################

## Brainstorm and helpful code continues

  # Top 5 ICD 10 codes for Grundy
grundy_icd_perc <- scp %>% 
  filter(county== 'Grundy', ER_Record_Flag== 'Y') %>% 
  group_by(Diag1, county_total) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  summarise(perc= n/county_total*100) %>% 
  arrange(desc(perc)) %>% 
  head(5)

  # Top 5 ICD 10 codes for Franklin
franklin_icd_perc <- scp %>% 
  filter(county== 'Franklin', ER_Record_Flag== 'Y') %>% 
  group_by(Diag1, county_total) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  summarise(perc= n/county_total*100) %>% 
  arrange(desc(perc)) %>% 
  head(5)

  # Top 5 ICD codes for all of SCP with percentages
scp_icd <- scp %>% 
filter(ER_Record_Flag == 'Y') %>% 
  mutate(scp_total = sum(unique(county_total))) %>% 
  group_by(Diag1, scp_total) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  summarise(perc= n/ scp_total*100)

  # Different types of insurances available
insurance_types <- scp %>% 
  group_by(Primary_Payer_Class_Cd) %>% 
  tally()

  # How many different names/variables are in a column?
scp$insurance %>% 
  unique()

  # How many "NAs" do we have in insurance?
scp %>% 
  filter(is.na (insurance))%>% 
  select(Primary_Payer_Class_Cd)

##############################################################################################################################################################################################################################################################################################################################
##############################################################################################################################################################################################################################################################################################################################
