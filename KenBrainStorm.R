library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(gsheet)

##########################################################################################################
##########################################################################################################

## Read in Hospital discharge data
scp <- read_csv("DataLab/ER/scp_data2")
scp %>%  view

  # Rename weird column
scp <- rename(scp, visit = ...1)

scp <- scp %>% filter(ER_Record_Flag == "Y")

##########################################################################################################
##########################################################################################################
##########################################################################################################

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

##########################################################################################################
##########################################################################################################

## NON EMERGENT codes vector:
  # Do the exact same as we did for ACSC codes vector
non_emerg <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/14m7RGYPh17lv1EQ5Tv9gbpj1kvzlVmeUQRZs0MkUyUw/edit#gid=0")

non_emerg <- as.vector(unlist(non_emerg$'ICD_10_code'))

non_emerg <- paste0( non_emerg, collapse = "|^" )

non_emerg <- paste0("^", non_emerg)

##########################################################################################################
##########################################################################################################

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

##########################################################################################################
##########################################################################################################

## SUBSTANCE ABUSE codes vector
sub_abuse <- c()

for(code in 11:19){
  new_value <- paste('F', as.character(code), sep = "")
  sub_abuse <- c(sub_abuse, new_value)
}

sub_abuse <- paste0( sub_abuse, collapse = "|^" )
sub_abuse <- paste0("^", sub_abuse)

##########################################################################################################
##########################################################################################################

##DENTAL codes vector:
  # same process as ACSC and NON EMERGENT vectors
  # these are included in the acsc vector, but this vector allows us to look only at dental services

dental <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1m2ifrfx9yczoLSTzlOwOuxoqXpEk8LNHa2MbhC6m-AQ/edit?usp=sharing")

dental <- as.vector(unlist(dental$'ICD_10_code'))

dental <- paste0( dental, collapse = "|^" )
dental <- paste0("^", dental)

##########################################################################################################
##########################################################################################################

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
##########################################################################################################
##########################################################################################################

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

# Run the following to create a new column "county_total" that will track
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

# Next, run the following to add a new column that says Race in characters rather
# than in values:

# Make a vector of names (in order, 1 = white, 2 = Black, etc.)
races_vec <- c("White", "Black", "Native American") 

# Create the new column called 'Race_Chr'
scp <- scp %>% 
  mutate(Race_Chr = ifelse(Race == 9, "Unkown", races_vec[Race]))
##########################################################################################################
##########################################################################################################
##########################################################################################################

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

##########################################################################################################
##########################################################################################################
##########################################################################################################

## BRAINSTORM

# How many ER Flags are Yes or No
table(scp$ER_Record_Flag) 

# How many Type of ER visits are there
ER  <- scp %>% 
  select(Type_ER_Visit)
table(ER)

##########################################################################################################
##########################################################################################################

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

##Top ICD 5 codes for each county, zip, and insurance
  # We already have a column with 'county_visits' so that will be used for percentages as our sum
    # What are the top ICD 10 codes of a certain county? Percentage wise
  
  # What are the top ICD 10 codes for Marion county?
marion_icd_perc <- scp %>% 
  filter(county== 'Marion', ER_Record_Flag == 'Y') %>% 
  group_by(Diag1, county_total) %>% 
  tally() %>% 
  arrange(desc(n))%>% 
  summarise(perc = n/county_total*100) %>% 
  arrange(desc(perc)) %>% 
  head(5)

  # Make a graph for this data
ggplot(data= marion_icd_perc) +
  geom_col(aes(x= Diag1, y= perc, fill=Diag1))+
  theme_light(base_size = 18)+
  scale_fill_manual(values= c('#fdcc8a',
    '#a1dab4',
    '#41b6c4',
    '#2c7fb8',
    '#253494'))


