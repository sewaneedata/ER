library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)

##########################################################################################################

## Read in Hospital discharge data
scp <- read_csv("Downloads/scp_data")
scp <- rename(scp, visit = ...1)

scp <- scp %>% filter(ER_Record_Flag == "Y")

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

## NON EMERGENT codes vector:
  # Do the exact same as we did for ACSC codes vector
non_emerg <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/14m7RGYPh17lv1EQ5Tv9gbpj1kvzlVmeUQRZs0MkUyUw/edit#gid=0")

non_emerg <- as.vector(unlist(non_emerg$'ICD_10_code'))

non_emerg <- paste0( non_emerg, collapse = "|^" )

non_emerg <- paste0("^", non_emerg)

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

## SUBSTANCE ABUSE codes vector
sub_abuse <- c()

for(code in 11:19){
  new_value <- paste('F', as.character(code), sep = "")
  sub_abuse <- c(sub_abuse, new_value)
}

sub_abuse <- paste0( sub_abuse, collapse = "|^" )
sub_abuse <- paste0("^", sub_abuse)

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
##########################################################################################################

  # What percentage of all visits to the ER had a primary diagnosis of ACSC?
scp %>% 
  group_by(acs_primary) %>%
  tally %>% 
  mutate(total = sum(n)) %>%
  group_by(acs_primary, n, total) %>% 
  summarise(perc = n/total*100)
# The column name in group_by() can be replaced to see trends for other conditions
# (mental, dental, substance abuse, etc.)


##########################################################################################################

# New columns that indicate whether or not the primary diagnosis was ACSC, mental, dental, etc.,
  # New column called 'Age_Group' that sorts patients into groups by decade,
  # Removes N/As for Patient_Sex.

scp <- scp %>%
  drop_na(Patient_Sex) %>% 
  mutate(acs_primary = grepl(acs, Diag1), 
         nonemerg_primary = grepl(non_emerg, Diag1),
         mental_primary = grepl(mental, Diag1),
         subabuse_primary = grepl(sub_abuse, Diag1),
         dental_primary = grepl(dental, Diag1),
         age_group = cut(Age,
                         breaks = 10,
                         labels = c('0-9', '10-19', '20-29', '30-39', '40-49',
                                    '50-59', '60-69', '70-79', '80-89', '90-99')))
##########################################################################################################
## BRAINSTORM

  # How many ER Flags are Yes or No
table(scp$ER_Record_Flag) 

  # How many Type of ER visits are there
ER  <- scp_data %>% 
  select(Type_ER_Visit)
table(ER)

##########################################################################################################
  
  # What percentage of all visits to the ER had a primary diagnosis of ACSC?
  perc_acsc <- scp %>% 
  group_by(acs_primary) %>%
  tally %>% 
  mutate(total = sum(n)) %>%
  group_by(acs_primary, n, total) %>% 
  summarise(perc = n/total*100)
  

  # What percentage of all visits to the ER had a primary diagnosis of non emergencies? 
    # The column name in group_by() can be replaced to see trends for other conditions
    # (non emergent, mental, dental, substance abuse, etc.)
    # example below shows non emergent trends
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

  # What hospitals are people from Grundy zip codes going to most frequently?
hospital_grundy <- scp %>% 
  filter(Patient_Zip %in% c('A','B','C','D','F','K','L','N')) %>% 
  group_by(JARID, Patient_Zip) %>% 
  tally()
    
    # For which conditions?
acs_grundy_a <- scp %>% 
  filter(Patient_Zip == 'A') %>% 
  group_by(JARID, Diag1) %>% 
  tally()

  # What does the number of UTI look like in different counties for different genders?
  
  

