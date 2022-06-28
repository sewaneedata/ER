# THIS IS OUR SHARED SCRIPT THAT INCLUDES ALL HELPFUL CODE NEEDED TO SET UP DATA-MINING
################
# THIS IS NOT OUR DELIVERABLE CODE
##############################################################

# libraries:
library(dplyr)
library(readr)
library(tidyverse)
library(ggplot2)
library(gsheet)

# scp_data data frame
scp <- readr::read_csv("Dropbox/DATALAB/ER_Project/scp_data")

# NOTE: If we determine that "ER_Record_Flag" indicates that the ER was used, we will add
# the following code to filter the 'scp' data down further to only ER visits.
    
  # scp <- scp %>% filter(ER_Record_Flag == "Y")

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
      # "^" tells R to search for anything that IS or STARTS WITH the value that it precedes. 

acs <- paste0("^", acs) # This adds the "^" to the first value, b/c it didn't do it in 
# the last code for some reason.

# 2. NON EMERGENT codes vector:
  # Do the exact same as we did for ACSC codes vector
non_emerg <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/14m7RGYPh17lv1EQ5Tv9gbpj1kvzlVmeUQRZs0MkUyUw/edit#gid=0")

non_emerg <- as.vector(unlist(non_emerg$'ICD_10_code'))

non_emerg <- paste0( non_emerg, collapse = "|^" )

non_emerg <- paste0("^", non_emerg)

# 3. MENTAL HEALTH codes vector:
  # NOTE: substance abuse codes are included in "mental" vector, but we also have a separate
    # vector of just sub abuse codes so we can look at those separately.
  
# Can make this vector differently than ACSC & NON_EMERG vectors b/c the codes are simpler
  # make an empty vector
mental <- c()
  # fill with 
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

# 4. SUBSTANCE ABUSE codes vector:
sub_abuse <- c()

for(code in 11:19){
  new_value <- paste('F', as.character(code), sep = "")
  sub_abuse <- c(sub_abuse, new_value)
}

sub_abuse <- paste0( sub_abuse, collapse = "|^" )
sub_abuse <- paste0("^", sub_abuse)

# 5. DENTAL codes vector:
  # same process as ACSC and NON EMERGENT vectors
  # these are included in the acsc vector, but this vector allows us to look only at dental services.
dental <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1m2ifrfx9yczoLSTzlOwOuxoqXpEk8LNHa2MbhC6m-AQ/edit?usp=sharing")

dental <- as.vector(unlist(dental$'ICD_10_code'))

dental <- paste0( dental, collapse = "|^" )
dental <- paste0("^", dental)

################################################
# SEARCH THROUGH ALL DIAGNOSES COLUMNS AT ONCE:

#1. Squish all Diag columns into one column with multiple rows per patient:
scp_long <- scp %>% pivot_longer(starts_with("Diag"))
  # rename new column so it's easier to understand it's purpose
scp_long <- rename(scp_long, visit = ...1)

#2.To avoid counting one visit/patient as multiple visits if they have multiple 
  # ICD 10 codes in one visit, do the following...

# ACSC VISITS
acs_visit <- scp_long %>% 
        mutate(`acs?` = grepl(acs,value)) %>% # create T/F column to show if acs code is present.
        group_by(visit) %>% # group by visit so the next step can work.
        summarize(code_sum = sum(`acs?`), # create column to show total acs codes from each visit.
                  acs_YN = ifelse(code_sum > 0, "Yes", "No")) # create column to show if each visit had an acs code or not.

View(acs_visit %>% 
  group_by(acs_YN) %>%
  tally) # Look at how many visits were for acs conditions vs non acs conditions.
       
# NON-EMERGENGY VISITS
  # NOTE: we will primarily be looking at ACS visits, but this is good code to have if
  # we want to look at non-emergencies separately. It can be potentially misguiding to
  # look at non emergency codes b/c they could be present in a visit that also involved
  # emergency conditions or ACS conditions.
nonemerg_visit <- scp_long %>% 
       mutate(`nonemerg?` = grepl(non_emerg,value)) %>%
       group_by(visit) %>% 
       summarize(code_sum = sum(`nonemerg?`),
                  nonemerg_YN = ifelse(code_sum > 0, "Yes", "No")) 

View(nonemerg_visit %>% 
       group_by(nonemerg_YN) %>% 
       tally)

# MENTAL HEALTH VISITS
mental_visit <- scp_long %>% 
  mutate(`mental?` = grepl(mental,value)) %>%
  group_by(visit) %>% 
  summarize(code_sum = sum(`mental?`),
            mental_YN = ifelse(code_sum > 0, "Yes", "No")) 

View(mental_visit %>% 
       group_by(mental_YN) %>%
       tally)

# SUBSTANCE ABUSE VISITS
subabuse_visit <- scp_long %>% 
  mutate(`sub abuse?` = grepl(sub_abuse,value)) %>%
  group_by(visit) %>% 
  summarize(code_sum = sum(`sub abuse?`),
            subabuse_YN = ifelse(code_sum > 0, "Yes", "No")) 

View(subabuse_visit %>% 
       group_by(subabuse_YN) %>%
       tally)

# DENTAL VISITS
dental_visit <- scp_long %>% 
  mutate(`dental?` = grepl(dental,value)) %>%
  group_by(visit) %>% 
  summarize(code_sum = sum(`dental?`),
            dental_YN = ifelse(code_sum > 0, "Yes", "No"))

View(dental_visit %>% 
       group_by(dental_YN) %>%
       tally)

######################################
# SEARCH THROUGH ONLY THE PRIMARY DIAGNOSIS COLUMN (Diag1):

# How many hospital visits were for a primary diagnosis that was an acs condition?

# Create a new variable from 'scp' that only looks at 'Diag1' column and identifies
  # Whether or not the primary diagnosis was an acs conditon in a new column
  # named 'acsprim_YN'.
acs_primdiag <- scp %>%
  mutate(primary = grepl(acs, Diag1)) %>% 
  group_by(...1) %>% 
  summarize(code_sum = sum(primary),
            acsprim_YN = ifelse(code_sum > 0, "Yes", "No"))

  # Look at the number of primary diagnoses that are acsc.
View(test %>% 
       group_by(acsprim_YN) %>%
       tally)









