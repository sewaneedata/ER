# THIS IS OUR SHARED SCRIPT THAT INCLUDES ALL HELPFUL CODE NEEDED TO SET UP DATA-MINING
################
# THIS IS NOT OUR DELIVERABLE CODE
################

# RUN LINES 9 THRU 169 EVERYDAY BEFORE DATA MINING...
# THE OTHER LINES ARE OPTIONAL, HELPFUL CODE
################
# libraries:
library(dplyr)
library(readr)
library(tidyverse)
library(ggplot2)
library(gsheet)
library(sf)
library(leaflet)
library(raster)

# Read in: scp_data data frame
#ELLIE: 
scp <- readr::read_csv("Dropbox/DATALAB/ER_Project/scp_data2")
#JENNA: scp <- readr::read_csv("C:/Users/jplus/OneDrive/Documents/DataLab/ER_Usage/scp_data2")

# rename weird column
scp <- rename(scp, visit = ...1)
  
# filter down to ER visits  
scp <- scp %>%
  filter(ER_Record_Flag == "Y")
  
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

#################################################
# Run the following for an updated 'scp' data frame that has:
# 1. new columns that indicate whether or not the primary diagnosis was ACSC, mental, dental, etc.,
# 2. a new column called 'Age_Group' that sorts patients into groups by decade,
# 3. removes N/As for Patient_Sex.
# 4. a new column "county" for county names.

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
                                    '50-59', '60-69', '70-79', '80-89', '90-99')),
         county = ifelse(Patient_Zip %in% grundy_zip, 
                         "Grundy", 
                         ifelse( Patient_Zip %in% franklin_zip,
                                 "Franklin",
                                 ifelse(Patient_Zip %in% marion_zip,
                                        "Marion",
                                        "NULL"))))

# Then, run the following to create a new column "county_total" that will track
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







#####################
# Code for Map
#####################
# Read in the shape file (remember to run ALL libraries at top of page)
zipcodes <- st_read("Dropbox/DATALAB/er_project/tl_2019_us_zcta510/tl_2019_us_zcta510.shp")

# NOTE: the name of your file will change depending on where the shape file is 
  # on your computer. So the "___" will change, but keep the name of the variable as
  # "zipcodes" so that it matches the 

# filter down to only include SCP zip codes
zipcodes <- zipcodes %>% 
  filter(ZCTA5CE10 %in% c("37301","37305","37313","37339","37356","37365", 
                          "37366","37374","37375","37383","37387","37397"))


# Want to join zipcodes with scp data so I can display scp data on the map:
#
# 1. Create a new variable with Patient_Zip and the number of cases for each condition 
# in that zip.

# a. Create new column in 'scp' called 'zip_total' that counts up total # of ER visits
# in each zip so we can create reproducible percentages.
zip_visits <- scp %>%
  group_by(Patient_Zip) %>%
  tally()

scp <- inner_join(scp, zip_visits, by = 'Patient_Zip') %>%
  dplyr::rename(zip_total = 'n')

# b. Use new column 'zip_total' to create new variable 'scp_map' with percentages.
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

###
# 2. Rename the column in "zipcodes" that holds zip codes so I can join "zipcodes" 
# with "scp_map" by "Patient_Zip".
zipcodes <- rename(zipcodes, Patient_Zip = ZCTA5CE10)
###
# 3. In 'scp_map', combine rows with "37375" and "37383" zip codes because there needs to be the
# same number of rows in "scp_map" and  "zipcodes" in order to join() them.

# a. Add the two rows together
# scp_map[9,] <- scp_map[9,] + scp_map[10,] 

# b. Delete the row "[10]" that I just added to "[9]"
scp_map <- scp_map[-10,]

# c. Rename the row "[9]" because it named it by adding up the zip codes.
# scp_map$Patient_Zip[scp_map$Patient_Zip == 74758] <- 37375
###
# 4. Join 'scp_map' and 'zipcodes' now that they have the same # of rows.

# a. Convert 'Patient_Zip' in 'zipcodes' from type character to type double/numeric 
# so that it matches 'Patient_Zip' in 'scp_map'.
zipcodes <- zipcodes %>% 
  mutate(Patient_Zip = as.numeric(Patient_Zip))

# b. Join 'zipcodes' and 'scp_map' to make a new varible called 'map'!
combine <- left_join(zipcodes, scp_map, by = "Patient_Zip")

combine <- combine %>% 
  mutate(acs_perc = as.numeric(acs_perc))

write.csv(combine[,-15], "zips_for_map")

# Now, can create map.
map <- leaflet(combine)

# Creating a palette to shade in the zip codes
pal <- colorNumeric(palette = c('#0571b0','#92c5de',  '#f7f7f7', '#f4a582', '#ca0020'), 
                    domain = map$acs_perc)




######################################
######################################
######################################
# The following has been commented out because it's not necessary code
# but may potentially be useful in the future.
######################################
# SEARCH THROUGH ONLY THE PRIMARY DIAGNOSIS COLUMN (Diag1):
######################################
# Will use the  'scp' data frame for this (it has all diag columns separated).

# What percentage of all visits to the ER had a primary diagnosis of ACSC?
#perc_acsc <- scp %>% 
  #group_by(acs_primary) %>%
  #tally %>% 
  #mutate(total = sum(n)) %>%
  #group_by(acs_primary, n, total) %>% 
  #summarise(perc = n/total*100)
    # The column name in group_by() can be replaced to see trends for other conditions
      # (non emergent, mental, dental, substance abuse, etc.)
      # example below shows non emergent trends

################################################
# IF NEEDED, the following allows you to search thru all diag columns at once:
  # Code has been commented out
################################################
#1. Squish all Diag columns into one column with multiple rows per patient:
  # scp_long <- scp %>% pivot_longer(starts_with("Diag"))

# rename new column so it's easier to understand it's purpose
  # scp_long <- rename(scp_long, visit = ...1)

#2.To avoid counting one visit/patient as multiple visits if they have multiple 
# ICD 10 codes in one visit, do the following...

# ACSC VISITS:
# acs_visit <- scp_long %>% 
        # mutate(`acs?` = grepl(acs,value)) %>% # create T/F column to show if acs code is present.
        # group_by(visit) %>% # group by visit so the next step can work.
        # summarize(code_sum = sum(`acs?`), # create column to show total acs codes from each visit.
                  # acs_YN = ifelse(code_sum > 0, "Yes", "No")) # create column to show if each visit had an acs code or not.

# View(acs_visit %>% group_by(acs_YN) %>% tally) # Look at how many visits were for acs conditions vs non acs conditions.
       
# NON-EMERGENGY VISITS
  # NOTE: we will primarily be looking at ACS visits, but this is good code to have if
  # we want to look at non-emergencies separately. It can be potentially misguiding to
  # look at non emergency codes b/c they could be present in a visit that also involved
  # emergency conditions or ACS conditions.

# nonemerg_visit <- scp_long %>% 
       # mutate(`nonemerg?` = grepl(non_emerg,value)) %>%
       # group_by(visit) %>% 
       # summarize(code_sum = sum(`nonemerg?`),
                  # nonemerg_YN = ifelse(code_sum > 0, "Yes", "No")) 

# View(nonemerg_visit %>% 
       # group_by(nonemerg_YN) %>% 
       # tally)

# MENTAL HEALTH VISITS

# mental_visit <- scp_long %>% 
  # mutate(`mental?` = grepl(mental,value)) %>%
  # group_by(visit) %>% 
  # summarize(code_sum = sum(`mental?`),
            # mental_YN = ifelse(code_sum > 0, "Yes", "No")) 
# View(mental_visit %>% 
       # group_by(mental_YN) %>%
       # tally)

# SUBSTANCE ABUSE VISITS
# subabuse_visit <- scp_long %>% 
  # mutate(`sub abuse?` = grepl(sub_abuse,value)) %>%
  # group_by(visit) %>% 
  # summarize(code_sum = sum(`sub abuse?`),
            # subabuse_YN = ifelse(code_sum > 0, "Yes", "No")) 

# View(subabuse_visit %>% 
       # group_by(subabuse_YN) %>%
       # tally)

# DENTAL VISITS
# dental_visit <- scp_long %>% 
  # mutate(`dental?` = grepl(dental,value)) %>%
  # group_by(visit) %>% 
  # summarize(code_sum = sum(`dental?`),
            # dental_YN = ifelse(code_sum > 0, "Yes", "No"))

# View(dental_visit %>% 
       # group_by(dental_YN) %>%
       # tally)