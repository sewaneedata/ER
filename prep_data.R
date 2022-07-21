################################################################################
# PREP DATA -----

# Description: 
# This script is used to prepare the code environment in order to run the shiny app. 
# Run the entire code in this script before running the shiny app script. 

# Use:
# Search for "INSERT FILE LOCATION HERE" across this script and insert the file locations
# on your computer for the specified CSV. 

################################################################################

  # Libraries:
  library(dplyr)
  library(readr)
  library(tidyverse)
  library(ggplot2)
  library(gsheet)
  library(sf)
  library(leaflet)
  library(raster)
  
  ######################################
  # DATA PREP -----
  ######################################
  
  #Read in the SCP_data2 file created in data_scrub.R
  scp <- readr::read_csv("INSERT FILE LOCATION HERE")
  
  # Rename ...1 column
  scp <- rename(scp, visit = ...1)
    
  # Filter to just ER visits  
  scp <- scp %>%
    filter(ER_Record_Flag == "Y")
    
  ######################################
  # ACSC ICD-10 CODES VECTOR -----
  ######################################
  
  # 1. ACSC codes vector: 
  # NOTE: dental conditions are included in ACSC code vector, but we also have a separate
    # vector of just dental codes so we can look at those separately.
    
    # Create an object out of google sheet with codes
    acs <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/14fZ-1PFInHdL8OIaLUSHhc7ulFwSf9i9O32SPitCkLU/edit?usp=sharing")  
  
    # Create a vector out of the object
    acs <- as.vector(unlist(acs$'ICD_10_code'))
  
    # Make vector readable by grepl():
    acs <- paste0( acs, collapse = "|^" ) 
      # NOTES for why I did this ^ :
        # "collapse" squishes the vector into "___ or ___ or ___.." statement that grepl() can read. 
        # "^" tells R to search for anything that IS or STARTS WITH the value that it precedes. 
  
  acs <- paste0("^", acs) # This adds the "^" to the first value
  
  ######################################
  # NON EMERGENT ICD-10 CODES VECTOR -----
  ######################################
  # Do the exact same as we did for ACSC codes vector
  non_emerg <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/14m7RGYPh17lv1EQ5Tv9gbpj1kvzlVmeUQRZs0MkUyUw/edit#gid=0")
  
  non_emerg <- as.vector(unlist(non_emerg$'ICD_10_code'))
  
  non_emerg <- paste0( non_emerg, collapse = "|^" )
  
  non_emerg <- paste0("^", non_emerg)
  
  ######################################
  # MENTAL HEALTH ICD-10 CODES VECTOR -----
  ######################################
    # NOTE: substance abuse codes are included in "mental" vector, but we also have a separate
    # vector of just sub abuse codes so we can look at those separately.
    
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
  
    # nPrepare for grepl
  mental <- paste0( mental, collapse = "|^" )
  mental <- paste0("^", mental)
  
  
  ######################################
  # SUBSTANCE ABUSE ICD-10 CODES VECTOR -----
  ######################################
  sub_abuse <- c()
  
  for(code in 11:19){
    new_value <- paste('F', as.character(code), sep = "")
    sub_abuse <- c(sub_abuse, new_value)
  }
  
  sub_abuse <- paste0( sub_abuse, collapse = "|^" )
  sub_abuse <- paste0("^", sub_abuse)
  
  ######################################
  # DENTAL ICD-10 CODES VECTOR -----
  ######################################
    # same process as ACSC and NON EMERGENT vectors
    # these are included in the acsc vector, but this vector allows us to look only at dental services.
  dental <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1m2ifrfx9yczoLSTzlOwOuxoqXpEk8LNHa2MbhC6m-AQ/edit?usp=sharing")
  
  dental <- as.vector(unlist(dental$'ICD_10_code'))
  
  dental <- paste0( dental, collapse = "|^" )
  dental <- paste0("^", dental)
  
  ######################################
  # INSURANCE TYPE VECTORS-----
  ######################################
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
  
  # Create a new column with Insurance names
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
  
  
  ######################################
  # PREP DATA cont. -----
  ######################################
  
  # COUNTY: Assign county to patients ----
  #########################################
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
                                      '50-59', '60-69', '70-99', '70-99', '70-99')), #
           county = ifelse(Patient_Zip %in% grundy_zip,
                           "Grundy",
                           ifelse( Patient_Zip %in% franklin_zip,
                                   "Franklin",
                                   ifelse(Patient_Zip %in% marion_zip,
                                          "Marion",
                                          "NULL"))))
  scp$age_group <- as.character(scp$age_group)
  
  
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
  
  # RACE: Format to Text-----
  ###############################
    # Next, run the following to add a new column that says Race in characters rather
    # than in values:
  
    # Make a vector of names (in order, 1 = white, 2 = Black, etc.)
  races_vec <- c("White", "Black", "Native American") 
  
    # Create the new column called 'Race_Chr'
  scp <- scp %>% 
    mutate(Race_Chr = ifelse(Race == 9, "Unkown", races_vec[Race]))
  
  ######################################
  # FINDINGS COUNTY vs WILLIAMSON Graph -----
  ######################################
  # 1. Run overuse_TN on inital dataset, download tn_conditions.csv to computer
    # NOTE: running overuse_TN.R creates tn_conditions.csv
  
  # 2. Paste file location of tn_conditions.csv inbetween the ""
  tn_diags <- readr::read_csv("INSERT FILE LOCATION HERE")
  
  #3. Filter out "other" conditions
  tn_diags <- tn_diags %>% filter(county != "Other", Condition != "Other")
  
  ######################################
  # OVERUSE ON MAP CODE -----
  ######################################
  # 1. Download zipcode shape files 
  # (Located in the github wiki, under data description and dictionary)
  
  # 2.Read in the shape file ending in .shp ONLY (remember to run ALL libraries at top of page)
  zipcodes <- st_read("INSERT FILE LOCATION HERE")
  # NOTE: the name of your file will change depending on where the shape file is 
    # on your computer. So the "___" will change, but keep the name of the variable as
    # "zipcodes" so that it matches the rest of the code/shiny app.
  
  # 3. Filter down wanted zip codes
  zipcodes <- zipcodes %>% 
    filter(ZCTA5CE10 %in% c("37301","37305","37313","37339","37356","37365", 
                            "37366","37374","37375","37383","37387","37397"))
  
  
  # Show data analysis on map
  ##############################
  # Want to join 'zipcodes' with 'scp' data so I can display scp data on the map:
  # 1. Create a new variable with "Patient_Zip" and the number of cases for each condition 
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
  
  # 2. Rename the column in "zipcodes" that holds zip codes so I can join "zipcodes" 
  # with "scp_map" by "Patient_Zip".
  zipcodes <- rename(zipcodes, Patient_Zip = ZCTA5CE10)
  
  # 3. In 'scp_map', delete the row "[10]"  (it's zip should be 37383)
  scp_map <- scp_map[-10,]
  
  # 4. Join 'scp_map' and 'zipcodes' now that they have the same # of rows.
  
  # a. Convert 'Patient_Zip' in 'zipcodes' from type character to type double/numeric 
  # so that it matches 'Patient_Zip' in 'scp_map'.
  zipcodes <- zipcodes %>% 
    mutate(Patient_Zip = as.numeric(Patient_Zip))
  
  # b. Join 'zipcodes' and 'scp_map' to make a new varible called 'map'!
  combine <- left_join(zipcodes, scp_map, by = "Patient_Zip")
  
  # 5. Creating a palette to shade in the zip codes
  pal <- colorNumeric(palette = c('#0571b0','#92c5de',  '#f7f7f7', '#f4a582', '#ca0020'), 
                      domain = combine$input$cond)
  
  ######################################
  # STATIC MAP CODE -----
  ######################################
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
  
  ######################################
  # INTERACTIVE MAP CODE -----
  ######################################
  # 1. Read in Google sheet of hospital lat/long information:
  hosp_location <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1EHHze7ABHXTTDUvaxbAIz5efFT67MR1VMXrMSkokMQs/edit?usp=sharing")

  # 2. Create new data frame called 'top_hosp' that only has the top 3 most visited
  # hospitals from each zip code.
  # a. Arrange most visited hospitals in descending order and make a new variable.
  
  test <- scp %>% 
    group_by(Patient_Zip, JARID) %>%
    tally %>%
    arrange(desc(n))
  
  # b. Use the above variable to create a new data frame with top 3 hospitals in each zip.  
  top_hosp <- Reduce(rbind,
                     by(test,
                        test["Patient_Zip"],
                        head,
                        n = 3))
  
  # c. Make JARID in 'top_hosp' type double rather than type character.
  top_hosp <- top_hosp %>% 
    mutate(JARID = as.numeric(JARID))

  # 3. Join 'top_hosp' with 'hosp_location' using the 'JARID' column.
  h <- inner_join(top_hosp, hosp_location, by = "JARID")
  
  # 4. Join varible made above (h) with 'zipcodes' variable (from other map code).
  hospitals <- inner_join(h, zipcodes, by = "Patient_Zip")
