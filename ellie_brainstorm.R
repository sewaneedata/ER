# Preliminary investigation of shrunken data set
#########################################
library(dplyr)
library(readr)
library(tidyverse)
library(ggplot2)
library(gsheet)

# scp_data set
scp <- readr::read_csv("scp_data2")

# See "shared_help_code.R" for vectors and necessary code to set up data mining.

########################################
# Making a Pie chart
# 1. ACS conditions
  # table
perc_acsc <- scp %>% 
  group_by(acs_primary) %>%
  tally %>% 
  mutate(total = sum(n)) %>%
  group_by(acs_primary, n, total) %>% 
  summarise(perc = n/total*100)

  # pie chart
x <- c(78.1, 21.9)

pie(x,
    labels = paste0(x, "%"), 
    col = c('light blue', 'red'),
    radius = .9,
    main = "Primary Diagnoses at the ER")
legend("bottomleft", legend = c('Other', 'ACSC'),
       fill =  c("light blue", "red"), title = "Condition")

#############
# Trends in Race
# table
race <- scp %>%
  filter(Race_Chr == c("White", "Black", "Native American")) %>% 
  group_by(Race_Chr, 
           acs_primary, 
           nonemerg_primary, 
           mental_primary, 
           dental_primary, 
           subabuse_primary) %>%
  tally %>%
  ungroup()%>%
  group_by(Race_Chr) %>% 
  summarise(percentage = (n/sum(n))*100, across(everything())) %>%
  mutate( type = case_when( !acs_primary & !nonemerg_primary & !mental_primary & !dental_primary & !subabuse_primary ~ "Other",
                            dental_primary ~ "Dental",
                            acs_primary ~ "ACS",
                            subabuse_primary ~ "Substance Abuse",
                            mental_primary ~ "Mental Health",
                            nonemerg_primary ~ "Non emergent"))

ggplot(data = race, aes(x = type, y = percentage, fill = type)) +
  geom_col() +
  labs(title = "Trends") +
  theme(legend.position = 'bottom') +
  scale_fill_discrete(name = "Condition") +
  facet_wrap(~Race_Chr) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

ggplot(data = race, aes(x = condition, y = percentage, fill = type)) +
  geom_col() +
  labs(title = "Trends") +
  theme(legend.position = 'bottom') +
  scale_fill_discrete(name = "Condition") +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
###############
# Code for Map

#libraries
library(sf)
library(leaflet)
library(raster)
library(httr)
library(jsonlite)
library(htmltools)

zipcodes <- st_read("Dropbox/DATALAB/er_project/tl_2019_us_zcta510/tl_2019_us_zcta510.shp")

# filter down to only include SCP zipcodes
zipcodes <- zipcodes %>% 
  filter(ZCTA5CE10 %in% c("37301",
                        "37305", 
                        "37313", 
                        "37339", 
                        "37356", 
                        "37365", 
                        "37366",
                        "37374",
                        "37375",
                        "37383",
                        "37387",
                        "37397"))

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


