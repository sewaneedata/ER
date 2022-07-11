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

#################################
# Making hospital map

# 1. Find out top 3 hospitals for each zipcode in order to find lat/long
  # and create a google sheet of values
# a. Top 3 hospitals of each zip
scp %>% 
  filter(Patient_Zip == '37301') %>% 
  group_by(JARID) %>% 
  tally %>% 
  arrange(desc(n))

scp %>% 
  filter(Patient_Zip == '37305') %>% 
  group_by(JARID) %>% 
  tally %>% 
  arrange(desc(n))

scp %>% 
  filter(Patient_Zip == '37313') %>% 
  group_by(JARID) %>% 
  tally %>% 
  arrange(desc(n))

scp %>% 
  filter(Patient_Zip == '37339') %>% 
  group_by(JARID) %>% 
  tally %>% 
  arrange(desc(n))

scp %>% 
  filter(Patient_Zip == '37356') %>% 
  group_by(JARID) %>% 
  tally %>% 
  arrange(desc(n))

scp %>% 
  filter(Patient_Zip == '37365') %>% 
  group_by(JARID) %>% 
  tally %>% 
  arrange(desc(n))

scp %>% 
  filter(Patient_Zip == '37366') %>% 
  group_by(JARID) %>% 
  tally %>% 
  arrange(desc(n))

scp %>% 
  filter(Patient_Zip == '37374') %>% 
  group_by(JARID) %>% 
  tally %>% 
  arrange(desc(n))

scp %>% 
  filter(Patient_Zip == '37375') %>% 
  group_by(JARID) %>% 
  tally %>% 
  arrange(desc(n))

scp %>% 
  filter(Patient_Zip == '37383') %>% 
  group_by(JARID) %>% 
  tally %>% 
  arrange(desc(n))
scp %>% 
  filter(Patient_Zip == '37387') %>% 
  group_by(JARID) %>% 
  tally %>% 
  arrange(desc(n))

scp %>% 
  filter(Patient_Zip == '37397') %>% 
  group_by(JARID) %>% 
  tally %>% 
  arrange(desc(n))

# b. Read in googlesheet created with the above hospital JARIDs.
hosp_location <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1EHHze7ABHXTTDUvaxbAIz5efFT67MR1VMXrMSkokMQs/edit?usp=sharing")

###
# 2. Create new data frame called 'top_hosp' that only has the top three most visited
  # hospitals from each zipcode.
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

# c. Make JARID in top_hosp type double rather than character.
top_hosp <- top_hosp %>% 
  mutate(JARID = as.numeric(JARID))

###
# 3. Join 'top_hosp' with the google sheet of lat/long for each hospital using the 'JARID' column.
h <- inner_join(top_hosp, hosp_location, by = "JARID")

hospitals <- inner_join(h, zipcodes, by = "Patient_Zip")

### 
# 4. code for shiny app map
leaflet() %>% 
  addTiles() %>% 
  addMarkers(data = hospitals) %>% 
  addPolygons(data = zipcodes,
              color = '#253494',
              weight = 1,
              smoothFactor = 0.25,
              opacity = 1.0,
              fillOpacity = 0.5,
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 1.0,
                                                  bringToFront = TRUE),
              label = paste0("Zip code: ", hospitals$Patient_Zip))


zipcodes <- rename(zipcodes, lat = INTPTLAT10, lng = INTPTLON10)

