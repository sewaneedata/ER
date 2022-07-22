# Background tab

This tab explores the background research that influenced the creation of our project. It walks the user through 

- Background research on ER overuse in the United States with stats, 
- What ER overuse is, 
- Why we care about exploring ER overuse on the South Cumberland Plateau,
- What our partner wants to gain from our project, and
- Important vocabulary.

# Primary Findings tab

This tab explores some of our primary findings. It consists of four static graphs that explore the following:

- ER overuse on the South Cumberland Plateau,
- ER overuse in Williamson county, the "healthiest" county in Tennessee, verses on the SCP,
- ER overuse by insurance type, and
- Visits to the ER at different times of day for both "appropriate" and "overuse" reasons.

All static graphs are accompanied by summaries of what they show.

# Explore ER Overuse dropdown tab

**All instructions for how to operate the tools in these tabs are in the dashboard.**

This tab is a dropdown tab where the user can play around in the data.

The tabs included in here are as follows:

**Map:** Outlines the South Cumberland Plateau and expresses severity of ER overuse by zip code. Urgent cares in the area are indicated by red dots, while primary care doctors (ONLY) on the Plateau are indicated by green dots. Blue markers represent the top ten most visited ERs by residents from the Plateau.
The other map is interactive and shows the top three hospitals visited by residents from a specific zip code.

**Demographics:** This tab explores trends in ER use based on demographics of Sex, Race, and Age group. The user can choose to investigate trends in ER use based on these demographics by zip code, county, and insurance type.

**Types of Conditions:** This tab explores the rate at which residents of the SCP visit the ER for certain types of conditions. Types of conditions include:

- ACS, 
- non-emergent, 
- mental health, 
- dental, and
- substance abuse conditions.

**Top Diagnoses:** This tab explores the top 5 diagnoses seen at the ER in instances of overuse in different zip codes, counties, and by insurance type.

# About tab

This tab introduces the user to DataLab, the Fellows, and our partner, the South Cumberland Health Network (SCHN).

#
# How to run the dashboard
The following is a detailed breakdown on how to use this code to scrub, prepare, and analyze data using the scripts and dashboard created. 

##### NOTES: 
- This data is not available online due to patient privacy, for information about how to obtain the data used for this analysis, contact DataLab at datalab@sewanee.edu.  
- The data received by DataLab from the Department of Health was two separate files: Inpatient and Outpatient. DataLab administration conjoined the two data sets, removed patient personal information, and anonymized patient identifiers. This data analysis started from the conjoined Inpatient and Outpatient data that was already anomizied. To begin this data analysis, conjoin the two datasets and remove columns containing patient personal information.

## Instructions
1. Download conjoined, anonymized data (see note above) onto computer  
2. Download [RStudio](https://www.rstudio.com/products/rstudio/download/) onto computer  
3. Download all github files onto computer  
a. Go to [DataLab ER GitHub](https://github.com/sewaneedata/ER)
b. Click the green “Code” button  
c. Select “Download Zip” option  
4. Unzip GitHub download file  
5. Open RStudio  
6. Open ```data_scrub.R``` file  
a. Select the “File” option in the top left corner of the screen  
b. Select “Open File”  
c. Go to the unzipped GitHub file on your computer  
d. Select ```data_scrub.R``` file  
7. Add data file path into ```data_scrub.R``` where it says “discharges_phi”   
a. Lines to change in “Filter Zipcodes” section   
8. Run ```data_scrub.R``` 
a. Will result in ```SCP_data2.csv``` file  
9. Locate ```scp_data2.csv``` on computer  
a. Open file explorer  
b. Search for ```scp_data2.csv```  
c. Right click file  
d. Select “Copy File Path” option  
10. Open ```prep_data.R``` file in RStudio  
a. Search “INSERT FILE PATH HERE”  
b. Paste file path into line: ```scp <- readr::read_csv("INSERT FILE LOCATION HERE")```
11. Open ```overuse_TN.R``` in RStudio  
12. Add data file path into ```overuse_TN.R``` where it says “discharges_phi”   
a. Lines to change in “Filter ER” section  
13. Run ```overuse_TN.R``` code  
a. Will result in ```tn_conditions.csv``` file  
13. Locate ```tn_conditions.csv``` on computer  
a. Open file explorer  
14. Search for ```tn_conditions.csv```  
a. Right click file  
b. Select “Copy File Path” option  
15. Open ```prep_data.R``` file in RStudio  
a. Search “INSERT FILE LOCATION HERE”  
b. Paste file path into line: ```tn_diags <- readr::read_csv("INSERT FILE LOCATION HERE")```
16. Download [Zip Code file](https://www2.census.gov/geo/tiger/TIGER2019/ZCTA5/). 
a. Click the .zip file to download  
b. Find zipped file in file explorer  
c. Right click and select unzip file  
d. Open file  
e. Find the file ending in .shp  
f. Right click and select copy file path option  
17. Open ```prep_data.R``` file in RStudio  
a. Search “INSERT FILE LOCATION HERE”  
b. Paste file path into line:  ```zipcodes <- st_read("INSERT FILE LOCATION HERE")```
18. Run ```prep_data.R```  
19. Open ```app.R``` in RStudio  
a. Select the “File” option in the top left corner of the screen  
b. Select “Open File”  
d. Go to the unzipped GitHub file on your computer  
e. Select ER_Usage_Dash folder  
f. Select ```app.R```  
20. Run ```app.R``` and dashboard should appear  
