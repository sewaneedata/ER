################################################################################
#Data Scrub -----

# Description: 
# This file contains the code in which was used to scrub the inital database to 
# include specific rows and columns and generate the CSV file for data analysis. 
# The code to select patients from certian zipcodes was written using SQL.
# R was used to then scrub the columns in which were not needed for data analysis. 
# Run this code on the combined inpatient and outpatient 2019 TN hospital discharge data to 
# prepare it for data analysis. 
################################################################################

####################################
# PACKAGE INSTALL ------
#Install all packages needed to use dashboard for all scripts
# NOTE: Only need to run once
install.packages(c("shinyjs", "leaflet", "DT", "ggplot2","dplyr","readr","shinydashboard","markdown","bslib","shiny","RSQLite","tidyverse", "SF","leaflet","raster","gsheet"))
####################################

# Library
library(RSQLite)

#######################
# FILTER ZIP CODES ------
# SQLite
#######################

# Read in inital database
portaldb <- dbConnect(SQLite(), 'discharges_phi')

# List tables in database
dbListTables(portaldb)

# Column names
dbListFields(portaldb, 'discharges_phi')

# Query description
# Select which rows to keep based on patient zip codes
res<-"SELECT * FROM discharges_phi WHERE Patient_Zip IN ('37301','37305', '37313', '37339', '37356','37365', '37366', '37374', '37375', '37383', '37387', '37397')"

# Run query
ex<-dbGetQuery(portaldb, res)

# Disconnect from sqlite
dbDisconnect(portaldb)

#############################
# COLUMN SCRUB ------
# R
##############################

#Libraries
library(dplyr)
library(tidyverse)

# First Batch of columns to remove
scp <- select (ex,-c(Data_Yr, Bill_Number, Record_Seq_Num, Form_Type, Fed_Tax_SubID,
                    Fed_Tax_Num, Do_Not_Resuscitate, Accident_St, Rev_Cd1,
                    Rev_Cd2, Rev_Cd3, Rev_Cd4, Rev_Cd5, Rev_Cd6, Rev_Cd7, Rev_Cd8, 
                    Rev_Cd9, Rev_Cd10, Rev_Cd11, Rev_Cd12, Rev_Cd13, Rev_Cd14, Rev_Cd15, 
                    Rev_Cd16,Rev_Cd17, Rev_Cd18, Rev_Cd19, Rev_Cd20, Rev_Cd21, Rev_Cd22,
                    Rev_Cd23, HCPC_Rate_HIPPS_Rate_Cd1, HCPC_Rate_HIPPS_Rate_Cd2, 
                    HCPC_Rate_HIPPS_Rate_Cd3, HCPC_Rate_HIPPS_Rate_Cd4, HCPC_Rate_HIPPS_Rate_Cd5,
                    HCPC_Rate_HIPPS_Rate_Cd6, HCPC_Rate_HIPPS_Rate_Cd7, HCPC_Rate_HIPPS_Rate_Cd8, 
                    HCPC_Rate_HIPPS_Rate_Cd9, HCPC_Rate_HIPPS_Rate_Cd10, HCPC_Rate_HIPPS_Rate_Cd11,
                    HCPC_Rate_HIPPS_Rate_Cd12, HCPC_Rate_HIPPS_Rate_Cd13, HCPC_Rate_HIPPS_Rate_Cd14,
                    HCPC_Rate_HIPPS_Rate_Cd15, HCPC_Rate_HIPPS_Rate_Cd16, HCPC_Rate_HIPPS_Rate_Cd17,
                    HCPC_Rate_HIPPS_Rate_Cd18, HCPC_Rate_HIPPS_Rate_Cd19, HCPC_Rate_HIPPS_Rate_Cd20,
                    HCPC_Rate_HIPPS_Rate_Cd21, HCPC_Rate_HIPPS_Rate_Cd22, HCPC_Rate_HIPPS_Rate_Cd23, 
                    Units_Service1, Units_Service2, Units_Service3, Units_Service4, Units_Service5, 
                    Units_Service6, Units_Service7, Units_Service8, Units_Service9, Units_Service10, 
                    Units_Service11, Units_Service12, Units_Service13, Units_Service14, Units_Service15, 
                    Units_Service16, Units_Service17, Units_Service18, Units_Service19, Units_Service20,
                    Units_Service21, Units_Service22, Units_Service23, Tot_Chrg_by_Rev_Cd1, 
                    Tot_Chrg_by_Rev_Cd2, Tot_Chrg_by_Rev_Cd3, Tot_Chrg_by_Rev_Cd4, Tot_Chrg_by_Rev_Cd5,
                    Tot_Chrg_by_Rev_Cd6, Tot_Chrg_by_Rev_Cd7, Tot_Chrg_by_Rev_Cd8, Tot_Chrg_by_Rev_Cd9, 
                    Tot_Chrg_by_Rev_Cd10, Tot_Chrg_by_Rev_Cd11, Tot_Chrg_by_Rev_Cd12, Tot_Chrg_by_Rev_Cd13, 
                    Tot_Chrg_by_Rev_Cd14, Tot_Chrg_by_Rev_Cd15, Tot_Chrg_by_Rev_Cd16, Tot_Chrg_by_Rev_Cd17, 
                    Tot_Chrg_by_Rev_Cd18, Tot_Chrg_by_Rev_Cd19, Tot_Chrg_by_Rev_Cd20, Tot_Chrg_by_Rev_Cd21, 
                    Tot_Chrg_by_Rev_Cd22, Tot_Chrg_by_Rev_Cd23, Non_Cvrd_Chrg_by_Rev_Cd1, Non_Cvrd_Chrg_by_Rev_Cd2,
                    Non_Cvrd_Chrg_by_Rev_Cd3, Non_Cvrd_Chrg_by_Rev_Cd4, Non_Cvrd_Chrg_by_Rev_Cd5, Non_Cvrd_Chrg_by_Rev_Cd6, 
                    Non_Cvrd_Chrg_by_Rev_Cd7, Non_Cvrd_Chrg_by_Rev_Cd8, Non_Cvrd_Chrg_by_Rev_Cd9, Non_Cvrd_Chrg_by_Rev_Cd10,
                    Non_Cvrd_Chrg_by_Rev_Cd11, Non_Cvrd_Chrg_by_Rev_Cd12, Non_Cvrd_Chrg_by_Rev_Cd13, Non_Cvrd_Chrg_by_Rev_Cd14, 
                    Non_Cvrd_Chrg_by_Rev_Cd15, Non_Cvrd_Chrg_by_Rev_Cd16, Non_Cvrd_Chrg_by_Rev_Cd17, Non_Cvrd_Chrg_by_Rev_Cd18, 
                    Non_Cvrd_Chrg_by_Rev_Cd19, Non_Cvrd_Chrg_by_Rev_Cd20, Non_Cvrd_Chrg_by_Rev_Cd21, Non_Cvrd_Chrg_by_Rev_Cd22,
                    Non_Cvrd_Chrg_by_Rev_Cd23, Primary_Health_Plan_Id, Secondary_Health_Plan_Id, Tertiary_Health_Plan_Id,
                    National_Provider_Id, Dx_Px_Qualifier, Admit_Diag_Cd, POA1, POA2, POA3, POA4, POA5, POA6, POA7, POA8,
                    POA9, POA10, POA11, POA12, POA13, POA14, POA15, POA16, POA17, POA18, Prospect_Pay_Code,
                    Inpatient_Flag, ASTC_Flag, Obs_Unit_Flag, ER_Flag, Lithotripsy_Flag, PET_Flag, MRI_MRA_Flag,
                    Megavolt_Rad_Flag, CT_Flag, Fatal_Error_Flag, Bill_End, MUL, Record_Num1, Tot_Charges_Recorded,
                    Tot_Charges_Analysis, LOS, Record_Num2, DRG_Rank, Inpat_Record_Flag, ASTC_Record_Flag, Obs_23hr_Record_Flag,
                    CON_Flag, Cumulative_Record_Flag, Amount_Counter, Reportable_Flag, Hospital_Id_JAR, MS_DRG,
                    MS_DRG_4digit, HAC, CostWt, Admit_From_ED_Flag, Wrong_Claim, Tot_Charges_Summed, Admit_Diag_Cd,
                    Payer_A, Payer_B, Payer_C))
#Batch 2
scp <- select (scp,-c(Attend_MD, Attend_MD_TN_Lic_Num, Attend_MD_UPIN,
                    Operate_MD, Other_Prov_MD1, Other_Prov_MD2, Other_Prov_MD_TN_Lic_Num1,
                    Other_Prov_MD_TN_Lic_Num2, Other_Prov_MD_UPIN1, Other_Prov_MD_UPIN2,
                    Primary_Insr_Group_Num, Secondary_Insr_Group_Num, Tertiary_Insr_Group_Num,
                    Infant_Age_Months))

#Batch 3      
scp <- select (scp,-c(Accident_Code, Operate_MD_TN_Lic_Num,Operate_MD_UPIN)) 

# Batch 4 - all columns containing...
scp <- select(scp, -starts_with(c('Ecode', 'E_POA', 'Proc')))

#Create New CSV File of this Data
write.csv(scp, "scp_data2", row.names = TRUE)

