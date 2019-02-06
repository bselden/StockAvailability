# Stock Availability

## Workflow
1. API_for_groundfish_DTSPL.R  
Download catch and haul data from FRAM for selected species for both annual and triennial surveys. Data saved as csv in Data/
2. Hauls_add_zeros_wc.R  
Use the haul and catch info from the API code to generate a file with zeros for all hauls conducted but species are not observed. Column names are then harmonized with what the VAST code is expecting, and data is saved as .RData in Data/
