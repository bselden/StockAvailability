## download and update FRAM data
rm(list=ls()) # clean workspace
DataLocation = "Data/" #Location in git repo
#setwd(DataLocation) #this gets set automatically with StockAvailability.Rproj

# Query example from FRAM website
# https://www.nwfsc.noaa.gov/data/api/v1/source/trawl.catch_fact/selection.json?filters=field_identified_taxonomy_dim$scientific_name=Eopsetta%20jordani,date_dim$year>=2010,date_dim$year<=2012
# &variables=date_yyyymmdd,field_identified_taxonomy_dim$scientific_name
# filters=MyFieldName|=["321"%2C"987"] - matches any records with an exact value of either 321 or 987.


### Select the species you wish to download data for
DTSPL_spp <- c("Anoplopoma fimbria", "Eopsetta jordani", "Microstomus pacificus", "Ophiodon elongatus", "Sebastolobus alascanus", "Sebastolobus altivelis")

### Convert species vector into form that can be queried with API
API_spp <-gsub(" ", "%20", DTSPL_spp)
API_spp_list <- paste0(",field_identified_taxonomy_dim$scientific_name|=",
                       '["',
                       paste(API_spp,  collapse=paste0('"%2C"')),
                       '"]')

# IMPORT DATA FROM WEB PAGE ####
# https://www.nwfsc.noaa.gov/data/api/v1/source/trawl.catch_fact/variables
# "actual_station_design_dim$area_hectares"
# https://www.nwfsc.noaa.gov/data/api/v1/source/trawl.operation_haul_fact/variables
# "area_swept_ha_der" #in Trawl_info
date = Sys.Date()

##### Download both triennial and annual simultaneously #####
project_list <- paste0(",project|=",
                       '["',
                       "Groundfish%20Slope%20and%20Shelf%20Combination%20Survey",
                       '"%2C"',
                       "Groundfish%20Triennial%20Shelf%20Survey",
                       '"]')


DestFile = paste(DataLocation,'Groundfish_all',date,'.csv', sep='')

### Create API string for query
ApiText = paste0("https://www.nwfsc.noaa.gov/data/api/v1/source/trawl.catch_fact/selection.csv",
                 "?filters=performance=Satisfactory",
                 project_list,
                 API_spp_list,
                 # ",date_dim$year>=1980,date_dim$year<=2018", #use to limit dates desired
                 "&variables=project,",
                 "trawl_id,",
                 "date_dim$year,",
                 "date_yyyymmdd,",
                 "vessel,",
                 "performance,",
                 "year_stn_invalid,",
                 "depth_m,",
                 "latitude_dd,",
                 "longitude_dd,",
                 "scientific_name,",
                 "common_name,",
                 "species_category,",
                 "species_subcategory,",
                 "partition,",
                 "total_catch_numbers,",
                 "total_catch_wt_kg,",
                 "cpue_kg_per_ha_der,",
                 "cpue_numbers_per_ha_der,",
                 "actual_station_design_dim$area_hectares")

### Download catch data from API
download.file(url=ApiText, destfile=DestFile)


df = data.frame(read.table(DestFile, sep=',', header=TRUE))

#### Download Trawl info #######
DestFile = paste(DataLocation,'Trawl_info_all',date,'.csv', sep='')
ApiText = paste0("https://www.nwfsc.noaa.gov/data/api/v1/source/trawl.operation_haul_fact/selection.csv",
                 "?filters=performance=Satisfactory",
                 project_list,
                 "&variables=project,",
                 "trawl_id,",
                 "date_dim$year,",
                 "date_yyyymmdd,",
                 "vessel,",
                 "performance,",
                 "year_stn_invalid,",
                 "depth_m,latitude_dd,",
                 "longitude_dd,",
                 "partition,",
                 "area_swept_ha_der")
download.file(url=ApiText, destfile=DestFile)




##### Combined Shelf and Slope Groundfish Trawl data 2003 to most recent #####
# https://www.nwfsc.noaa.gov/data/api/v1/source/trawl.catch_fact/variables
# "actual_station_design_dim$area_hectares"
# https://www.nwfsc.noaa.gov/data/api/v1/source/trawl.operation_haul_fact/variables
# "area_swept_ha_der" #in Trawl_info

DestFile = paste(DataLocation,'Groundfish_Annual',date,'.csv', sep='')


### Create API string for query
ApiText = paste0("https://www.nwfsc.noaa.gov/data/api/v1/source/trawl.catch_fact/selection.csv",
                 "?filters=project=Groundfish%20Slope%20and%20Shelf%20Combination%20Survey",
                  API_spp_list,
                 # ",date_dim$year>=1980,date_dim$year<=2018", #use to limit dates desired
                 "&variables=project,",
                 "trawl_id,",
                 "date_dim$year,",
                 "date_yyyymmdd,",
                 "vessel,",
                 "performance,",
                 "year_stn_invalid,",
                 "depth_m,",
                 "latitude_dd,",
                 "longitude_dd,",
                 "scientific_name,",
                 "common_name,",
                 "species_category,",
                 "species_subcategory,",
                 "partition,",
                 "total_catch_numbers,",
                 "total_catch_wt_kg,",
                 "cpue_kg_per_ha_der,",
                 "cpue_numbers_per_ha_der,",
                 "actual_station_design_dim$area_hectares")

### Download catch data from API
download.file(url=ApiText, destfile=DestFile)


df = data.frame(read.table(DestFile, sep=',', header=TRUE))

#### Download Trawl info #######
DestFile = paste(DataLocation,'Trawl_info_annual',date,'.csv', sep='')
ApiText = paste0("https://www.nwfsc.noaa.gov/data/api/v1/source/trawl.operation_haul_fact/selection.csv",
                 "?filters=project=Groundfish%20Slope%20and%20Shelf%20Combination%20Survey",
                 "&variables=project,",
                 "trawl_id,",
                 "date_dim$year,",
                 "date_yyyymmdd,",
                 "vessel,",
                 "performance,",
                 "year_stn_invalid,",
                 "depth_m,latitude_dd,",
                 "longitude_dd,",
                 "partition,",
                 "area_swept_ha_der")
download.file(url=ApiText, destfile=DestFile)


##### GROUNDFISH TRIENNIAL SHELF SURVEY #######
DestFile = paste0(DataLocation,'Triennial_',date,'.csv')
ApiText = paste0("https://www.nwfsc.noaa.gov/data/api/v1/source/trawl.catch_fact/selection.csv",
                 "?filters=project=Groundfish%20Triennial%20Shelf%20Survey",
                 API_spp_list,
                 "&variables=project,",
                 "trawl_id,",
                 "date_dim$year,",
                 "date_yyyymmdd,",
                 "vessel,",
                 "performance,",
                 "year_stn_invalid,",
                 "depth_m,latitude_dd,",
                 "longitude_dd,",
                 "scientific_name,",
                 "common_name,",
                 "species_category,",
                 "species_subcategory,",
                 "partition,",
                 "total_catch_numbers,",
                 "total_catch_wt_kg,",
                 "cpue_kg_per_ha_der,",
                 "cpue_numbers_per_ha_der")
download.file(url=ApiText, destfile=DestFile)

# triennial info
DestFile= paste0(DataLocation,'Triennial_info_',date,'.csv')
ApiText = paste0("https://www.nwfsc.noaa.gov/data/api/v1/source/trawl.operation_haul_fact/selection.csv",
                 "?filters=project=Groundfish%20Triennial%20Shelf%20Survey",
                 "&variables=project,",
                 "trawl_id,",
                 "date_dim$year,",
                 "date_yyyymmdd,",
                 "vessel,",
                 "performance,",
                 "year_stn_invalid,",
                 "depth_m,latitude_dd,",
                 "longitude_dd,",
                 "partition,",
                 "area_swept_ha_der")
download.file(url=ApiText, destfile = DestFile)

