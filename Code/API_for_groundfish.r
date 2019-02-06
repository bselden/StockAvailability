## download and update FRAM data
rm(list=ls()) # clean workspace
DataLocation = "/Users/Nick.Tolimieri/Documents/Data/Groundfish Data/FRAM Survey/To 2018/"
#DataLocation = "/Users/Nick.Tolimieri/Desktop/"
setwd(DataLocation)

# IMPORT DATA FROM WEB PAGE ####
date = Sys.Date()
DestFile = paste(DataLocation,'Groundfish_',date,'.csv', sep='')

##### Combined Shelf and Slope Groundfish Trawl data 2003 to most recent #####
# "actual_station_design_dim$area_hectares"
# https://www.nwfsc.noaa.gov/data/api/v1/source/trawl.operation_haul_fact/variables
# "area_swept_ha_der"

ApiText = paste0("https://www.nwfsc.noaa.gov/data/api/v1/source/trawl.catch_fact/selection.csv",
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

df = data.frame(read.table(DestFile, sep=',', header=TRUE))

#### Download Trawl info #######
DestFile = paste(DataLocation,'Trawl_info_',date,'.csv', sep='')
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

