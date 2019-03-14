### Objective: Add zeros for hauls that occurred where species were absent
library(tidyverse)
library(lubridate)

### Read in catch data
catch <- read_csv(paste0("Data/", list.files("Data", pattern="Groundfish_all")))

### Read in haul data
haul <- read_csv(paste0("Data/", list.files("Data", pattern="Trawl_info_all")))



### Add in column for area_swept_km and day of year
#wctri_hauls_spp_zeros2[,"Date":=as.Date(gsub(" .*$", "", START_TIME), format='%m/%d/%Y')]
#wctri_hauls_spp_zeros2[,"Numerical_calendar_date":=yday(Date)]
haul2 <- haul %>%
  mutate(Numerical_calendar_date=yday(ymd(date_yyyymmdd)),
         Year=year(ymd(date_yyyymmdd)),
         AreaSwept_km2=0.01*area_swept_ha_der)


### Add zeros function
add.zeros.sp <- function(bio.dt, haul.dt){
  bio.dt$pres <- bio.dt$total_catch_wt_kg > 0
  
  ### Merge bio with hauls so that every haul is repeated
  #spp <- sort(unique(bio.dt$scientific_name))
  spp <- data.frame(scientific_name=unique(bio.dt$scientific_name), common_name=unique(bio.dt$common_name))
  
  print("Step1: Create master haul_spp df")
  
  hauls <- unique(haul.dt$trawl_id)
  
  haul.spp.master <- expand.grid(trawl_id=hauls, scientific_name=spp$scientific_name)
  haul.spp.master2 <- merge(haul.spp.master, spp, by=c("scientific_name"))
  
  ### Merge back with haul info to get those haul meta data associated with file
  haul.spp <- merge(haul.dt, haul.spp.master2, all.y=T, by="trawl_id")
  
  
  cols.shared <- colnames(bio.dt)[colnames(bio.dt)%in%colnames(haul.spp)]
  
  # ### Merge with mean biomass to get those data associated
  # haul.spp.mean.bio <- merge(haul.spp, mean.bio.yr, by=c("spp", "year"))
  print("Step2: Merge with catch bio and add zeros")
  ### Merge with biomass 
  bio.haul.zeros <- merge(haul.spp, bio.dt, 
                          by=cols.shared, all.x=T)
  
  ##
  
  ### Make pres FALSE where NA
  bio.haul.zeros$pres2 <- as.logical(ifelse(is.na(bio.haul.zeros$pres), "FALSE", bio.haul.zeros$pres))
  
  ### Make NAs for absences into zero biomass
  bio.haul.zeros$total_catch_wt_kg <- ifelse(is.na(bio.haul.zeros$total_catch_wt_kg), 0, bio.haul.zeros$total_catch_wt_kg)
  bio.haul.zeros$total_catch_numbers <- ifelse(is.na(bio.haul.zeros$total_catch_numbers), 0, bio.haul.zeros$total_catch_numbers)
  
  

  return(bio.haul.zeros)
}

catch.zeros <- as_tibble(add.zeros.sp(bio.dt=catch, haul.dt=haul2))

vast.catch <- catch.zeros %>%
  transmute(Trawl.Identifier=trawl_id,
            Year=Year,
            Longitude=longitude_dd,
            Latitude=latitude_dd,
            Survey=ifelse(project=="Groundfish Triennial Shelf Survey","Tri.Shelf",
                          ifelse(project=="Groundfish Slope and Shelf Combination Survey", "WCGBTS", NA)),
            Numerical_calendar_date=Numerical_calendar_date,
            Catch_KG=total_catch_wt_kg,
            AreaSwept_km2=AreaSwept_km2,
            Species=common_name)

#Check that same number of hauls in processed files
length(unique(haul$trawl_id))
length(unique(vast.catch$Trawl.Identifier))

### Check that each haul has lat and lon
sum(is.na(vast.catch$Longitude))
sum(is.na(vast.catch$Latitude))

#Test subset to one species
filter(vast.catch, Species=="sablefish")


save(vast.catch, file="Data/catch_for_vast.RData")

