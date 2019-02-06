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

#### Download Trawl info #######
trawl.info.file = paste(DataLocation,'Trawl_info_',date,'.csv', sep='')
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
download.file(url=ApiText, destfile=trawl.info.file)


################################

# CONVERT TO WIDE FORMAT ####
# CORRECT NAMES etc

##### bring in data #### Currently off
# remember to update file name.  Date changes with download date.
rawdata = data.frame(read.table(DestFile, sep=",", header=TRUE))
min(rawdata$date_dim.year)
max(rawdata$date_dim.year)
as.factor(rawdata$date_dim.year)

# rawdata = data.frame(read.table("Groundfish_2017-10-10.csv", sep=",", header=TRUE))
colnames(rawdata)
dim(rawdata)
levels(as.factor(rawdata$project))
levels(as.factor(rawdata$date_dim.year))
# get just the NWFSC WCGF Trawl Data, just a double check ####
# rawdata = rawdata[rawdata$project=="Groundfish Slope and Shelf Combination Survey",]

# get just fish 
rawfish = rawdata[rawdata$species_category=='fish',]
# rawfish = rawdata[rawdata$species_category %in% c('fish','Fish','FISH'),]
#head(rawfish)

# get crabs 
rawcrabs = rawdata[rawdata$scientific_name %in% c('Cancer magister','Chionoecetes tanneri','Chionoecetes bairdi'),]
#head(rawcrabs)

# put back together but get rid of other inverts except crabs #### 
RawData = rbind(rawfish,rawcrabs)


# rename so don't change column titles below
colnames(RawData)
# reset some column names ####
cnam = c(
     'common_name',
     'cpue_kg',
     'cpue_no',
     'year',
     'date', # supposed to import but doesn't
     'depth_m',
     'lat_dd',
     'lon_dd',
     'partition',
     'perf',
     'project',
     "scientific_name",
     "species_category",
     "species_subcategory",
     'no',
     'kg',
     "trawl_id",
     "vessel",
     'yr_invalid')
# double check the names are correct #
cbind(colnames(RawData),cnam)
colnames(RawData) <- cnam

# rename file 
fish <- RawData
fish$spp = as.character(fish$scientific_name)
# get spp category table 
cat.table = aggregate(no ~ spp + species_category + species_subcategory, data=fish, FUN=sum)
write.table(cat.table, "T_species_category_table.csv", sep=',', row.names = FALSE, col.names = TRUE)
# check and fix some names ####
levels(as.factor(fish$spp))
fish$spp <- ifelse(fish$spp == "Bathyraja kincaidii (formerly B. interrupta)", "Bathyraja kincaidii" ,as.character(fish$spp))
fish$spp <- ifelse(fish$spp == "Sebastes sp. (Vermilion And Sunset)", "Sebastes crocotulus.miniatus" ,as.character(fish$spp))
fish$spp <- ifelse(fish$spp == "Sebastes sp. (miniatus / crocotulus)", "Sebastes crocotulus.miniatus" ,as.character(fish$spp))
fish$spp <- ifelse(fish$spp == "Sebastes melanostictus or Sebastes aleutianus", "Sebastes melananostictus.aleutianus",as.character(fish$spp))
fish$spp <- ifelse(fish$spp == "Sebastes sp. (aleutianus / melanostictus)", "Sebastes melananostictus.aleutianus",as.character(fish$spp))
fish$spp <- ifelse(fish$spp == "",as.character(fish$spp.cn),fish$spp)
fish$spp <- ifelse(fish$spp == "Lampanyctus ritteri", "Nannobrachium ritteri" ,as.character(fish$spp))
fish$spp <- ifelse(fish$spp == "Platytoctes apus", "Platytroctes apus" ,as.character(fish$spp))
fish$spp <- ifelse(fish$spp == "Caristius.marcropus", "Caristius.macropus" ,as.character(fish$spp))
fish$spp <- ifelse(fish$spp == "Chaenophryne longicepts", "Chaenophryne longiceps" ,as.character(fish$spp))
fish$spp <- ifelse(fish$spp == "Lumpenus maculatus", "Leptoclinus maculatus" ,as.character(fish$spp))

## change some partition labels ####
# change some names
levels(as.factor(fish$partition))
fish$partition = as.character(fish$partition)
fish$partition[fish$partition=="Adipose Fin Present, Returned Live"]<- 'Adi.Live'
fish$partition[fish$partition=="Adipose Fin Present, Returned Dead"]<- 'Adi.Dead'
fish$partition[fish$partition=="Adult, Adipose Fin Absent, Returned Dead"]<-'Adult.noAdi.Dead'
fish$partition[fish$partition== "Adult, Adipose Fin Absent, Returned Live" ]<-'Adult.noAdi.Live'
fish$partition[fish$partition=="Adult, Adipose Fin Present, Returned Dead"]<-'Adult.Adi.Dead'
fish$partition[fish$partition=="Adult, Adipose Fin Present, Returned Live"]<-'Adult.Adi.Live'
fish$partition[fish$partition=="Subadult, Adipose Fin Absent, Returned Dead"]<-'Sub.noAdi.Dead'
fish$partition[fish$partition=="Subadult, Adipose Fin Absent, Returned Live"]<-'Sub.noAdi.Live'
fish$partition[fish$partition== "Subadult, Adipose Fin Present, Returned Dead"]<-'Sub.Adi.Dead'
fish$partition[fish$partition=="Subadult, Adipose Fin Present, Returned Live"]<-'Sub.Adi.Live'

# spp or fish identifier ####
fish$taxa = "xxxxx"
fish$taxa[grep(" ", fish$spp, fixed=TRUE)] = "species"
fish$taxa[grep("sp.", fish$spp, fixed=TRUE)] = "genus"
fish$taxa[grep("spp.", fish$spp, fixed=TRUE)] = "genus"
fam = as.character(substr(fish$spp,(nchar(fish$spp)-3),nchar(fish$spp)))
fish$taxa[grep("idae", fam, fixed=TRUE)] = "family"  # find words ending in 'idae'
fish$taxa[grep("idae ", fish$spp)] = "family"  # find 'idae' at end of a word within a space after
fish$taxa[grep("inae", fam, fixed=TRUE)] = "tribe"  # find words ending in 'idae'
fish$taxa[grep("unident.", fish$spp, fixed=TRUE)] = "family"
fish$taxa = ifelse(fish$spp=="Liparidae n. gen. (Orr)","family",as.character(fish$taxa))
fish$taxa[grep("iformes", fish$spp)] = "order"
fish$taxa[grep('Selachii.sharks',fish$spp)] = 'family'
fish$species_category = ifelse(fish$spp%in% c("Cancer magister", "Chionoecetes tanneri", "Chionoecetes bairdi")=="TRUE",'crab',as.character(fish$species_category))

### rename some spp to include eggs, yoy etc. based on Partition #####
#head(fish)
levels(as.factor(fish$partition))
# levels(as.factor(fish$spp))
# fish$partition[is.na(fish$partition)==TRUE] <- -9999
x = grep('YOY',fish$partition)
fish$spp[x] <- paste(fish$scientific_name[x],'yoy',sep='.')
x = grep('Large',fish$partition)
fish$spp[x] <- paste(fish$scientific_name[x],'large',sep='.')
x = grep('Small',fish$partition)
fish$spp[x] <- paste(fish$scientific_name[x],'small',sep='.')
x = grep('Eggs',fish$partition)
fish$spp[x] <- paste(fish$scientific_name[x],'egg',sep='.')
x = grep('Egg Cases',fish$partition)
fish$spp[x] <- paste(fish$scientific_name[x],'egg.case',sep='.')
x = grep('Larvae',fish$partition)
fish$spp[x] <- paste(fish$scientific_name[x],'larvae',sep='.')
x = grep('Oncorhynchus tshawytscha',fish$scientific_name)
fish$spp[x] <- paste(fish$scientific_name[x],fish$partition[x],sep='.')

levels(as.factor(fish$spp))
# check names and levels ####
nam <- data.frame(levels(as.factor(fish$spp))) 
colnames(nam)<- "name"
nam$tax <- fish$taxa[match(nam$name, fish$spp)]
nam$species_category <- fish$species_category[match(nam$name, fish$spp)]


# check for spp with multiple entries ####
# delete, sum and add back in
xxx = fish
xxx$n = 1
x1 = aggregate(n ~ trawl_id + spp, data=xxx, FUN=sum)
x2 = x1[x1$n>1,]
x2 = x2[order(x2$trawl_id),]
rmx = paste(x2$spp,x2$trawl_id)
rmf = paste(fish$spp,fish$trawl_id)
duplicates = fish[rmf %in% rmx,]
duplicates = duplicates[order(duplicates$trawl_id),]
duplicates[,1:15]
write.csv(duplicates,"Spp_duplicates_no.partition.csv")

######## DELETE DUPLICATES not sure why they are in there ############
# only a couple in any given year #

fish = fish[!(rmf %in% rmx),]

###############################################

#############################
## END corrections #####
#############################

#replace any NA/empty values in the number or kg column.  These are no data NOT zeros.
# they will be turned into NAs later.

# still includes trawls from closed areas #####
maxyear = max(fish$year)
write.table(fish,paste("D_GF.data.long.1998-",maxyear,".withNA.csv", sep=""), sep=",", col.names=TRUE, row.names=FALSE)

# need -9999 to keep actual NAs separate from zeros when changing to WIDE format.  
fish$no[is.na(fish$no)] <- -9999
fish$kg[is.na(fish$kg)] <- -9999
fish$cpue_no[is.na(fish$cpue_no)] <- -9999
fish$cpue_kg[is.na(fish$cpue_kg)] <- -9999
write.table(fish,paste("D_GF.data.long.1998-",maxyear,".csv", sep=""), sep=",", col.names=TRUE, row.names=FALSE)

#########################################################
###########       TRANSPOSE TO WIDE   ##################
#########################################################

#### remove trawls from closed areas ####
fish <- fish[is.na(fish$yr_invalid)==TRUE,]
#### remove Unsatisfactory trawls ####
fish <- fish[fish$perf=="Satisfactory",]

fish$part2 = 'yes'
fish$part2[fish$partition %in% c('Eggs','Egg Cases','Larvae')] <- 'no'

data.name = paste("D_GroundFish_2003-",max(rawdata$date_dim.year,na.rm = TRUE), sep="")

# output all forms of the 
for(i in 1:12){
# select data and name output ####
     print(i)
     if(i==1){out.put.name = paste(data.name, "_fish.crab.spp.cpue.kg.csv",sep="")
          fish01 <- fish[ fish$tax=='species' & 
                         fish$species_category %in% c('fish','crab') &
                         fish$part2=='yes',]     
          fish02 <- fish01[,c("spp","trawl_id","cpue_kg")]
          fish.wide = reshape(fish02, direction="wide", timevar="spp", idvar="trawl_id", v.name="cpue_kg")
       }    
     if(i==2){out.put.name = paste(data.name, "_fish.crab.spp.cpue.no.csv",sep="")
     
          fish01 <- fish[ fish$tax=='species' & 
                     fish$species_category %in% c('fish','crab') &
                     fish$part2=='yes',]     
          fish02 <- fish01[,c("spp","trawl_id","cpue_no")]
          fish.wide = reshape(fish02, direction="wide", timevar="spp", idvar="trawl_id", v.name="cpue_no")
     } 
     if(i==3){out.put.name = paste(data.name, "_fish.crab.spp.kg.csv",sep="")
          fish01 <- fish[ fish$tax=='species' & 
                     fish$species_category %in% c('fish','crab') &
                     fish$part2=='yes',]     
          fish02 <- fish01[,c("spp","trawl_id","kg")] 
          fish.wide = reshape(fish02, direction="wide", timevar="spp", idvar="trawl_id", v.name="kg")
     }    
     if(i==4){out.put.name =paste(data.name, "_fish.crab.spp.no.csv",sep="")
     
          fish01 <- fish[ fish$tax=='species' & 
                          fish$species_category %in% c('fish','crab') &
                     fish$part2=='yes',]     
          fish02 <- fish01[,c("spp","trawl_id","no")]
          fish.wide = reshape(fish02, direction="wide", timevar="spp", idvar="trawl_id", v.name="no")
     }     
     
     if(i==5){out.put.name = paste(data.name, "_fish.crab.spp.hake.chin.cpue.kg.csv",sep="")
          fish01 <- fish[ fish$tax=='species' & 
                          fish$species_category %in% c('fish','crab') &
                          fish$part2=='yes',]     
          fish02 <- fish01[,c("spp","trawl_id","cpue_kg")]
          fish.wide = reshape(fish02, direction="wide", timevar="spp", idvar="trawl_id", v.name="cpue_kg")
     }    
     if(i==6){out.put.name = paste(data.name, "_fish.crab.spp.hake.chin.cpue.no.csv",sep="")
     
          fish01 <- fish[ fish$tax=='species' & 
                          fish$species_category %in% c('fish','crab') &
                     fish$part2=='yes',]     
          fish02 <- fish01[,c("spp","trawl_id","cpue_no")]
          fish.wide = reshape(fish02, direction="wide", timevar="spp", idvar="trawl_id", v.name="cpue_no")
     } 
     if(i==7){out.put.name = "D_GF9815_fish.crab.spp.hake.chin.kg.csv"
          fish01 <- fish[ fish$tax=='species' & 
                          fish$species_category %in% c('fish','crab') &
                          fish$part2=='yes',]     
          fish02 <- fish01[,c("spp","trawl_id","kg")] 
          fish.wide = reshape(fish02, direction="wide", timevar="spp", idvar="trawl_id", v.name="kg")
     }    
     if(i==8){out.put.name = paste(data.name, "_fish.crab.spp.hake.chin.no.csv",sep="")
     
          fish01 <- fish[ fish$tax=='species' & 
                          fish$species_category %in% c('fish','crab') &
                          fish$part2=='yes',]     
          fish02 <- fish01[,c("spp","trawl_id","no")]
     fish.wide = reshape(fish02, direction="wide", timevar="spp", idvar="trawl_id", v.name="no")
     }       
     if(i==9){out.put.name = paste(data.name, "_all.cpue.kg.csv",sep="")
          fish01 <- fish[fish$species_category %in% c('fish','crab'),]     
          fish02 <- fish01[,c("spp","trawl_id","cpue_kg")]
          fish.wide = reshape(fish02, direction="wide", timevar="spp", idvar="trawl_id", v.name="cpue_kg")
     }    
     if(i==10){out.put.name = paste(data.name, "_all.cpue.no.csv",sep="")
     
          fish01 <- fish[fish$species_category %in% c('fish','crab'),]     
          fish02 <- fish01[,c("spp","trawl_id","cpue_no")]
          fish.wide = reshape(fish02, direction="wide", timevar="spp", idvar="trawl_id", v.name="cpue_no")
     } 
     if(i==11){out.put.name = paste(data.name, "_all.kg.csv",sep="")
          fish01 <- fish[fish$species_category %in% c('fish','crab'),]     
          fish02 <- fish01[,c("spp","trawl_id","kg")] 
          fish.wide = reshape(fish02, direction="wide", timevar="spp", idvar="trawl_id", v.name="kg")
     }    
     if(i==12){out.put.name = paste(data.name, "_all.no.csv",sep="")
     
          fish01 <- fish[fish$species_category %in% c('fish','crab'),]     
          fish02 <- fish01[,c("spp","trawl_id","no")]
          fish.wide = reshape(fish02, direction="wide", timevar="spp", idvar="trawl_id", v.name="no")
     }    
     
 
     # correct some names ####
     nam01 <- colnames(fish.wide)
     # remove the 'cpue.kg'(9) or 'kg' (4) before species names in columns and replace
     # first correct for kg then redo if cpue is at the beginning.  Will give error if cpue isn't in the metric term but this is ok.
     if(length(grep('cpue',colnames(fish.wide)))==0){nam02 <- substr(nam01,4,nchar(nam01))}else{nam02 <- substr(nam01,9,nchar(nam01))}

     nam02[1]= "trawl_id"
     colnames(fish.wide) <- nam02
     
     # replace NAs with zero's for fish not caught in that trawl
     fish.wide[is.na(fish.wide)] <-0
     #replace no data with NA
     #fish.wide[fish.wide<0] <-"NA"
     # head(fish.wide)
     
     #### sum up yoy for hake and groups for salmon ####
     if(i %in% c(1:4)){
     
          mp = grep("Merluccius productus", colnames(fish.wide))
          hake = fish.wide[,mp]
          hake[hake== -9999] <- NA
          fish.wide = fish.wide[,-mp]
          tothake = rowSums(hake, na.rm=TRUE)
          fish.wide$Merluccius.productus = tothake
     
          ot = grep("Oncorhynchus tshawytscha", colnames(fish.wide))
          chin = fish.wide[,ot]
          chin[chin == -9999] <- NA
          fish.wide = fish.wide[,-ot]
          totchin = rowSums(chin, na.rm=TRUE)
          fish.wide$Oncorhynchus.tshawytscha = totchin
     }
     
     #### put back trawl information ####
     trawl = fish.wide[,1]
     fishXX = fish.wide[,-1]
     fishORD = fishXX[,order(colnames(fishXX))]
     fish.wide = cbind(trawl,fishORD)
     colnames(fish.wide)[1] = 'trawl_id'
     # bring back in trawl information ####
     # problem with date is controlled here ####
     if(length(grep('date',colnames(fish)))==0){trawl.info = data.frame(aggregate(trawl_id ~ depth_m+lat_dd+lon_dd+year, data=fish, FUN=mean))}else{
       trawl.info = data.frame(aggregate(trawl_id ~ depth_m+lat_dd+lon_dd+year+date, data=fish, FUN=mean))
       }
     trawl.info$perf = fish$perf[match(trawl.info$trawl_id,fish$trawl_id)]
     trawl.info$vessel = fish$vessel[match(trawl.info$trawl_id,fish$trawl_id)]
     trawl.info$swept.area.ha = fish$swept_area[match(trawl.info$trawl_id,fish$trawl_id)]
     
     head(trawl.info)
     
     fish.info = data.frame(fish.wide$trawl_id)
     colnames(fish.info) = 'trawl_id'
     fish.info$year = trawl.info$year[match(fish.info$trawl_id, trawl.info$trawl_id)]
     fish.info$lat_dd = trawl.info$lat_dd[match(fish.info$trawl_id, trawl.info$trawl_id)]
     fish.info$lon_dd = trawl.info$lon_dd[match(fish.info$trawl_id, trawl.info$trawl_id)]
     fish.info$depth = trawl.info$depth_m[match(fish.info$trawl_id, trawl.info$trawl_id)]
     fish.info$perf = trawl.info$perf[match(fish.info$trawl_id, trawl.info$trawl_id)]
     fish.info$vessel = trawl.info$vessel[match(fish.info$trawl_id, trawl.info$trawl_id)]
     fish.info$swept.area.ha = trawl.info$swept.area.ha[match(fish.info$trawl_id, trawl.info$trawl_id)]
     fish.info$END = 'END'
     
     GF.data.wide = data.frame(cbind(fish.info,fish.wide[,-1]))
     colnames(GF.data.wide)[1] <- 'trawl.id'
     
     # change any -9999 back to NAs so they don't get summed up.
     GF.data.wide[GF.data.wide == -9999] <- NA
     
    #### output data ####
     write.table(GF.data.wide,out.put.name,col.names = TRUE,row.names = FALSE, sep = ',')     
}


################




