library(data.table)
library(tidyverse)
library(Hmisc)
library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(maptools)
library(PBSmapping)

# ### Plot code from VAST (from FishStatsUtils)
# source("Code/VAST_plot_data.R")
# source("Code/VAST_plot_maps.R")
# 
# 
# DateFile = paste0(getwd(),'/VAST_output/')
# dir.create(DateFile)

# ===================
# = State Shapefile =
# ===================

states <- readOGR("GIS_Data/cb_2016_us_state_500k", "cb_2016_us_state_500k")
states.data <- states@data

proj.states <- proj4string(states)

states.wcoast <- states[states$NAME %in% c("Washington", "California", "Oregon"),]


# ===================
# =  Spatial Info from VAST =
# ===================

# extrap_l <- readRDS("Data/VAST_spatial_info/sable_Extrapolation_List.rds")
# 
# spatial_l <- readRDS("Data/VAST_spatial_info/sable_Spatial_List.rds")

### Load in all R objects from sablefish VAST run from VAST_output_sablefish_n500_2019-04-02
load("Data/VAST_output_sablefish/SableAll.RData", verbose=T)



extrap_loc <- data.table(Northin_m=Extrapolation_List$Data_Extrap$Northin_m,
                         Easting_m=Extrapolation_List$Data_Extrap$Easting_m,
                         Lat=Extrapolation_List$Data_Extrap$Lat,
                         Lon=Extrapolation_List$Data_Extrap$Lon,
                         propInTriennial=Extrapolation_List$Data_Extrap$propInTriennial,
                         loc=seq(1,length(Extrapolation_List$Data_Extrap$Northin_m)))

spatial_knot <- data.table(knot_num=Spatial_List$NN_Extrap$nn.idx[,1], 
                           loc=seq(1, length(Spatial_List$NN_Extrap$nn.idx)))

extrap_knot <-merge(extrap_loc, spatial_knot, by=c("loc"))
extrap_knot[,"E_km":=Easting_m/1000]
extrap_knot[,"N_km":=Northin_m/1000]
                    

knot_locs <- as.data.table(read.csv("Data/VAST_knots_1April19/Knot_area_km2_sablefish.csv"))
knot_locs[,"knot_num":= seq(1,500)]
# knot_locs[,"N_km_adj":=N_km - min(N_km)] #subtract minimum northing to get on same scale as extrap
# knot_locs[,"E_km_adj":=E_km - min(E_km)] #subtract minimum easting to get on same scale as extrap
# ### Didn't quite work, but could just use the lat and lon in the extrap instead


attr(knot_locs, "projection") <- "UTM"
attr(knot_locs, "zone") <- 10
knot_locs$X <- knot_locs$E_km
knot_locs$Y <- knot_locs$N_km
knot_locs_LL <- rename(convUL(knot_locs), Lon=X, Lat=Y)

knot_locs_LL[knot_num==284]
extrap_knot[knot_num==284] #Lat and Lon still not corresponding, so don't use

dens_df <- as.data.table(read.csv("Data/VAST_density_1April19/Dens_DF_sablefish.csv"))


plot(Lat ~ Lon, extrap_knot, cex=0.1, col="gray")
points(Lat ~ Lon, extrap_knot[propInTriennial>0], cex=0.01, col="black")
plot(states.wcoast, add=T)
points(Lat ~ Lon, knot_locs_LL[Area_km2==0], col="red", cex=0.2)
points(Lat ~ Lon, knot_locs_LL[Area_km2>0], col="yellow", cex=0.2)


plot(N_km ~E_km, extrap_knot, col="red")
points(N_km_adj ~ E_km_adj, knot_info)
