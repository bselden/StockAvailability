# devtools::install_github("james-thorson/VAST")

library(TMB)               # Can instead load library(TMBdebug)
library(VAST)

Version = get_latest_version( package="VAST" )

#### Set Parameters and species ##########

spp = c("Dover sole", "lingcod","longspine thornyhead",
        "petrale sole","sablefish","shortspine thornyhead")

# Thorson settings for 2016 paper
#FieldConfig = c("Omega1"=0, "Epsilon1"=1, "Omega2"=0, "Epsilon2"=1)
#RhoConfig = c("Beta1"=0, "Beta2"=0, "Epsilon1"=2, "Epsilon2"=2)
#ObsModel = c(1,0)

SPECIES = spp[5]
print(SPECIES)
Method = c("Grid", "Mesh", "Spherical_mesh")[2]
grid_size_km = 25
n_x = 250   # Specify number of stations (a.k.a. "knots")
FieldConfig = c("Omega1"=0, "Epsilon1"=1, "Omega2"=0, "Epsilon2"=1)
RhoConfig = c("Beta1"=0, "Beta2"=0, "Epsilon1"=2, "Epsilon2"=2) # 2= random walk
OverdispersionConfig = c("Eta1"=0, "Eta2"=0)
ObsModel = c(1,0) # lognormal and logit 
Options =  c("SD_site_density"=FALSE, "SD_site_logdensity"=FALSE, "Calculate_Range"=TRUE, "Calculate_effective_area"=TRUE)

# Default
strata.limits <- data.frame('STRATA'="All_areas")

#RootDir = "/Users/Nick.Tolimieri/Documents/VAST_Becca/"
#setwd(RootDir)
RootDir <- paste0(getwd(), "/Data/")
DateFile = paste0(RootDir,'VAST_output_',SPECIES,'/')
  dir.create(DateFile)

Record = ThorsonUtilities::bundlelist( c("Version",
                                         "Method",
                                         "grid_size_km",
                                         "n_x",
                                         "FieldConfig",
                                         "RhoConfig",
                                         "OverdispersionConfig",
                                         "ObsModel",
                                         "Options") )

save( Record, file=file.path(DateFile,"Record.RData"))
capture.output( Record, file=paste0(DateFile,"Record.txt"))

##################
# ADD NEW DATA
##################

load( paste0(RootDir,"catch_for_vast.RData") ) # vast.catch

vast.catch <- as.data.frame(vast.catch) #Becca needed this for Data_Geostat to work properly

# vast.catch = vast.catch[order(vast.catch$Year, vast.catch$Numerical_calendar_date),]
# vast.catch = vast.catch[order(vast.catch$Year),]

# Change column names
Data_Geostat = data.frame( "Species"=vast.catch[,'Species'], 
                           "Catch_KG"=vast.catch[,'Catch_KG'], 
                           "Year"=vast.catch[,'Year'], 
                           "Vessel"="missing",
                           "Date" = vast.catch[,'Numerical_calendar_date'],
                           "AreaSwept_km2"=vast.catch[,"AreaSwept_km2"], 
                           "Lat"=vast.catch[,'Latitude'], 
                           "Lon"=vast.catch[,'Longitude'], 
                           "Survey"=vast.catch[,'Survey'] )

# Kludge to fix later
Data_Geostat[,'AreaSwept_km2'] = ifelse( is.na(Data_Geostat[,'AreaSwept_km2']), 0.02, Data_Geostat[,'AreaSwept_km2'] )
Data_Geostat = na.omit( Data_Geostat )

# Catchability matrix
Q_ik = ThorsonUtilities::vector_to_design_matrix( Data_Geostat[,'Survey'] )[,-1,drop=FALSE]

# Choose species
Which2Keep = which( Data_Geostat[,'Species']==SPECIES )
Data_Geostat = Data_Geostat[ Which2Keep, ]
Q_ik = Q_ik[ Which2Keep, , drop=FALSE ]
Q_ik = cbind(Q_ik, scale(Data_Geostat[,"Date"])) # survey and date as catchability coefficients

##################
# END NEW DATA
##################

#Extrapolation_List = make_extrapolation_info( Region="California_current", strata.limits=strata.limits, surveyname='propInWCGBTS' )
Extrapolation_List = make_extrapolation_info( Region="California_current", 
                                              strata.limits=strata.limits, 
                                              surveyname='propInTriennial' )

Spatial_List = make_spatial_info( grid_size_km=grid_size_km, 
                                  n_x=n_x, 
                                  Method=Method, 
                                  Lon=Data_Geostat[,'Lon'], 
                                  Lat=Data_Geostat[,'Lat'], 
                                  Extrapolation_List=Extrapolation_List, 
                                  DirPath=DateFile, 
                                  Save_Results=FALSE )
# Add knots to Data_Geostat
Data_Geostat = cbind( Data_Geostat, "knot_i"=Spatial_List$knot_i )

# Format data
TmbData = VAST::make_data("Version"=Version, 
                  "Q_ik"=Q_ik, 
                  "FieldConfig"=FieldConfig, 
                  "OverdispersionConfig"=OverdispersionConfig, 
                  "RhoConfig"=RhoConfig, 
                  "ObsModel"=ObsModel, 
                  "c_i"=rep(0,nrow(Data_Geostat)), 
                  "b_i"=Data_Geostat[,'Catch_KG'], 
                  "a_i"=Data_Geostat[,'AreaSwept_km2'], 
                  "v_i"=as.numeric(Data_Geostat[,'Vessel'])-1, 
                  "s_i"=Data_Geostat[,'knot_i']-1, 
                  "t_i"=Data_Geostat[,'Year'], 
                  "a_xl"=Spatial_List$a_xl, 
                  #"X_xtp" = X_xtp, # density coveriates, 3d array
                  "MeshList"=Spatial_List$MeshList, 
                  "GridList"=Spatial_List$GridList, 
                  "Method"=Spatial_List$Method,
                  "Options"=Options )

TmbList = VAST::make_model("TmbData"=TmbData, 
                       "RunDir"=DateFile, 
                       "Version"=Version, 
                       "RhoConfig"=RhoConfig, 
                       "loc_x"=Spatial_List$loc_x, 
                       "Method"=Method)
Obj = TmbList[["Obj"]]

Opt = TMBhelper::Optimize( obj=Obj, 
                           lower=TmbList[["Lower"]], 
                           upper=TmbList[["Upper"]], 
                           getsd=TRUE, savedir=DateFile, 
                           bias.correct=FALSE, 
                           newtonsteps=1, 
                           bias.correct.control=list(sd=FALSE, split=NULL, nsplit=1, vars_to_correct="Index_cyl") )

Report = Obj$report()
Save = list("Opt"=Opt, "Report"=Report, "ParHat"=Obj$env$parList(Opt$par), "TmbData"=TmbData)
save(Save, file=paste0(DateFile,"Save.RData"))

# Get region-specific settings for plots
MapDetails_List = make_map_info( "Region"="California_current", "NN_Extrap"=Spatial_List$PolygonList$NN_Extrap, "Extrapolation_List"=Extrapolation_List )
# Decide which years to plot                                                   
Year_Set = seq(min(Data_Geostat[,'Year']),max(Data_Geostat[,'Year']))
Years2Include = which( Year_Set %in% sort(unique(Data_Geostat[,'Year'])))

# Plot density maps
Dens_xt = plot_maps(plot_set=c(3), MappingDetails=MapDetails_List[["MappingDetails"]], Report=Report, Sdreport=Opt$SD, PlotDF=MapDetails_List[["PlotDF"]], MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=DateFile, Year_Set=Year_Set, Years2Include=Years2Include, Rotate=MapDetails_List[["Rotate"]], Cex=MapDetails_List[["Cex"]], Legend=MapDetails_List[["Legend"]], zone=MapDetails_List[["Zone"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), cex=1.8, plot_legend_fig=FALSE)

# Extract densities
Dens_DF = cbind( "Density"=as.vector(Dens_xt), "Year"=Year_Set[col(Dens_xt)], "E_km"=Spatial_List$MeshList$loc_x[row(Dens_xt),'E_km'], "N_km"=Spatial_List$MeshList$loc_x[row(Dens_xt),'N_km'] )

# Area and location for each knot
area.per.knot = cbind( "Area_km2"= Spatial_List$a_xl[,1], Spatial_List$loc_x )


x = data.frame(area.per.knot)
x$pch = ifelse(x$Area_km2>0,19,1)


graphics.off()
png("knot_locations.png")
par(mfrow = c(1,1))
plot(x$E_km, x$N_km, pch=x$pch)
dev.off()



write.table(Dens_DF, paste0(DateFile,"Dens_DF_",SPECIES, ".csv"),col.names=TRUE, sep=',', row.names = FALSE)
write.table(area.per.knot, paste0(DateFile,"Knot_area_km2_",SPECIES, ".csv"),col.names=TRUE, sep=',', row.names = FALSE)

