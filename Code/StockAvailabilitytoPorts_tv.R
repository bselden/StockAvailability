### Objective calculate stock availability to select ports

library(tidyverse)
library(Hmisc)
library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(maptools)

### Read in port locations
port_locs<- read_csv("Data/PacFIN & BEF ports LUT.csv")


#############
### Subset to desired ports based on PACFIN port code
ports_lim <- c("MRO", "BRG", "COS", "AST")
port_names_touse <- c("Morro Bay", "Fort Bragg", "Coos Bay", "Astoria")

port_names_df <- data.frame(Pcid=ports_lim, Port_Name=port_names_touse)

port_locs_lim <- port_locs %>%
  inner_join(port_names_df, by=c("Pcid"))%>%
  transmute(X=Lon, Y=Lat, Port_Name=Port_Name, port=Pcid, Lon=Lon, Lat=Lat)



#############
### Convert port location to northings and eastings
library(PBSmapping)
attr(port_locs_lim, "projection") <- "LL"
port_locs_utm <- rename(convUL(port_locs_lim), E_km_port=X, N_km_port=Y)


# ===================
# = State Shapefile =
# ===================
library(rgdal)
library(sp)
states <- readOGR("GIS_Data/cb_2016_us_state_500k", "cb_2016_us_state_500k")
states.data <- states@data

proj.states <- proj4string(states)

states.wcoast <- states[states$NAME %in% c("Washington", "California", "Oregon"),]

states.wcoast.utm <- spTransform(states.wcoast, "+proj=utm +zone=10 +ellps=GRS80 +datum=NAD83 +units=km +no_defs")

# png("Figures/rcas_map.png", height=5, width=5, units="in", res=300)
# plot(closed_areas, col="orange")
# plot(states.wcoast, add=T)
# points(Lat ~ Lon, knot_locs_LL, cex=0.1)
# dev.off()

# =============================
# = VAST Density Output =
# =============================
fname <- "Data/VAST_density_1April19/" #all from this date, except sablefish which is 2April2019
#fname <- "Data/VAST_density_500/"
#fname <- "Data/VAST_output_v1/"
spp.fold <- list.files(fname)


#write.csv(data.table(spp_common=spp.fold), "thorson_spp.csv")

spp.list <- vector("list", length(spp.fold))
spp.names <- gsub(".csv", "", gsub("Dens_DF_", "", spp.fold))


yrs <- seq(1977,2017)

### Get area in each knot from shortspine (in future runs all output will likely have this column)
#knot_locs <- read_csv("Data/VAST_knots_v1/Knot_area_km2_shortspine thornyhead.csv")
knot_locs <- read_csv("Data/VAST_knots_1April19/Knot_area_km2_sablefish.csv")
#knot_locs <- read_csv("Data/VAST_knots_500/Knot_area_km2_sablefish.csv")
knot_locs <- knot_locs %>%
  mutate(knot_num=seq(1:length(E_km)))


### knots with 0 area (this effectively removes them from the calculation of TotalBio)
### Nick says when knots are outside the footprint, they get an area set to 0, and should be removed
### Will remove before creating 500 knots in future runs
knot_locs %>% filter(Area_km2==0)

attr(knot_locs, "projection") <- "UTM"
attr(knot_locs, "zone") <- 10
knot_locs$X <- knot_locs$E_km
knot_locs$Y <- knot_locs$N_km
knot_locs_LL <- rename(convUL(knot_locs), Lon=X, Lat=Y)


### Knot N  or S of 40 degrees 10 minute line
knot_locs_LL <- knot_locs_LL %>%
  mutate(knotN4010=ifelse(Lat>40.166666666666664, 1, 0))


cc <- bind_rows(spp.list)


# =====================================
# = Remove years without trawl survey =
# =====================================
### Also limit to beginning in 1980, first year with PACFIN landings data
tri.yrs <- seq(1980, 2004, by=3)
ann.yrs <- seq(2003,2017)

surv.yrs <- c(tri.yrs, ann.yrs)

cc.lim <- cc %>% filter(Year %in% surv.yrs)


# =====================================
# = COG Knot Bio =
# =====================================
cog_knot <- cc.lim %>%
  group_by(spp_common, year) %>%
  summarise(cog_N=weighted.mean(N_km, w=KnotBio),
            cog_Lat=weighted.mean(Lat, w=KnotBio),
            cog_N_density=weighted.mean(N_km, w=Density_kgkm2),
            cog_Lat_density=weighted.mean(Lat, w=Density_kgkm2))

plot(cog_N_density ~ year, subset(cog_knot, spp_common=="Dover sole"), type="o", col="red")
#points(cog_N ~ Year, cog_tpw, type="o", col="blue") #from original ThorsonPinskyWard

points(cog_N_density ~ year, subset(cog_knot, spp_common=="shortspine thornyhead"), type="o", col="blue")


# =====================================
# = Fraction bio N of 4010 =
# =====================================
frac.n <- cc.lim %>%
  group_by(year, spp_common) %>%
  summarise(BioN=sum(KnotBio[knotN4010==1]),
            TotalBio=sum(KnotBio))

frac.n <- frac.n %>%
  mutate(fracN=BioN/TotalBio)

plot(fracN ~ year, subset(frac.n, spp_common=="sablefish"), type="l")

ggplot(frac.n, aes(x=year, y=fracN, color=spp_common)) + geom_point() + geom_line()

# # =====================================
# # = Closed Areas =
# # =====================================
# ### 2006 vintage
# cons_areas <- readOGR("GIS_data/efhgroundfish","efh_consarea_polygons")
# 
# # efh_700fm <- readOGR("GIS_data/efhgroundfish", "efh_700fm_polygons")
# # 
# closed_areas <- unionSpatialPolygons(cons_areas, IDs=cons_areas@data$PROHIBIT)
# 
# 
# 
# 
# ### 2015-2016
# rca1516 <- readOGR("GIS_Data/CommTrawlRCA_2015-16/", "RCA_CommTrawl_2015_16_poly")
# 
# rca1516_utm <- spTransform(rca1516, crs(states.wcoast.utm))
# 
# 
# plot(states.wcoast.utm)
# plot(rca1516_utm,  col="orange")
# points(knot_locs$E_km, knot_locs$N_km, cex=0.1)
# 
# # =====================================
# # = Knots within Closed Areas =
# # =====================================
# 
# 
# coordinates(knot_locs)<- cbind(knot_locs$X, knot_locs$Y)
# proj4string(knot_locs) <- crs(rca1516_utm)
# 
# knots_inrca <- sp::over(knot_locs, rca1516_utm)
# knots_inrca$knot_num <- seq(1:500)
# knots_inrca$closed <- ifelse(is.na(knots_inrca$region_id), 0, 1)
# 
# knot_locs_df <- as.data.frame(knot_locs)
# knot_locs_df2 <- merge(knot_locs_df, knots_inrca, by=c("knot_num"))
# 
# png("Figures/rcas1516_knots.png", height=8, width=8, units="in", res=300)
# plot(states.wcoast.utm)
# plot(rca1516_utm, col="yellow", border=NA, add=T)
# points(N_km ~ E_km, subset(knot_locs_df2,closed==0), cex=0.25, col="blue")
# points(N_km ~ E_km, subset(knot_locs_df2,closed==1), cex=0.5,  col="red")
# dev.off()
# 
# 
# ### Number of knots in closed areas
# sum(knot_locs_df2$closed)
# 
# cc.lim <- cc.lim %>%  
#   inner_join(knot_locs_df2, by=c("E_km", "N_km", "knot_num", "Area_km2"))
# 

# =====================================
# = Distribution of Biomass (kg) across Space =
# =====================================
region.bio <- cc.lim %>%
  group_by(spp_common, year) %>%
  summarise(regionBio=sum(KnotBio))


cc.lim2 <- cc.lim %>%
  inner_join(region.bio, by=c("spp_common", "year"))%>%
  mutate(relBio=KnotBio/regionBio)


# =====================================
# = Couple with spawning stock biomass from assessment =
# =====================================
### Units= metric tons of spawning biomass
dover <- read_csv("Data/AssessmentSPB/Dover_2015_SPB.csv")
dover <- dover %>%
  mutate(spp_common="Dover sole",
         year:=as.numeric(gsub("SPB_", "", LABEL)),
         LABEL=NULL,
         Projected=ifelse(year > 2015, 1, 0)) #projected years in the assessment



sable <- read_csv("Data/AssessmentSPB/Sablefish_2015_SPB.csv")
sable <- sable %>%
  mutate(spp_common="sablefish",
         year:=as.numeric(gsub("SPB_", "", LABEL)),
         LABEL=NULL,
         Projected=ifelse(year > 2015, 1, 0)) # projected years in the assessment

petrale <- read_csv("Data/AssessmentSPB/Petrale_2015_SPB.csv")
petrale <- petrale %>%
  mutate(spp_common="petrale sole",
         year:=as.numeric(gsub("SPB_", "", LABEL)),
         LABEL=NULL,
         Projected=ifelse(year > 2015, 1, 0))

short <- read_csv("Data/AssessmentSPB/Shortspine_2013_SPB.csv")
short <- short %>%
  mutate(spp_common="shortspine thornyhead",
         year:=as.numeric(gsub("SPB_", "", LABEL)),
         LABEL=NULL,
         Projected=ifelse(year > 2013, 1, 0))



dtsp.bio <- rbind(dover, sable, petrale, short)




lingS <- read_csv("Data/AssessmentSPB/Lingcod_South_2017_SPB.csv")
lingN <- read_csv("Data/AssessmentSPB/Lingcod_North_2017_SPB.csv")
lingS <- lingS %>%
  mutate(spp_common="lingcod",
         region="South",
         year=as.numeric(gsub("SPB_", "", LABEL)),
         LABEL=NULL,
         Projected=ifelse(year > 2017, 1, 0))
lingN <- lingN %>%
  mutate(spp_common="lingcod",
         region="North",
         year=as.numeric(gsub("SPB_", "", LABEL)),
         LABEL=NULL,
         Projected=ifelse(year > 2017, 1, 0))
ling <- rbind(lingS, lingN)

ling.overall <- ling %>%
  group_by(year, spp_common, Projected) %>%
  summarise(Value=sum(Value),
            StdDev=sqrt(sum(StdDev^2)))

### SSB file for all species
dtspling <- bind_rows(dtsp.bio, ling.overall)




# =====================================
# = Merge with distribution =
# =====================================
### Calculate stock biomass in each knot by multiplying relative biomass across space by total SSB from assessment
dtspling.dist <- dtspling %>%
  inner_join(cc.lim2, by=c("year", "spp_common")) %>%
  mutate(StockBio=relBio*Value)


# =====================================
# = COG and Stock Assessment Bio =
# =====================================
### COG by year
cog <- dtspling.dist%>%
  group_by(year, spp_common, Projected)%>%
  summarise(cog_N=weighted.mean(N_km, StockBio),
            cog_E=weighted.mean(E_km, StockBio),
            cog_Lat=weighted.mean(Lat, StockBio),
            log_assessBio=log(mean(Value)),
            SSB=mean(Value)) %>%
  mutate(SSB.thous=SSB/1000) ### SSB in thousand metric tons

cog_p <- ggplot(cog, aes(x=year, y=cog_N, color=spp_common)) + geom_line()

cog_dover <- ggplot(subset(cog, spp_common=="Dover sole"), aes(x=year, y=cog_N, color=spp_common)) + geom_line()

### Update these when remaking Fig 2
# ### Annual mean COG
# overall_cog <- dtspling.dist[,list(cog_N_overall=weighted.mean(N_km, StockBio)), by=list(spp_common)]
# cog2 <- merge(cog, overall_cog, by=c("spp_common"))
# 
# ### Anomaly in COG
# cog2[,"cog_anom":=cog_N - cog_N_overall]
# 
# ### Standard deviation of COG anomaly by species
# cog_sd <- cog2[,list(sd_cog=sd(cog_N),
#                      mad_cog=sum(abs(cog_N-cog_N_overall))/.N,
#                      max_cog_anom=max(abs(cog_anom))), by=list(spp_common)]


### Compare with raw COG
load("Data/catch_for_vast.RData", verbose=T)
raw_cog <- vast.catch %>%
  filter(Species != "longspine thornyhead") %>%
  group_by(Year, Species)%>%
  summarise(cog_Lat=weighted.mean(Latitude, Catch_KG))

cog_raw <- ggplot(raw_cog, aes(x=Year, y=cog_Lat, color=Species)) + geom_line()

### NOTE: weighting by log biomass will change the relative COG

# ==================================
# = Distance between Port and Knot =
# ==================================
# calc_d_km <- function(knots, ports){
#   dist= knots - ports
# }

knotE <- cross_df(list(E_km=knot_locs$E_km, E_km_port=port_locs_utm$E_km_port))
knotN <- cross_df(list(N_km=knot_locs$N_km, N_km_port=port_locs_utm$N_km_port))

knotE <- knotE %>%
  mutate(dist.E=E_km - E_km_port) %>%
  inner_join(knot_locs, by=c("E_km"))

knotN <- knotN %>%
  mutate(dist.N=N_km - N_km_port) %>%
  inner_join(knot_locs, by=c("N_km"))

dist_port <- knotN %>%
  inner_join(knotE, by=c("N_km", "E_km", "Area_km2", "knot_num")) %>%
  inner_join(port_locs_utm, by=c("N_km_port", "E_km_port"))%>%
  mutate(total_dist=sqrt((dist.E)^2 + (dist.N)^2)) #distance in km

# port_locs <- ports_lim[,list(port=PACFIN_PORT_CODE, easting=X, northing=Y)]
# 
# port_name <- ports_lim$PACFIN_PORT_CODE
# port_easting <-ports_lim$X
# port_northing <- ports_lim$Y


#### Merge with biomass data
cc_spp_dist <- dtspling.dist %>%
  inner_join(dist_port, by=c("knot_num", "E_km", "N_km", "Area_km2"))%>%
  mutate(inv.dist= 1/total_dist,
         SB.inv=StockBio*inv.dist) #use inverse distance to weight biomass in calculating stock availability

### Diagnostics, making sure each species has the right number of observations
### Each species should have 2000 observations in each year (500 knots * 4 ports), but only 1988 because 3 knots had 0 area
with(cc_spp_dist, table(year, spp_common))
#length(unique(cc_spp_dist$year)) #23 years

cc_spp_dist %>% 
  filter(year==2016) %>%
  ggplot(., aes(x=log(SB.inv))) + geom_histogram( col="black", fill="gray") + facet_wrap(~port)


# ==================================
# = Biomass Weighted by Inverse Distance =
# ==================================
### Taking weighted.mean of StockBio weighted by inverse distance is equivalent to 
### taking sum(StockBio*inv.dist)/sum(inv.dist)
### is it more relevant to take sum of the quantity StockBio*inv.dist?
### --> the sum matches more with kernel density of StockBiomass as a function of Northings
cc_spp_bio_port <- cc_spp_dist %>%
  group_by(spp_common, port, Port_Name, LANDING_YEAR, year)%>%
  summarise(wtd.bio=weighted.mean(StockBio, w=inv.dist),
            sum.bio=sum(StockBio*inv.dist), 
            port.N=round(mean(N_km_port))) %>%
  mutate(thous.wtd.bio=wtd.bio/1000,
         log.bio=log(wtd.bio)) #not getting the same value as when logging within summation
## my equation in the paper, we log first (wtd.log.bio)
### Arguably fishermen would care about absolute StockBiomass not logged, so logging after makes sense for plotting purposes
### So change equation for availability to remove log (and then say log(Avail) for plotting)

cc_spp_bio_port %>% filter(spp_common=="sablefish" & port=="MRO")
cc_spp_bio_port %>% filter(spp_common=="sablefish" & port=="AST")

p_sum <- ggplot(cc_spp_bio_port%>%filter(spp_common %in% c("sablefish", "petrale sole")), 
                aes(x=year, y=sum.bio, color=port))+
  geom_line() + facet_wrap(~spp_common, scales="free_y")

# ==================================
# = Biomass Weighted by Inverse Distance within Radius =
# ==================================
### Taking weighted.mean of StockBio weighted by inverse distance is equivalent to 
### taking sum(StockBio*inv.dist)/sum(inv.dist)
### is it more relevant to take sum of the quantity StockBio*inv.dist
### --> the sum matches more with kernel density of StockBiomass as a function of Northings
## my equation in the paper, we log first (wtd.log.bio)
### Arguably fishermen would care about absolute StockBiomass not logged, so logging after makes sense for plotting purposes
### So change equation for availability to remove log (and then say log(Avail) for plotting)
### Or just use raw for Figure 3 (since species specific anyway)

### Logbook data suggests ports don't travel more than 200km to catch these species
# ### See Figure DistanceWeightedbyCatch_DTSPL.png in FATEavail
rad <- 200 #limit to knots within radius (km)

cc_spp_dist_lim <- cc_spp_dist %>%
  filter(total_dist < rad)

cc_spp_bio_port_rad <- cc_spp_dist_lim %>%
  group_by(spp_common, port, Port_Name, LANDING_YEAR, year)%>%
  summarise(wtd.bio=weighted.mean(StockBio, w=inv.dist),
            sum.bio=sum(StockBio*inv.dist),
            sum.bio.unwtd=sum(StockBio),
            port.N=round(mean(N_km_port)),
            num.knots=length(unique(knot_num))) %>%
  mutate(thous.wtd.bio=wtd.bio/1000,
         log.bio=log(wtd.bio))


### Without closed areas
cc_spp_bio_port_rad_rca <- cc_spp_dist_lim %>%
  group_by(spp_common, port, Port_Name, LANDING_YEAR, year, closed)%>%
  summarise(wtd.bio=weighted.mean(StockBio, w=inv.dist),
            sum.bio=sum(StockBio*inv.dist),
            sum.bio.unwtd=sum(StockBio),
            port.N=round(mean(N_km_port)),
            num.knots=length(unique(knot_num))) %>%
  mutate(thous.wtd.bio=wtd.bio/1000,
         log.bio=log(wtd.bio))


# =====================================
# = Plot settings =
# =====================================

dtspling.spp <- unique(dtspling$spp_common)
library(RColorBrewer)
dts.color <- brewer.pal(n=9, "Spectral")
dts.color <- dts.color[c(1:3, 7,9)]
#dts.pch <- c(0,15,1,16,2,17)
dts.pch <- c(19,3,15,7,17) #to match ggplot in fig 4

#port3 <- c("MRO", "BRG", "COS", "AST") #ordered by lat
# use port_names_df instead
port.col <- c( "gray", "darkorchid4", "black", "#008081")
port.pch <- c(19,1,15, 7)

exp.list <- seq(0,4, by=1)
land.exp.brks <- 10^(exp.list)
land.log.brks <- log(land.exp.brks)


bio.exp.brks <- c(1,5,10,25,50,100, 250,500,1000,2500, 5000,10000)
bio.log.brks <- log(bio.exp.brks)

ssb.exp.brks <- c(2500, 5000,10000, 20000, 50000, 100000, 200000, 500000)
ssb.log.brks <- log(ssb.exp.brks)
ssb.exp.brks.thous <- round(ssb.exp.brks/1000)

### Transparent gray for MRO polygon
gray_trans <- rgb(190, 190,190, 127, maxColorValue=255) 

### Transparent green for AST polygon
col2rgb("#008081", alpha=T)
green_trans <- rgb(0,128,129, 127, maxColorValue=255)

# ==================================
# = Plot availability to each port by species =
# ==================================
p <- ggplot(cc_spp_bio_port, aes(x=year, y=wtd.bio, color=port))+
  geom_line() + facet_wrap(~spp_common, scales="free_y")




###################### Inverse distance weighted mean biomass available to port ######################
### Interpretation: this is the mean biomass (mt) in each knot weighted by the inverse distance of that knot to port
### Higher values could arise from higher biomass for the same distance
### or smaller distance for the same biomass
library(data.table)
png("Figures/wtdmean.StockBiobySpp_DTSPling.png", height=8, width=8, units="in", res=300)
par(mfrow=c(2,2), mar=c(4,4,2,2))
as.data.table(cc_spp_bio_port)[,j={
  t.dt <- .SD
  plot(wtd.bio ~ year, t.dt, col="white",  
       main=paste0(unique(spp_common)), ylab="Stock Availability (mt)")
  for(i in 1:length(port_names_df$Pcid)){
    sub <- t.dt[port == port_names_df$Pcid[i]]
    points(wtd.bio ~ year, sub, type="o", col=port.col[i], pch=port.pch[i], lwd=1)
  }
  abline(v=2003, col="grey", lty=2)
}, by=list(spp_common)]

plot(states.wcoast.utm, xlim=c(200,600), ylim=c(3800,5200))
points(N_km_port ~ E_km_port, port_locs_utm, pch=rev(port.pch), col=rev(port.col), cex=1.5, lwd=1.5)
# text(x=port_locs_utm$E_km_port-150,
#      y=port_locs_utm$N_km_port,
#      labels=port_locs_utm$Port_Name)
legend("topleft", xjust=0, 
       legend=rev(port_names_df$Port_Name), lty=1, pch=rev(port.pch), col=rev(port.col), xpd=T, lwd=1, bty="n")
dev.off()




###################### Sum (bio*inverse distance) ######################
library(data.table)
png("Figures/sumStockBiobySpp_DTSPling_500knots.png", height=8, width=8, units="in", res=300)
par(mfrow=c(2,2), mar=c(4,4,2,2))
as.data.table(cc_spp_bio_port)[,j={
  t.dt <- .SD
  plot(sum.bio ~ year, t.dt, col="white",  
       main=paste0(unique(spp_common)), ylab="Stock Availability (mt/km)")
  for(i in 1:length(port_names_df$Pcid)){
    sub <- t.dt[port == port_names_df$Pcid[i]]
    points(sum.bio ~ year, sub, type="o", col=port.col[i], pch=port.pch[i], lwd=1)
  }
  abline(v=2003, col="grey", lty=2)
}, by=list(spp_common)]

plot(states.wcoast.utm, xlim=c(200,600), ylim=c(3800,5200))
points(N_km_port ~ E_km_port, port_locs_utm, pch=rev(port.pch), col=rev(port.col), cex=1.5, lwd=1.5)
# text(x=port_locs_utm$E_km_port-150,
#      y=port_locs_utm$N_km_port,
#      labels=port_locs_utm$Port_Name)
legend("topleft", xjust=0, 
       legend=rev(port_names_df$Port_Name), lty=1, pch=rev(port.pch), col=rev(port.col), xpd=T, lwd=1, bty="n")
dev.off()


###################### Sum bio * inverse distance with fixed radius ######################
library(data.table)
png("Figures/sumStockBiobySpp_DTSPling_rad200_500knots.png", height=8, width=8, units="in", res=300)
par(mfrow=c(2,2), mar=c(4,4,2,2))
as.data.table(cc_spp_bio_port_rad)[,j={
  t.dt <- .SD
  plot(sum.bio ~ year, t.dt, col="white",  
       main=paste0(unique(spp_common)), ylab="Stock Availability (mt/km)")
  for(i in 1:length(port_names_df$Pcid)){
    sub <- t.dt[port == port_names_df$Pcid[i]]
    points(sum.bio ~ year, sub, type="o", col=port.col[i], pch=port.pch[i], lwd=1)
  }
  abline(v=2003, col="grey", lty=2)
}, by=list(spp_common)]

plot(states.wcoast.utm, xlim=c(200,600), ylim=c(3700,5300))
symbols(x=port_locs_utm$E_km_port, y=port_locs_utm$N_km_port, circles=rep(200,4), inches=F,
        fg=rev(port.col), add=T)
plot(states.wcoast.utm, xlim=c(200,600), ylim=c(3700,5300), add=T, col="white")
points(N_km_port ~ E_km_port, port_locs_utm, pch=rev(port.pch), col=rev(port.col), cex=1.5, lwd=1.5)
# text(x=port_locs_utm$E_km_port-150,
#      y=port_locs_utm$N_km_port,
#      labels=port_locs_utm$Port_Name)
legend("topleft", xjust=0, 
       legend=rev(port_names_df$Port_Name), lty=1, pch=rev(port.pch), col=rev(port.col), xpd=T, lwd=1, bty="n")
dev.off()


################ Unweighted sum biomass within radius ######################
### Units thousand metric tons (matches that for the overall stock biomass)
library(data.table)
png("Figures/unwtd.sumStockBiobySpp_DTSPling_rad200_500knots.png", height=8, width=8, units="in", res=300)
par(mfrow=c(2,2), mar=c(4,4,2,2))
as.data.table(cc_spp_bio_port_rad)[,j={
  t.dt <- .SD
  plot(sum.bio.unwtd/1000 ~ year, t.dt, col="white",  
       main=paste0(unique(spp_common)), ylab="Biomass (thousand mt) within 200km")
  for(i in 1:length(port_names_df$Pcid)){
    sub <- t.dt[port == port_names_df$Pcid[i]]
    points(sum.bio.unwtd/1000 ~ year, sub, type="o", col=port.col[i], pch=port.pch[i], lwd=1)
  }
  abline(v=2003, col="grey", lty=2)
}, by=list(spp_common)]

plot(states.wcoast.utm, xlim=c(200,600), ylim=c(3700,5300))
symbols(x=port_locs_utm$E_km_port, y=port_locs_utm$N_km_port, circles=rep(200,4), inches=F,
        fg=rev(port.col), add=T)
plot(states.wcoast.utm, xlim=c(200,600), ylim=c(3700,5300), add=T, col="white")
points(N_km_port ~ E_km_port, port_locs_utm, pch=rev(port.pch), col=rev(port.col), cex=1.5, lwd=1.5)
# text(x=port_locs_utm$E_km_port-150,
#      y=port_locs_utm$N_km_port,
#      labels=port_locs_utm$Port_Name)
legend("topleft", xjust=0, 
       legend=rev(port_names_df$Port_Name), lty=1, pch=rev(port.pch), col=rev(port.col), xpd=T, lwd=1, bty="n")
dev.off()

### Only petrale sole and sablefish
png("Figures/unwtd.sumStockBio_sablepetrale_rad200_500knots.png", height=5, width=8, units="in", res=300)
par(mfrow=c(1,2), mar=c(4,4,2,2))
as.data.table(cc_spp_bio_port_rad)[spp_common %in% c("petrale sole"),j={
  t.dt <- .SD
  plot(sum.bio.unwtd/1000 ~ year, t.dt, col="white",  
       main=paste0(unique(spp_common)), ylab="Biomass (thousand mt) within 200km")
  for(i in 1:length(port_names_df$Pcid)){
    sub <- t.dt[port == port_names_df$Pcid[i]]
    points(sum.bio.unwtd/1000 ~ year, sub, type="o", col=port.col[i], pch=port.pch[i], lwd=1)
  }
  abline(v=2003, col="grey", lty=2)
}, by=list(spp_common)]
legend("topleft", xjust=0, 
       legend=rev(port_names_df$Port_Name), lty=1, pch=rev(port.pch), col=rev(port.col), lwd=1, bty="n")

as.data.table(cc_spp_bio_port_rad)[spp_common %in% c("sablefish"),j={
  t.dt <- .SD
  plot(sum.bio.unwtd/1000 ~ year, t.dt, col="white",  
       main=paste0(unique(spp_common)), ylab="Biomass (thousand mt) within 200km")
  for(i in 1:length(port_names_df$Pcid)){
    sub <- t.dt[port == port_names_df$Pcid[i]]
    points(sum.bio.unwtd/1000 ~ year, sub, type="o", col=port.col[i], pch=port.pch[i], lwd=1)
  }
  abline(v=2003, col="grey", lty=2)
}, by=list(spp_common)]
dev.off()


### Just the map with the radius
png("Figures/map_wradius.png", height=5, width=5, units="in", res=300)
plot(states.wcoast.utm, xlim=c(200,600), ylim=c(3700,5300))
symbols(x=port_locs_utm$E_km_port, y=port_locs_utm$N_km_port, circles=rep(200,4), inches=F,
        fg=rev(port.col), add=T)
plot(states.wcoast.utm, xlim=c(200,600), ylim=c(3700,5300), add=T, col="white")
points(N_km_port ~ E_km_port, port_locs_utm, pch=rev(port.pch), col=rev(port.col), cex=1.5, lwd=1.5)
# text(x=port_locs_utm$E_km_port-150,
#      y=port_locs_utm$N_km_port,
#      labels=port_locs_utm$Port_Name)
legend("topleft", xjust=0, 
       legend=rev(port_names_df$Port_Name), lty=1, pch=rev(port.pch), col=rev(port.col), xpd=T, lwd=1, bty="n")
dev.off()


########################## FIGURE 2 ##########################
### Will want to make different symbol for whether SSB is projected or measured (eg is date later than stock assessment)
cog2 <- as.data.table(cog)

# prepare UTM coordinates matrix
proj_utm <- "+proj=utm +zone=10 +ellps=GRS80 +datum=NAD83 +units=km +no_defs";
cog_utm <- data.frame(cog_E=cog2$cog_E, cog_N=cog2$cog_N, m=seq(1:length(cog2$cog_N)))
utmcoor<-SpatialPointsDataFrame(coords=data.frame(cog_Lon=cog_utm$cog_E, cog_Lat=cog_utm$cog_N),
                                data=cog_utm, proj4string=CRS(proj_utm))
#utmdata$X and utmdata$Y are corresponding to UTM Easting and Northing, respectively.
#zone= UTM zone
# converting
longlatcoor<-spTransform(utmcoor,CRS("+proj=longlat"))
longlatcoor.df <- as.data.frame(longlatcoor)

cog_wll <- merge(cog2, longlatcoor.df, by=c("cog_E", "cog_N"))
setorder(cog_wll, year)


png("Figures/RegionBioCOG_DTSPling_500knots.png", height=5, width=8, units="in", res=300)
par(mfrow=c(1,2), mar=c(4,4,4,3))
### log(SSB) is plotted, but labeled as thousand metric tons by changing axis(side=2)
plot(log_assessBio ~ year,cog2, col="white", ylab="Assessed Spawning Biomass (thousand mt)", yaxt="n")
axis(side=2, at=ssb.log.brks, labels=ssb.exp.brks.thous, las=1)
abline(v=2003, col="grey", lty=2)
for(i in 1:length(dtspling.spp)){
  sub <- cog2[spp_common==dtspling.spp[i]]
  points(log_assessBio  ~ year, sub, col=dts.color[i], pch=dts.pch[i], type="o")
}

legend(1980, 14.5, ncol=3, legend=dtspling.spp, pch=dts.pch, col=dts.color, xpd=NA, bty="n")

plot(cog_N ~ year,cog_wll, col="white", ylab="Center of Gravity (km Northing)", las=1)
#out <- matrix(nrow=length(unique(cog2$year)), ncol=length(dtspling.spp))
abline(v=2003, col="grey", lty=2)
for(i in 1:length(dtspling.spp)){
  sub <- cog_wll[spp_common==dtspling.spp[i]]
  points(cog_N ~ year, sub, col=dts.color[i], pch=dts.pch[i], type="o")
  # temp <- smooth.spline(x=sub$year, y=sub$cog_anom)
  # out[,i] <- temp$y
  # points(unique(cog2$year), out[,i], type="l", col=dts.color[i])
}
par(new=T)
plot(cog_Lat ~ year, cog_wll[spp_common=="sablefish"], col=NA, axes=F, xlab=NA, ylab=NA)
axis(side=4)
mtext("Latitude", side=4, line=1.75)
dev.off()

#################
compare.stockBio.map <- function(dat_knot, dat_cog, spp, yrs){
  zlims <- c(min(dat_knot[spp_common==spp]$StockBio), max(dat_knot[spp_common==spp]$StockBio))
  # a <- ggplot(data=dat_knot[spp_common==spp & year==yrs[1]], aes(x=E_km, y=N_km))+ labs(title=paste0(yrs[1], " ", spp))
  # b <- a + geom_point(aes(color=StockBio)) + 
  #   scale_color_distiller(palette="Spectral", limits=zlims) + 
  #   guides(color = "none")+ 
  #   geom_hline(aes(yintercept=dat_cog[spp_common==spp & year==yrs[1]]$cog_N), lty=2)
  
  a <- ggplot()+ labs(title=paste0(yrs[1], " ", spp))
  b <- a + geom_point(data=dat_knot[spp_common==spp & year==yrs[1]], aes(x=E_km, y=N_km, color=StockBio)) +
                      xlim(250,1000) + 
    scale_color_distiller(palette="Spectral", limits=zlims) + 
    guides(color = "none")+ 
    geom_hline(aes(yintercept=dat_cog[spp_common==spp & year==yrs[1]]$cog_N), lty=2)
  b2 <- b + geom_point(data=port_locs_utm, aes(x=E_km_port, y=N_km_port))+
    geom_text(data=port_locs_utm, aes(x=E_km_port, y=N_km_port, label=Port_Name),hjust=-0.15, vjust=0)
  
  c <- ggplot()+ labs(title=paste0(yrs[2], " ", spp))
  d <- c + geom_point(data=dat_knot[spp_common==spp & year==yrs[2]], aes(x=E_km, y=N_km, color=StockBio)) + 
    xlim(250,1000) + scale_color_distiller(palette="Spectral", limits=zlims) + 
    guides(color = "none")+ 
    geom_hline(aes(yintercept=dat_cog[spp_common==spp & year==yrs[2]]$cog_N), lty=2)
  d2 <- d + geom_point(data=port_locs_utm, aes(x=E_km_port, y=N_km_port))+
    geom_text(data=port_locs_utm, aes(x=E_km_port, y=N_km_port, label=Port_Name),hjust=-0.15, vjust=0)
  
  
  e <- ggplot()+ labs(title=paste0(yrs[3], " ", spp))
  f <- e + geom_point(data=dat_knot[spp_common==spp & year==yrs[3]], aes(x=E_km, y=N_km, color=StockBio)) + 
    xlim(250,1000) + scale_color_distiller(palette="Spectral", limits=zlims) + 
    geom_hline(aes(yintercept=dat_cog[spp_common==spp & year==yrs[3]]$cog_N), lty=2)
  f2 <- f + geom_point(data=port_locs_utm, aes(x=E_km_port, y=N_km_port))+
    geom_text(data=port_locs_utm, aes(x=E_km_port, y=N_km_port, label=Port_Name),hjust=-0.15, vjust=0)
  
  
  library(cowplot)
  figname <- paste0("Figures/StockDistMaps/", spp, paste0(yrs, collapse=""), "StockBio_map.png")
  fig <- ggdraw(plot_grid(b2,d2,f2, ncol=3, rel_widths=c(0.7,0.7,1)))
  ggsave(figname, fig, width=10, height=5, units="in")
}

compare.stockBio.map(as.data.table(dtspling.dist),cog_wll, "sablefish", c(1980, 1992, 2013))
# compare.stockBio.map(as.data.table(dtspling.dist),cog_wll, "sablefish", c(2003, 2004, 2005))
# compare.stockBio.map(as.data.table(dtspling.dist),cog_wll, "sablefish", c(2006, 2007, 2008))
# compare.stockBio.map(as.data.table(dtspling.dist),cog_wll, "sablefish", c(2009, 2010, 2011))
# compare.stockBio.map(as.data.table(dtspling.dist),cog_wll, "sablefish", c(2012, 2013, 2014))
# compare.stockBio.map(as.data.table(dtspling.dist),cog_wll, "sablefish", c(2015, 2016, 2017))


##########################
### Density plots of StockBio across latitude in focal years
dtspling.dist.dt <- as.data.table(dtspling.dist)

### Still need to check math on converting densities back to Stock Biomass (mt) and matching to sumStockBio
### Here I do so by SB=totalSB*(d$y/totaly)
### Should be the trapezoid of x2-x1=200km around port

# density approximates probabilty density function
# with x in km, this is probability density per km
# probability=Area under curve would be trapezoid x2-x1=200km, y1=dens(x1), y2=dens(x2)

library(weights)
northings.brks <- seq(3600,5400, by=c(50))

pdf("Figures/sablefish_SB_hist.pdf", height=8, width=8)
par(mfrow=c(3,3))
dtspling.dens.SB <- dtspling.dist.dt[spp_common=="sablefish",j={
  t.dt <- .SD
  totalSB <- sum(t.dt$StockBio)
  wtd.hist(x=t.dt$N_km, weight=t.dt$StockBio, breaks=northings.brks, freq=T, 
                main=paste0(spp_common, " ", year))
  d <- density(x=t.dt$N_km, weights=t.dt$StockBio/totalSB)
  list(N_km=d$x, totalSB=totalSB, SB=totalSB*d$y)
}, by=list(spp_common, year)]
dev.off()



### 200km vicinity of port
port_vic <- port_locs_utm %>%
  mutate(port_100N=N_km_port + 100,
         port_100S=N_km_port - 100)

plot_SB_N <- function(df, sp, yr){
  ylim.SB <- c(0,max(df[spp_common==sp]$SB))
  plot(SB ~ N_km, df[spp_common==sp & year==yr], type="l", 
       main=paste0(sp, " ", yr), ylab="Kernel Density (mt/km Northing)", ylim=ylim.SB)
  points(port_locs_utm$N_km_port, rep(0,4), pch=rev(port.pch), col=rev(port.col))
  abline(v=cog2[spp_common==sp & year==yr]$cog_N, lty=2)
  mro_100N <- subset(port_locs_utm, port=="MRO")$N_km_port+100 #100 km N of MRO
  mro_100S <- subset(port_locs_utm, port=="MRO")$N_km_port-100 # 100 km S of MRO
  polygon(x=c(mro_100S, mro_100S, mro_100N,mro_100N),y=c(-5,ylim.SB[2],ylim.SB[2], -5), 
          col=gray_trans, border=NA)
  ast_100N <- subset(port_locs_utm, port=="AST")$N_km_port+100 #100 km N of MRO
  ast_100S <- subset(port_locs_utm, port=="AST")$N_km_port-100 # 100 km S of MRO
  polygon(x=c(ast_100S, ast_100S, ast_100N,ast_100N),y=c(-5,ylim.SB[2],ylim.SB[2], -5), 
          col=green_trans, border=NA)
  text(port_locs_utm$N_km_port, rep(ylim.SB[2],4), labels=port_locs_utm$port)
}

pdf("Figures/sablefish_kern_ts_0317.pdf", height=8, width=8)
par(mfrow=c(3,3), mar=c(4,4,2,2))
yrs.plot <- seq(2003,2017)
for(i in 1:length(yrs.plot)){
  plot_SB_N(df=dtspling.dens.SB[year>=2003], "sablefish", yrs.plot[i])
}
dev.off()


pdf("Figures/sablefish_kern_ts_yrslim.pdf", height=8, width=8)
par(mfrow=c(2,2), mar=c(4,4,2,2))
yrs.lim <- c(2003, 2006,2010,2013)
for(i in 1:length(yrs.lim)){
  plot_SB_N(df=dtspling.dens.SB[year%in%yrs.lim], "sablefish", yrs.lim[i])
}
dev.off()

pdf("Figures/petrale_kern_ts_0317.pdf", height=8, width=8)
par(mfrow=c(3,3), mar=c(4,4,2,2))
yrs.plot <- seq(2003,2017)
for(i in 1:length(yrs.plot)){
  plot_SB_N(df=dtspling.dens.SB[year>=2003], "petrale sole", yrs.plot[i])
}
dev.off()
