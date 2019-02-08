### Objective calculate stock availability to select ports

library(tidyverse)


### Read in port locations
port_locs<- read_csv("Data/PacFIN & BEF ports LUT.csv")


#############
### Subset to desired ports (can subset by name or Pcid)
ports_lim <- c("MRO", "BRG", "COS", "AST")

port_locs_lim <- port_locs %>%
  filter(Pcid %in% ports_lim)%>%
  transmute(X=Lon, Y=Lat, Port_Name=Name, port=Pcid, Lon=Lon, Lat=Lat)


#############
### Convert port location to northings and eastings
library(PBSmapping)
attr(port_locs_lim, "projection") <- "LL"
port_locs_utm <- rename(convUL(port_locs_lim), E_km_port=X, N_km_port=Y)


# =============================
# = VAST Density Output =
# =============================
spp.fold <- list.files("Data/VAST_output")

#write.csv(data.table(spp_common=spp.fold), "thorson_spp.csv")

spp.list <- vector("list", length(spp.fold))
spp.names <- gsub(".csv", "", gsub("Dens_DF_", "", spp.fold))


yrs <- seq(1977,2017)

### Get area in each knot from shortspine (in future runs all output will likely have this column)
knot_locs <- read_csv("Data/Knot_area_km2_shortspine thornyhead.csv")
knot_locs <- knot_locs %>%
  mutate(knot_num=seq(1:length(E_km)))


### knots with 0 area (this effectively removes them from the calculation of TotalBio)
### Nick says these are outside the footprint, and should be removed
### Will remove before creating 500 knots in future runs
knot_locs %>% filter(Area_km2==0)


### Import VAST data and combine into single df
for(i in 1:length(spp.fold)){
  print(spp.fold[i])
  dens.yr.loc <- read_csv(paste0("Data/VAST_output/", spp.fold[i]))
  spp.name <- spp.names[i]
  
  ### Merge with knot_area
  dens.yr.loc <- dens.yr.loc %>%
    inner_join(knot_locs, by=c("E_km", "N_km")) %>%
    mutate(TotalBio=Area_km2 * Density, 
           spp_common=spp.name,
           year=Year, #to match old code with lowercase year
           LANDING_YEAR=Year) #for later join with PACFIN landings

  spp.list[[i]] <- dens.yr.loc
}


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
# = Distribution of Biomass (kg) across Space =
# =====================================
region.bio <- cc.lim %>%
  group_by(spp_common, year) %>%
  summarise(regionBio=sum(TotalBio))


cc.lim2 <- cc.lim %>%
  inner_join(region.bio, by=c("spp_common", "year"))%>%
  mutate(relBio=TotalBio/regionBio)


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


dtspling <- bind_rows(dtsp.bio, ling.overall)




# =====================================
# = Merge with distribution =
# =====================================
dtspling.dist <- merge(dtspling, cc.lim2, by=c("year", "spp_common"))

### Calculate stock biomass in each knot by multiplying relative biomass across space by total SSB from assessment
dtspling.dist <- dtspling.dist %>%
  mutate(StockBio=relBio*Value)


# =====================================
# = COG and Stock Assessment Bio =
# =====================================
### COG by year
cog <- dtspling.dist%>%
  group_by(year, spp_common, Projected)%>%
  summarise(cog_N=weighted.mean(N_km, StockBio),
            log_assessBio=log(mean(Value)),
            SSB=mean(Value)) %>%
  mutate(SSB.thous=SSB/1000)

cog_p <- ggplot(cog, aes(x=year, y=cog_N, color=spp_common)) + geom_line()

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
  mutate(inv.dist= 1/total_dist ) #use inverse distance to weight biomass in calculating stock availability

### Diagnostics, making sure each species has the right number of observations
### Each of 5 species has 2000 observations in each year (500 knots * 4 ports)
with(cc_spp_dist, table(year, spp_common))
#length(unique(cc_spp_dist$year)) #23 years
#23*2000*5

# ==================================
# = Biomass Weighted by Inverse Distance =
# ==================================
cc_spp_bio_port <- cc_spp_dist %>%
  group_by(spp_common, port, Port_Name, LANDING_YEAR, year)%>%
  summarise(wtd.bio=weighted.mean(StockBio, w=inv.dist),
            port.N=round(mean(N_km_port))) %>%
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

port3 <- c("MRO", "BRG", "COS", "AST") #ordered by lat
port.col <- c( "gray", "darkorchid4", "black", "#008081")
port.pch <- c(19,1,3, 7)

exp.list <- seq(0,4, by=1)
land.exp.brks <- 10^(exp.list)
land.log.brks <- log(land.exp.brks)


bio.exp.brks <- c(1,5,10,25,50,100, 250,500,1000,2500, 5000,10000)
bio.log.brks <- log(bio.exp.brks)

ssb.exp.brks <- c(2500, 5000,10000, 20000, 50000, 100000, 200000, 500000)
ssb.log.brks <- log(ssb.exp.brks)
ssb.exp.brks.thous <- round(ssb.exp.brks/1000)




# ==================================
# = Plot availability to each port by species =
# ==================================
p <- ggplot(cc_spp_bio_port, aes(x=year, y=wtd.bio, color=Pcid))+
  geom_line() + facet_wrap(~spp_common, scales="free_y")


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


###################### FIGURE 3 ######################
library(data.table)
png("Figures/logStockBiobySpp_DTSPling.png", height=8, width=6, units="in", res=300)
par(mfrow=c(3,2), mar=c(4,4,2,2))
as.data.table(cc_spp_bio_port)[port %in% port3,j={
  t.dt <- .SD
  plot(log(wtd.bio) ~ year, t.dt, col="white",  
       main=paste0(unique(spp_common)), ylab="Stock Availability (mt)", yaxt="n")
  axis(side=2, at=bio.log.brks, labels=bio.exp.brks, las=1)
  for(i in 1:length(port3)){
    sub <- t.dt[port == port3[i]]
    points(log(wtd.bio) ~ year, sub, type="o", col=port.col[i], pch=port.pch[i], lwd=1)
  }
  abline(h=0, lty=2)
  abline(v=2003, col="grey", lty=2)
}, by=list(spp_common)]

plot(states.wcoast.utm, xlim=c(200,600), ylim=c(3800,5200))
# points(Y ~ X, ports_lim[PACFIN_PORT_CODE %in% port.select], col="black", pch=20)
# text(x=ports_lim[PACFIN_PORT_CODE %in% port.select]$X-150, 
#      y=ports_lim[PACFIN_PORT_CODE %in% port.select]$Y, 
#      labels=ports_lim[PACFIN_PORT_CODE %in% port.select]$PACFIN_PORT_CODE)
# #plot(x=c(0,1), y=c(0,1), col="white", axes=F, xlab="", ylab="")
legend("topleft", legend=rev(port3), lty=1, pch=rev(port.pch), col=rev(port.col), xpd=T, lwd=1.5, bty="n")
dev.off()


########################## FIGURE 2 ##########################
### Will want to make different symbol for whether SSB is projected or measured (eg is date later than stock assessment)
cog2 <- as.data.table(cog)

png("Figures/RegionBioCOG_DTSPling.png", height=5, width=8, units="in", res=300)
par(mfrow=c(1,2))
plot(log_assessBio ~ year,cog2, col="white", ylab="Assessed Spawning Biomass (thousand mt)", yaxt="n")
axis(side=2, at=ssb.log.brks, labels=ssb.exp.brks.thous, las=1)
abline(v=2003, col="grey", lty=2)
for(i in 1:length(dtspling.spp)){
  sub <- cog2[spp_common==dtspling.spp[i]]
  points(log_assessBio  ~ year, sub, col=dts.color[i], pch=dts.pch[i], type="o")
}

legend(1980, 14.5, ncol=3, legend=dtspling.spp, pch=dts.pch, col=dts.color, xpd=NA, bty="n")

plot(cog_N ~ year,cog2, col="white", ylab="Center of Gravity (km)", las=1)
#out <- matrix(nrow=length(unique(cog2$year)), ncol=length(dtspling.spp))
abline(v=2003, col="grey", lty=2)
for(i in 1:length(dtspling.spp)){
  sub <- cog2[spp_common==dtspling.spp[i]]
  points(cog_N ~ year, sub, col=dts.color[i], pch=dts.pch[i], type="o")
  # temp <- smooth.spline(x=sub$year, y=sub$cog_anom)
  # out[,i] <- temp$y
  # points(unique(cog2$year), out[,i], type="l", col=dts.color[i])
}
dev.off()

