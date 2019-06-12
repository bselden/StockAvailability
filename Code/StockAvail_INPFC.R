# Objective
### Integrate spawning stock biomass from assessments with distribution estimates from VAST
### Calculate availability to each INPFC region
### Calculate availability to port based on port-specific quantiles of distance traveled when landing DTSPL
### Calculate landings per vessel (and/or landings per fish ticket) as measure of CPUE
### Examine relationship between landings and availability

library(data.table)
library(sp)
library(rgdal)
library(maptools)
library(PBSmapping)
library(raster)
library(rasterVis)
library(latticeExtra)
library(tidyverse)

### Availability to INPFC regions ###
##########################
# =============================
# = VAST Density Output =
# =============================
#fname <- "Data/VAST_density_1April19/" #all from this date, except sablefish which is 2April2019
#the latest data appears to have output that differs from previous and from Thorson Pinsky Ward

fname <- "Data/Old_VAST/VAST_density_500/"
#fname <- "Data/VAST_output_v1/"
spp.fold <- list.files(fname)


#write.csv(data.table(spp_common=spp.fold), "thorson_spp.csv")

spp.list <- vector("list", length(spp.fold))
spp.names <- gsub(".csv", "", gsub("Dens_DF_", "", spp.fold))


yrs <- seq(1977,2017)

### Get area in each knot from sablefish
knot_locs <- as.data.table(read.csv("Data/Old_VAST/VAST_knots_500/Knot_area_km2_sablefish.csv"))
knot_locs[,"knot_num":=seq(1:length(E_km))]

#### Get knot location in lat lon
attr(knot_locs, "projection") <- "UTM"
attr(knot_locs, "zone") <- 10
knot_locs$X <- knot_locs$E_km
knot_locs$Y <- knot_locs$N_km
#knot_locs_LL <- rename(convUL(knot_locs), Lon=X, Lat=Y)
knot_locs[,"Lon":=convUL(knot_locs)$X]
knot_locs[,"Lat":=convUL(knot_locs)$Y]




### Import VAST data and combine into single df
for(i in 1:length(spp.fold)){
  print(spp.fold[i])
  dens.yr <- as.data.table(read.csv(paste0(fname, spp.fold[i])))
  spp.name <- spp.names[i]
  
  ### Merge with knot_area (only of knots with Area_km2>0)
  dens.yr.loc <- merge(dens.yr, knot_locs[Area_km2>0], by=c("E_km", "N_km"))
  
  dens.yr.out <- dens.yr.loc[,
                             list(Density_kgkm2=exp(Density), #Original Density output in VAST is log(Density),
                                  spp_common=spp.name,
                                  year=Year,
                                  LANDING_YEAR=Year),
                             by=list(E_km, N_km, knot_num, Area_km2, Lon, Lat)] 
  dens.yr.out[,"KnotBio":=Area_km2*Density_kgkm2]
  
  spp.list[[i]] <- dens.yr.out
}

cc <- rbindlist(spp.list)

# =====================================
# = Remove years without trawl survey =
# =====================================
tri.yrs <- seq(1980, 2004, by=3)
ann.yrs <- seq(2003,2018)

surv.yrs <- c(tri.yrs, ann.yrs)


cc.lim <- cc[year %in% surv.yrs]





# =====================================
# = Distribution of Biomass (kg) across Space =
# =====================================
region.bio <- cc.lim[,list(regionBio=sum(KnotBio)), by=list(spp_common, year)]

cc.lim2 <- merge(cc.lim, region.bio, by=c("spp_common", "year"))

cc.lim2[,"relBio":=KnotBio/regionBio]


# =====================================
# = Couple with spawning stock biomass from assessment =
# =====================================
### Units= metric tons of spawning biomass
dover <- as.data.table(read.csv("Data/AssessmentSPB/Dover_2015_SPB.csv"))
dover[,"spp_common":="Dover sole"]



sable <- as.data.table(read.csv("Data/AssessmentSPB/Sablefish_2015_SPB.csv"))
sable[,"spp_common":="sablefish"]

petrale <- as.data.table(read.csv("Data/AssessmentSPB/Petrale_2015_SPB.csv"))
petrale[,"spp_common":="petrale sole"]

short <- as.data.table(read.csv("Data/AssessmentSPB/Shortspine_2013_SPB.csv"))
short[,"spp_common":="shortspine thornyhead"]



dtsp.bio <- rbind(dover, sable, petrale, short)
dtsp.bio[,"year":=as.numeric(gsub("SPB_", "", LABEL))]
dtsp.bio[,"LABEL":=NULL]



lingS <- as.data.table(read.csv("Data/AssessmentSPB/Lingcod_South_2017_SPB.csv"))
lingN <- as.data.table(read.csv("Data/AssessmentSPB/Lingcod_North_2017_SPB.csv"))
lingS[,"spp_common":="lingcod"]
lingN[,"spp_common":="lingcod"]
lingS[,"region":="South"]
lingN[,"region":="North"]

ling <- rbind(lingS, lingN)
ling[,"year":=as.numeric(gsub("SPB_", "", LABEL))]

ling.overall <- ling[,list(Value=sum(Value), StdDev=sqrt(sum(StdDev^2))), by=list(year, spp_common)]


dtspling <- rbind(dtsp.bio, ling.overall)

# ==================================
# = Plot Settings =
# ==================================

### Log and exp axes


exp.list <- seq(0,4, by=1)
land.exp.brks <- 10^(exp.list)
land.log.brks <- log(land.exp.brks)


bio.exp.brks <- c(1,5,10,25,50,100, 250,500,1000,2500, 5000,10000)
bio.log.brks <- log(bio.exp.brks)

ssb.exp.brks <- c(2500, 5000,10000, 20000, 50000, 100000, 200000, 500000)
ssb.log.brks <- log(ssb.exp.brks)
ssb.exp.brks.thous <- round(ssb.exp.brks/1000)

#Colors, pch
dtspling.spp <- unique(dtspling$spp_common)
library(RColorBrewer)
dts.color <- brewer.pal(n=9, "RdGy")
dts.color <- dts.color[c(1:3, 7,9)]
#dts.pch <- c(0,15,1,16,2,17)
dts.pch <- c(19,3,15,7,17) #to match ggplot in fig 4

inpfc.col <- brewer.pal(n=6, "YlGnBu")[2:6]
inpfc.pch <- c(15,1,16,2,17)





# =====================================
# = Merge with distribution =
# =====================================
dtspling.dist <- merge(dtspling, cc.lim2, by=c("year", "spp_common"))

### Calculate stock biomass in each knot by multiplying relative biomass across space by total SSB from assessment
dtspling.dist[,"StockBio":=relBio*Value]







# ===================
# = State Shapefile =
# ===================
states <- readOGR("GIS_Data/cb_2016_us_state_500k", "cb_2016_us_state_500k")
states.data <- states@data

proj.states <- proj4string(states)

states.wcoast <- states[states$NAME %in% c("Washington", "California", "Oregon"),]

states.wcoast.utm <- spTransform(states.wcoast, "+proj=utm +zone=10 +ellps=GRS80 +datum=NAD83 +units=km +no_defs")


### INPFC boundaries
# http://www.pcouncil.org/wp-content/uploads/georock.pdf
inpfc_lat <- c(30,36,40.5,43,47.5,50)
inpfc_code <- c("CP", "MT", "EK", "CL", "VN")
inpfc_labs <- c("Conception", "Monterey", "Eureka", "Columbia", "Vancouver")

inpfc_lines <- data.frame(lat=c(36,40.5,43,47.5),
                          lat_end=c(36,40.5,43,47.5),
                          lon=rep(-127,4),
                          lon_end=rep(-121,4))

inpfc_text <- data.frame(lat=c(35, 38, 42, 45, 48),
                         lon=rep(-126.5, 5),
                         inpfc=inpfc_code)

# ===================
# = Plot map of StockBio=
# ===================
######################
### Test rasterFromXYZ with sablefish stock biomass --MEAN
knot.r <- raster(res=0.25,
                 xmn=min(knot_locs$Lon), xmx=max(knot_locs$Lon), 
                 ymn=min(knot_locs$Lat), ymx=max(knot_locs$Lat),
                 vals=NULL)

sable1980.r2 <- rasterize(x=dtspling.dist[spp_common=="sablefish" & year==1980][,c("Lon", "Lat")],
                          y=knot.r,
                          field=(dtspling.dist[spp_common=="sablefish" & year==1980]$StockBio),
                          fun=mean)

sable1992.r2 <- rasterize(x=dtspling.dist[spp_common=="sablefish" & year==1992][,c("Lon", "Lat")],
                          y=knot.r,
                          field=(dtspling.dist[spp_common=="sablefish" & year==1992]$StockBio),
                          fun=mean)

sable2013.r2 <- rasterize(x=dtspling.dist[spp_common=="sablefish" & year==2013][,c("Lon", "Lat")],
                          y=knot.r,
                          field=(dtspling.dist[spp_common=="sablefish" & year==2013]$StockBio),
                          fun=mean)
### Stack desired rasters
s <- stack(sable1980.r2, sable1992.r2, sable2013.r2)
names(s) <- c("1980", "1992", "2013")

myTheme=rasterTheme(region=rev(brewer.pal('Spectral', n=9)))

png("Figures/sable_StockBio_raster.png", height=5, width=6, units="in", res=300)
levelplot(s, layout=c(3,1), names.attr=c("1980", "1992", "2013"), main="Sablefish",
          par.settings=myTheme)  +
  latticeExtra::layer(panel.abline(h=c(47.5,43,40.5,36), lty=1, col="black")) +
  latticeExtra::layer(sp.polygons(states.wcoast, fill="lightgray"))+
  latticeExtra::layer(panel.text(x=rep(-125.5,5), y=c(48,45,42,38,35), rev(inpfc_code), cex=0.75))
dev.off()




# ===================
# = Mean biomass in each INPFC area=
# ===================
dtspling.dist[,"inpfc":=cut(Lat, breaks=inpfc_lat, labels=inpfc_code)]

with(dtspling.dist, table(Lat_grid, inpfc))
with(dtspling.dist, table(inpfc))


inpfc.bio <- dtspling.dist[,
                           list(mean.StockBio=mean(StockBio)),
                           by=list(spp_common, inpfc, year, LANDING_YEAR)]

png("Figures/sable_inpfc.png", height=5, width=5, units="in", res=300)
plot(mean.StockBio ~ year, inpfc.bio[spp_common=="sablefish"], col="white")
for(i in 1:length(inpfc_code)){
  points(mean.StockBio ~ year, inpfc.bio[spp_common=="sablefish" & inpfc==inpfc_code[i]],
         type="o", col=inpfc.col[i], pch=inpfc.pch[i])
}
legend("topright", legend=rev(inpfc_code), col=rev(inpfc.col), pch=rev(inpfc.pch), bty="n")
dev.off()

png("Figures/Fig2_dtspl_bio_inpfc.png", height=8, width=6, units="in", res=300)
par(mfrow=c(3,2), mar=c(4,4,2,2))
for(j in 1:length(dtspling.spp)){
  plot(mean.StockBio ~ year, inpfc.bio[spp_common==dtspling.spp[j]], col="white",
       main=dtspling.spp[j], ylab="Mean Biomass (mt)")
  for(i in 1:length(inpfc_code)){
    points(mean.StockBio ~ year, inpfc.bio[spp_common==dtspling.spp[j] & inpfc==inpfc_code[i]],
           type="o", col=inpfc.col[i], pch=inpfc.pch[i])
  }
}

plot(states.wcoast, col="white")
#abline(h=c(47.5,43,40.5,36))
segments(x0=rep(-130, 5), x1=rep(-120, 5), y0=c(47.5,43,40.5,36))
plot(states.wcoast, add=T, col="gray")
text(x=rep(-126.5,5), y=c(48.5,45,42,38,35), rev(inpfc_code))
legend("topright", legend=rev(inpfc_code), 
       col=rev(inpfc.col), pch=rev(inpfc.pch), cex=1.5,
       box.col="white")
dev.off()


##########################
### Stock Biomass and COG

stock_cog <- dtspling.dist[,list(spawning=mean(Value),
                                 cog=weighted.mean(Lat, Density_kgkm2)),
                           by=list(spp_common, year)]
stock_cog[,"log_assessBio":=log(spawning)]

png("Figures/Fig1_RegionBioCOG_DTSPling_INPFC.png", height=5, width=8, units="in", res=300)
par(mfrow=c(1,2))
plot(log_assessBio ~ year,stock_cog, col="white", ylab="Assessed Spawning Biomass (thousand mt)", yaxt="n")
axis(side=2, at=ssb.log.brks, labels=ssb.exp.brks.thous, las=1)
abline(v=2003, col="grey", lty=2)
for(i in 1:length(dtspling.spp)){
  sub <- stock_cog[spp_common==dtspling.spp[i]]
  points(log_assessBio  ~ year, sub, col=dts.color[i], pch=dts.pch[i], type="o")
}

legend(1980, 14.5, ncol=3, legend=dtspling.spp, pch=dts.pch, col=dts.color, xpd=NA, bty="n")

plot(cog ~ year,stock_cog, col="white", ylab="Center of Gravity (Latitude)", las=1)
#out <- matrix(nrow=length(unique(cog2$year)), ncol=length(dtspling.spp))
abline(v=2003, col="grey", lty=2)
for(i in 1:length(dtspling.spp)){
  sub <- stock_cog[spp_common==dtspling.spp[i]]
  points(cog ~ year, sub, col=dts.color[i], pch=dts.pch[i], type="o")
  # temp <- smooth.spline(x=sub$year, y=sub$cog_anom)
  # out[,i] <- temp$y
  # points(unique(cog2$year), out[,i], type="l", col=dts.color[i])
}
dev.off()



# =====================================
# = Plot stock biomass spatially
# =====================================
states.df <- broom::tidy(states.wcoast)
#a <- ggplot(data = states.df, aes(x = long, y = lat, group = group)) + geom_path()

all_states <- map_data("state")
west_states <- subset(all_states, region %in% c("california", "oregon", "washington"))

############### FIGURE 4 ####################

compare.stockBio.map <- function(dat_knot, dat_cog, spp, yrs){
  zlims <- c(min(dat_knot[spp_common==spp]$StockBio), max(dat_knot[spp_common==spp]$StockBio))
  # a <- ggplot(data=dat_knot[spp_common==spp & year==yrs[1]], aes(x=E_km, y=N_km))+ labs(title=paste0(yrs[1], " ", spp))
  # b <- a + geom_point(aes(color=StockBio)) + 
  #   scale_color_distiller(palette="Spectral", limits=zlims) + 
  #   guides(color = "none")+ 
  #   geom_hline(aes(yintercept=dat_cog[spp_common==spp & year==yrs[1]]$cog_N), lty=2)
  
  # a <- ggplot(data = states.df, aes(x = long, y = lat)) +
  #   geom_path(aes(group=group)) +
  #   labs(title=paste0(yrs[1], " ", spp))
  a <- ggplot()+ labs(title=paste0(yrs[1], " ", spp))
  b <- a + geom_point(data=dat_knot[spp_common==spp & year==yrs[1]], 
                      aes(x=Lon, y=Lat, color=StockBio), size=0.5) + 
    scale_color_distiller(palette="Spectral", limits=zlims) + 
    scale_x_continuous(limits=c(-127,-119))+
    guides(color = "none")+ 
    #geom_hline(aes(yintercept=dat_cog[spp_common==spp & year==yrs[1]]$cog), lty=2)+
    geom_segment(data=inpfc_lines, aes(x=lon, xend=lon_end, y=lat, yend=lat_end), lty=1)+
    geom_text(data=inpfc_text, aes(x=lon, y=lat, label=inpfc))
  # b2 <- b + geom_point(data=port_locs[port %in% port3], aes(x=Lon, y=Lat))+
  #   geom_text(data=port_locs[port %in% port3], aes(x=easting, y=northing, label=port),hjust=-0.15, vjust=0)
  #b2 <- b+ geom_polygon(aes(group = group), fill="gray", col="black", size=0.5)
  #b2 <- b + geom_sf(data=west_states)
  
  c <- ggplot() + 
    labs(title=paste0(yrs[2], " ", spp))
  d <- c + geom_point(data=dat_knot[spp_common==spp & year==yrs[2]], 
                      aes(x=Lon, y=Lat, color=StockBio), size=0.5) + 
    scale_color_distiller(palette="Spectral", limits=zlims) + 
    scale_x_continuous(limits=c(-127,-119))+
    guides(color = "none")+ 
    #geom_hline(aes(yintercept=dat_cog[spp_common==spp & year==yrs[2]]$cog), lty=2)+
    geom_segment(data=inpfc_lines, aes(x=lon, xend=lon_end, y=lat, yend=lat_end), lty=1)+
    geom_text(data=inpfc_text, aes(x=lon, y=lat, label=inpfc))
  
  #d2 <- d+ geom_polygon(aes(group = group), fill="gray", col="black")
  
  
  # d2 <- d + geom_point(data=port_locs[port %in% port3], aes(x=easting, y=northing))+
  #   geom_text(data=port_locs[port %in% port3], aes(x=easting, y=northing, label=port),hjust=-0.15, vjust=0)
  
  
  e <- ggplot() +
    labs(title=paste0(yrs[3], " ", spp))
  f <- e + geom_point(data=dat_knot[spp_common==spp & year==yrs[3]], 
                      aes(x=Lon, y=Lat, color=StockBio), size=0.5) + 
    scale_color_distiller(palette="Spectral", limits=zlims) + 
    scale_x_continuous(limits=c(-127,-119))+
    #geom_hline(aes(yintercept=dat_cog[spp_common==spp & year==yrs[3]]$cog), lty=2)+
    geom_segment(data=inpfc_lines, aes(x=lon, xend=lon_end, y=lat, yend=lat_end), lty=1)+
    geom_text(data=inpfc_text, aes(x=lon, y=lat, label=inpfc))
  
  # f2 <- f + geom_point(data=port_locs[port %in% port3], aes(x=easting, y=northing))+
  #   geom_text(data=port_locs[port %in% port3], aes(x=easting, y=northing, label=port),hjust=-0.15, vjust=0)
  #f2 <- f+ geom_polygon(aes(group = group), fill="gray", col="black")
  
  
  library(cowplot)
  figname <- paste0("Figures/StockDistMaps/", spp, "StockBio_map.png")
  fig <- ggdraw(plot_grid(b,d,f, ncol=3, rel_widths=c(0.7,0.7,1)))
  ggsave(figname, fig, width=10, height=5, units="in")
}

### Figure 3
compare.stockBio.map(dtspling.dist,stock_cog, "sablefish", c(1980, 1992, 2008))
#compare.stockBio.map(dtspling.dist,cog, "Dover sole", c(1986, 1998, 2010))
#compare.stockBio.map(dtspling.dist,cog, "lingcod", c(1980, 1995, 2010))


###########################
### Port-specific trends
library(RColorBrewer)
port_col <- brewer.pal(n=9, "PRGn")[c(1:4,6:9)]
port_pch <- c(7, 10, 16, 17, 15, 18, 19, 5)



ports_loc <- as.data.table(read.csv("Data/ports.lim.loc.csv"))
ports_loc[,"X":=Lon]
ports_loc[,"Y":=Lat]
attr(ports_loc, "projection") <- "LL"
ports_utm <- convUL(ports_loc)


ports_rad <- merge(ports_utm, logbook_quant, by.x="Pcid", by.y="RPCID")
setorder(ports_rad, -Lat)
ports_vec <- ports_rad$Pcid





### Import logbook output
logbook_quant_yr <- readRDS("Data/logbook_quantile_dist_yr.rds")
logbook_quant <- readRDS("Data/logbook_quantile_dist.rds")
logbook_raw <- readRDS("Data/logbook_rawdist.rds")

logbook_quant_yr[,"port":=factor(RPCID, ports_vec)]

png("Figures/FigS2_DistancePort_boxplot.png", height=5, width=7, units="in", res=300)
ggplot(logbook_quant_yr, aes(x=port, y=q75, fill=port))+geom_boxplot()+
  scale_fill_manual(values=port_col)+
  labs(y="75th quantile of \nDistance from Port (km)\nWeighted by Catch")
dev.off()




##############
#Sum biomass within radius of port

# ==================================
# = Distance between Port and Knot =
# ==================================

port_locs_only <- ports_rad[,list(port=Pcid, easting=X, northing=Y, q50=q50, q75=q75)]

# port_name <- ports_utm$Pcid
# port_easting <-ports_utm$X
# port_northing <- ports_utm$Y

dist_port <- knot_locs[,j={
  t.dt <- .SD
  
  temp <- copy(port_locs_only)
  
  temp[,"dist.E":=t.dt$E_km - easting] # knot easting - port easting  
  temp[,"dist.N":= t.dt$N_km - northing ] #knot northing - port northing  
  list(port=temp$port, port.N=round(temp$northing), 
       dist.E=temp$dist.E, dist.N=temp$dist.N,
       q50=temp$q50, q75=temp$q75)
  
}, by=list(knot_num)]

dist_port[,"total_dist":=sqrt((dist.E)^2 + (dist.N)^2)]

knots_within_rad <- dist_port[total_dist <= q75]

### Merge with dtspling.dist
dtspling.dist.port <- merge(dtspling.dist, knots_within_rad,
                            by="knot_num")

### Sum stock biomass within radii of each port
stock_port <- dtspling.dist.port[,list(StockBio=sum(StockBio)),
                                 by=list(port, year, spp_common)]



### By species
png("Figures/Fig5_port_bio.png", height=8, width=6, units="in", res=300)
par(mfrow=c(3,2), mar=c(4,4,2,2))
for(i in 1:length(dtspling.spp)){
  plot(StockBio/1000 ~ year,stock_port[spp_common==dtspling.spp[i]], col="white",
       main=dtspling.spp[i], ylab="Available Biomass (thousand mt)")
  for(j in 1:length(ports_vec)){
    points(StockBio/1000 ~ year, stock_port[port==ports_vec[j] & spp_common==dtspling.spp[i]],
           type="o", col=port_col[j], pch=port_pch[j])
  }
}

plot(states.wcoast.utm)
points(Y  ~ X, ports_rad, pch=port_pch, col=port_col)
symbols(x=ports_rad$X, y=ports_rad$Y, circles=ports_rad$q75, inches=F, add=T, fg=port_col)
legend("topright", legend=ports_vec, pch=port_pch, col=port_col, bty="n", cex=1.5)
dev.off()



###############################################
### Knot locs inside/outside of RCAs
### 2015-2016
rca1516 <- readOGR("GIS_Data/CommTrawlRCA_2015-16/", "RCA_CommTrawl_2015_16_poly")

rca1516_utm <- spTransform(rca1516, crs(states.wcoast.utm))




png("Figures/Fig4_port_radii_wRCA.png", height=8, width=6, units="in", res=300)
plot(states.wcoast.utm)
#plot(rca1516_utm,  col="orange", add=T, border=NULL)
points(N_km ~ E_km, subset(knot_locs_df2,closed==0), cex=0.3, col="black", pch=16)
points(N_km ~ E_km, subset(knot_locs_df2,closed==1), cex=0.3,  col="red", pch=16)
points(Y  ~ X, ports_rad, pch=port_pch, col=port_col, cex=0.8)
symbols(x=ports_rad$X, y=ports_rad$Y, circles=ports_rad$q75, inches=F, 
        add=T, fg=port_col, lwd=2)
legend("left", legend=ports_vec, pch=port_pch, col=port_col, bty="n")
legend("bottomleft", legend=c("RCA", "Open"), col=c("red", "black"), pch=16, bty="n")
dev.off()


# =====================================
# = Knots within Closed Areas =
# =====================================


coordinates(knot_locs)<- cbind(knot_locs$X, knot_locs$Y)
proj4string(knot_locs) <- crs(rca1516_utm)

knots_inrca <- sp::over(knot_locs, rca1516_utm)
knots_inrca$knot_num <- seq(1:500)
knots_inrca$closed <- ifelse(is.na(knots_inrca$region_id), 0, 1)

knot_locs_df <- as.data.frame(knot_locs)
knot_locs_df2 <- merge(knot_locs_df, knots_inrca, by=c("knot_num"))




### Number of knots in closed areas
sum(knot_locs_df2$closed)

dtspling.port.rca <- merge(dtspling.dist.port[year %in% c(2015)], knot_locs_df2[,c("E_km", "N_km", "knot_num", "Area_km2", "closed")],
                           by=c("E_km", "N_km", "knot_num", "Area_km2"))

dtspling.port.closed <- dtspling.port.rca[,list(StockBio_closed=sum(StockBio[closed==1]),
                                                StockBio=sum(StockBio)),
                                          by=list(spp_common, port)]
dtspling.port.closed[,"frac.closed":=StockBio_closed/StockBio]
dtspling.port.closed[, "port" := factor(port, levels = ports_vec)]
dtspling.port.closed[, "spp_common":=factor(spp_common, levels=dtspling.spp)]


png("Figures/Fig6_FracBio_closed.png", height=8, width=8, units="in", res=300)
a <- ggplot(dtspling.port.closed, aes(x=port, y=frac.closed, fill=port))+
  geom_col()+ facet_wrap(~spp_common, nrow=3) + 
  labs(y="Fraction of Available Biomass in RCA") +
  scale_fill_manual(values=port_col) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
a
dev.off()

###################################################
### Landings vs. Availability
### Import PACFIN output
load("Data/pacfin_output.Rdata", verbose=T)
#top ports had > 30000 mt of these species and were present 1981-2017

#Summarize landings for species with more than one species code
dtspl.land2 <- merge(dtspl.land.portlim, dtspl.lookup, 
                     by=c("PACFIN_SPECIES_CODE"))





# Limit to just years in survey and sum across codes for a species
dtspl.land.yr <- dtspl.land2[!(spp %in% c("longspine thornyhead", "thornyheads")) & LANDING_YEAR %in% surv.yrs,
                             list(mtons=sum(mtons, na.rm=T),
                                   dollars=sum(dollars, na.rm=T),
                                   frac.catch=sum(frac.catch, na.rm=T),
                                   frac.rev=sum(frac.rev, na.rm=T),
                                  num.tix=sum(num.tix, na.rm=T),
                                  num.vess=sum(num.vess, na.rm=T),
                                  num.dealer=sum(num.dealer, na.rm=T)),
                             by=list(PACFIN_PORT_CODE, LANDING_YEAR, spp,
                                     total.tix, total.vess, total.dealer)]

setorder(dtspl.land.yr, PACFIN_PORT_CODE, LANDING_YEAR)
dtspl.land.yr[mtons==0]
# AST,  BRK, and COS shortspine thornyhead in 1983 and 1986
# CRS lingcod in 2011-13 (but fewer than 3 vessels in 2012 and 2013)
# CRS all species in 2015 (and no trawl vessels)
# MRO all species in 2010


### Limit to years with > 3 total trawl vessels in each port
dtspl.land.yr.lim <- dtspl.land.yr[total.vess>=3]


dtspl.land.yr.lim[,"port":=factor(PACFIN_PORT_CODE, ports_vec)]

### Importance of all DTSPL combined to each port
dtspl.land.combined <- dtspl.land.yr.lim[,list(frac.catch=sum(frac.catch, na.rm=T)),
                                         by=list(port, LANDING_YEAR)]

png("Figures/FigS1_DTSPL_dist_import.png", height=5, width=5, units="in", res=300)
a <- ggplot(logbook_quant_yr, aes(x=port, y=q75, fill=port))+geom_boxplot()+
  scale_fill_manual(values=port_col)+
  labs(y="75th quantile of \nDistance from Port (km)\nWeighted by Catch")+
  theme(legend.position = "none")

b <- ggplot(dtspl.land.combined, aes(x=port, y=frac.catch, fill=port))+
  geom_boxplot() + 
  #scale_y_continuous(limits = quantile(dtspl.land.yr.lim$frac.catch, c(0.1,0.9)))+
  scale_fill_manual(values=port_col)+
  labs(y="Fraction of Total Catch")+
  theme(legend.position = "none")
plot_grid(a, b, nrow=2, align="v")
dev.off()



### Merge with availability to port
### Exclude 1980 because landings records start in 1981 (so first year with both is 1983)
land_avail <- merge(stock_port[year>1980], dtspl.land.yr.lim,
                    by.x=c("year", "port", "spp_common"),
                    by.y=c("LANDING_YEAR", "PACFIN_PORT_CODE", "spp"))
land_avail[,"StockBio.thous.mtons":=StockBio/1000]
land_avail[,"land.thous.mtons":=mtons/1000]

mod_land_avail <- land_avail[,j={
  t.dt <- .SD
  mod <- lm(land.thous.mtons ~ StockBio.thous.mtons, t.dt)
  list(StockBio.est=summary(mod)$coefficients[2,1],
       StockBio.p=summary(mod)$coefficients[2,4],
       intercept.est=summary(mod)$coefficients[1,1])
}, by=list(spp_common, port)]

land_avail_wmod <- merge(land_avail, mod_land_avail, by=c("spp_common", "port"))
land_avail_wmod[,"predicted.land":=intercept.est + StockBio.est*StockBio.thous.mtons]

### By species
dev.new()
par(mfrow=c(3,2), mar=c(4,4,2,2))
for(i in 1:length(dtspling.spp)){
  plot(land.thous.mtons ~ StockBio.thous.mtons, land_avail_wmod[spp_common==dtspling.spp[i]],
       main=dtspling.spp[i], col="white", 
       xlab="Available Stock Biomass (thousand mt)", ylab="Annual Landings (thousand mt)")
  for(j in 1:length(ports_vec)){
    sub <- land_avail_wmod[spp_common==dtspling.spp[i] & port==ports_vec[j]]
    #sub2 <- mod_land_avail[spp_common==dtspling.spp[i] & port==ports_vec[j]]
    points(land.thous.mtons ~ StockBio.thous.mtons, sub,
           col=port_col[j], pch=port_pch[j])
    points(predicted.land ~ StockBio.thous.mtons, sub, type="l", col=port_col[j])
    #abline(a=sub2$intercept.est, b=sub2$StockBio.est, col=port_col[j])
  }
}



png("Figures/Fig7_landing_avail_port.png", height=8, width=8, units="in", res=300)
par(mfrow=c(3,3), mar=c(2,2,2,2), oma=c(2,2,4,4))
for(j in 1:length(ports_vec)){
  plot(land.thous.mtons ~ StockBio.thous.mtons, 
       land_avail_wmod[port==ports_vec[j] & spp_common !="Dover sole"], col="white",
       main=ports_vec[j], 
       xlab="", 
       ylab="")
  for(i in 2:length(dtspling.spp)){
    sub <- land_avail_wmod[spp_common==dtspling.spp[i] & port==ports_vec[j]]
    #sub2 <- mod_land_avail[spp_common==dtspling.spp[i] & port==ports_vec[j]]
    points(land.thous.mtons ~ StockBio.thous.mtons, sub,
           col=dts.color[i], pch=dts.pch[i])
    points(predicted.land ~ StockBio.thous.mtons, sub, 
           type="l", col=dts.color[i], lwd=2)
    #abline(a=sub2$intercept.est, b=sub2$StockBio.est, col=dts.color[i])
  }
}

### Position of legend is relative to last plot axes
legend(-8.5,2.2, horiz=T, cex=1.25,
       legend=dtspling.spp[2:5], col=dts.color[2:5], pch=dts.pch[2:5], 
       bty="n", xpd=NA)

plot(land.thous.mtons ~ StockBio.thous.mtons, 
     land_avail_wmod[spp_common=="Dover sole"],
     col="white", xlab="", ylab="", main="Dover sole")
for(j in 1:length(ports_vec)){
  sub2 <- land_avail_wmod[spp_common=="Dover sole" & port==ports_vec[j]]
  #sub2 <- mod_land_avail[spp_common==dtspling.spp[i] & port==ports_vec[j]]
  points(land.thous.mtons ~ StockBio.thous.mtons, sub2,
         col=port_col[j], pch=port_pch[j])
  points(predicted.land ~ StockBio.thous.mtons, sub2, type="l", col=port_col[j])
  #abline(a=sub2$intercept.est, b=sub2$StockBio.est, col=port_col[j])
}



# Position of legend is relative to last plot axes
legend(200,5, cex=1.25,
       legend=ports_vec, col=port_col, pch=port_pch, bty="n", xpd=NA)

mtext("Available Stock Biomass (thousand mt)", side=1,line=1,  outer=T)
mtext("Annual Landings (thousand mt)", side=2, line=0.5, outer=T)
dev.off()
