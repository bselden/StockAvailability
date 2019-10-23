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
library(geosphere)
library(forecast)





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
dover <- as.data.table(read.csv("Data/AssessmentSPB/Dover_2011_SPB.csv"))
dover[,"spp_common":="Dover sole"]
dover[,"report":=2011]
dover[,"Year":=as.numeric(gsub("SPB_", "", LABEL))]
#remove estimates from 2011 report that are >2013
dover <- dover[Year<2013]


# use spawning biomass estimates from 2015 report to council
# https://www.pcouncil.org/groundfish/stock-assessments/by-species/dover-sole/
dover15 <- as.data.table(read.csv("Data/AssessmentSPB/DoverProjections_2015Council.csv"))

dover15[,"LABEL":=paste0("SPB_",Year)]
dover15[,"Value":=Spawning.biomass]
dover15[,"StdDev":=NA]
dover15[,"spp_common":="Dover sole"]
dover15[,"report":=2015]
dover15[,c("OFL", "Catch..mt.", "Depletion", "Spawning.biomass"):=NULL]

dover.all <- rbind(dover, dover15)
dover.all[,"Year":=NULL]



sable <- as.data.table(read.csv("Data/AssessmentSPB/Sablefish_2015_SPB.csv"))
sable[,"spp_common":="sablefish"]
sable[,"report":=2015]

petrale <- as.data.table(read.csv("Data/AssessmentSPB/Petrale_2015_SPB.csv"))
petrale[,"spp_common":="petrale sole"]
petrale[,"report":=2015]

short <- as.data.table(read.csv("Data/AssessmentSPB/Shortspine_2013_SPB.csv"))
short[,"spp_common":="shortspine thornyhead"]
short[,"report":=2013]



dtsp.bio <- rbind(dover.all, sable, petrale, short)
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

ling.overall <- ling[year<=2028,list(Value=sum(Value), StdDev=sqrt(sum(StdDev^2))), by=list(year, spp_common)]
ling.overall[,"report":=2017]


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

inpfc.col <- brewer.pal(n=6, "RdGy")[c(1:2, 4:6)]
inpfc.pch <- c(15,1,21,2,17)
inpfc.bg <- c(NA, NA, inpfc.col[3], NA,NA)
inpfc.col <- c(inpfc.col[1:2], "black", inpfc.col[4:5])





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

# ===================
# = Depth Information =
# ===================
### Load bathymetry data
depth <- raster("GIS_Data/etopo1_CAcurrent.nc")
plot(depth, zlim=c(-500,0))


### query depths of knots
knot_locs_sp <- SpatialPoints(copy(knot_locs[,c("Lon", "Lat")]))
proj4string(knot_locs_sp) <- proj4string(depth)
depth.knot <- raster::extract(depth, knot_locs_sp, sp=T)
depth.knot.df <- as.data.table(as.data.frame(depth.knot))
knot_locs_depth <- merge(knot_locs, depth.knot.df, by=c("Lon", "Lat"))
knot_locs_depth[Altitude < -500]
# only two knots less than 500 m (both in the south)

plot(states.wcoast)
contour(depth, add=T, zlim=c(-500,-500), nlevels=1)
contour(depth, add=T, zlim=c(-1500,-1500), nlevels=1, col="gray")


### Depth preferences for our focal species
raw_trawl <- as.data.table(read.csv("Data/Groundfish_all2019-03-14.csv"))

png("Figures/Depth_pref.png", height=8, width=5, units="in", res=300)
par(mfrow=c(3,2))
raw_trawl[project=="Groundfish Slope and Shelf Combination Survey",j={
  hist(depth_m, main=common_name)
  abline(v=500)
  list("hist")
}, by=common_name]
dev.off()

# ===================
# = INPFC Boundaries =
# ===================

### INPFC boundaries
# http://www.pcouncil.org/wp-content/uploads/georock.pdf
inpfc_lat <- c(30,36,40.5,43,47.5,50)
inpfc_code <- c("CP", "MT", "EK", "CL", "VN")
inpfc_labs <- c("Conception", "Monterey", "Eureka", "Columbia", "Vancouver")

inpfc_lines <- data.frame(lat=c(36,40.5,43,47.5),
                          lat_end=c(36,40.5,43,47.5),
                          lon=rep(-128,4),
                          lon_end=rep(-121,4))

inpfc_text <- data.table(lat=c(35, 38, 42, 45, 48),
                         lon=rep(-127.5, 5),
                         inpfc=inpfc_code)

inpfc_ll_lines <- data.table(lat=c(36,40.5,43,47.5,36,40.5,43,47.5),
                              lon=c(rep(-128,4), rep(-121,4)),
                              type=c(rep("start",4), rep("end", 4)))
                              

inpfc_ll_lines[,"X":=lon]
inpfc_ll_lines[,"Y":=lat]
attr(inpfc_ll_lines, "projection") <- "LL"
inpfc_utm_lines <- convUL(inpfc_ll_lines)

inpfc_text[,"X":=lon]
inpfc_text[,"Y":=lat]
attr(inpfc_text, "projection") <- "LL"
inpfc_utm_text <- convUL(inpfc_text)

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

#####################
### Figure 4
png("Figures/Fig4_dtspl_bio_inpfc.png", height=8, width=6, units="in", res=300)
par(mfrow=c(3,2), mar=c(4,4,2,2))
for(j in 1:length(dtspling.spp)){
  plot(mean.StockBio ~ year, inpfc.bio[spp_common==dtspling.spp[j]], col="white",
       main=dtspling.spp[j], ylab="Mean Biomass (mt)", las=1)
  for(i in 1:length(inpfc_code)){
    points(mean.StockBio ~ year, inpfc.bio[spp_common==dtspling.spp[j] & inpfc==inpfc_code[i]],
           type="o", col=inpfc.col[i], bg=inpfc.bg[i], pch=inpfc.pch[i])
  }
}

plot(states.wcoast, col="white")
#abline(h=c(47.5,43,40.5,36))
segments(x0=rep(-130, 5), x1=rep(-120, 5), y0=c(47.5,43,40.5,36))
plot(states.wcoast, add=T, col="gray")
text(x=rep(-126.5,5), y=c(48.5,45,42,38,35), rev(inpfc_code))
legend("topright", legend=rev(inpfc_code), 
       col=rev(inpfc.col), pch=rev(inpfc.pch), pt.bg=rev(inpfc.bg), cex=1.5,
       box.col="white")
dev.off()


##########################
### Stock Biomass and COG

stock_cog <- dtspling.dist[,list(spawning=mean(Value),
                                 cog=weighted.mean(Lat, Density_kgkm2),
                                 cog_N=weighted.mean(N_km, Density_kgkm2)),
                           by=list(spp_common, year)]
stock_cog[,"log_assessBio":=log(spawning)]
stock_cog[,"spawn.thou":=spawning/1000]



## Just sable and dover
png("Figures/COG_sableDover_Packard.png", height=5, width=5, units="in", res=300)
par(mar=c(4,4,2,2))
plot(cog ~ year,stock_cog[spp_common=="sablefish"], ylab="Center of Gravity (Latitude)", las=1,
     col=dts.color[2], pch=dts.pch[2], type="o")
points(cog ~ year,stock_cog[spp_common=="Dover sole"], ylab="Center of Gravity (Latitude)", 
       col=dts.color[1], pch=dts.pch[1], type="o")
#abline(v=1998, lty=2)
#abline(v=2013, lty=3)
legend("topleft", legend=c("Dover", "sable"), pch=dts.pch[1:2], col=dts.color[1:2], bty="n")
dev.off()

png("Figures/COG_sable.png", height=4, width=4, units="in", res=300)
par(mar=c(4,4,2,2))
plot(cog ~ year,stock_cog[spp_common=="sablefish"], ylab="Center of Gravity (Latitude)", las=1,
     col="darkgrey", pch=17, type="o", main="sablefish", lwd=2)
abline(v=1998, lty=2)
abline(v=2013, lty=3)
dev.off()


png("Figures/RegionBioCOG_DTSPling_INPFC.png", height=5, width=8, units="in", res=300)
par(mfrow=c(1,2), oma=c(0,0,0,3))
plot(log_assessBio ~ year,stock_cog, col="white", ylab="Assessed Spawning Biomass (thousand mt)", yaxt="n")
axis(side=2, at=ssb.log.brks, labels=ssb.exp.brks.thous, las=1)
abline(v=2003, col="grey", lty=2)
for(i in 1:length(dtspling.spp)){
  sub <- stock_cog[spp_common==dtspling.spp[i]]
  points(log_assessBio  ~ year, sub, col=dts.color[i], pch=dts.pch[i], type="o")
}

legend(1980, 14.5, ncol=3, legend=dtspling.spp, pch=dts.pch, col=dts.color, xpd=NA, bty="n")


plot(cog ~ year,stock_cog, col="white", ylab="Center of Gravity (Latitude)", las=1)
polygon(x=c(2013, 2017, 2017, 2013, 2013), y=c(41,41,45,45, 41), col="gray90", border=NA)
#out <- matrix(nrow=length(unique(cog2$year)), ncol=length(dtspling.spp))
abline(v=2003, col="grey", lty=2)
for(i in 1:length(dtspling.spp)){
  sub <- stock_cog[spp_common==dtspling.spp[i]]
  points(cog ~ year, sub, col=dts.color[i], pch=dts.pch[i], type="o")
  # temp <- smooth.spline(x=sub$year, y=sub$cog_anom)
  # out[,i] <- temp$y
  # points(unique(cog2$year), out[,i], type="l", col=dts.color[i])
}
par(new = T)
plot(cog_N ~ year, stock_cog, col=NA, axes=F, xlab=NA, ylab=NA)
axis(side = 4, las=1)
mtext(side = 4, line = 3, 'Northings (km)')
dev.off()


################
### New Figure 2 separated by each species
png("Figures/Fig2_COGStock_byspp.png", height=8, width=6, units="in", res=300)
par(mfrow=c(3,2), mar=c(4,5,2,5.5))
for(i in 1:length(dtspling.spp)){
  sub <- stock_cog[spp_common==dtspling.spp[i]]
  ### Stock Bio
  plot(spawn.thou  ~ year, sub, col="black", pch=1, type="o",
       ylab="", main=dtspling.spp[i], las=1)
  mtext(side=2, line=2.5, "Assessed Spawning Biomass\n(thousand mt)", cex=0.8, col="black")
  abline(v=2003, col="grey", lty=2)
  
  par(new = T)
  plot(cog ~ year, sub, ylim=c(41.5, 44.5),
       col="darkgrey", axes=F, xlab=NA, ylab=NA, type="o", lty=1, pch=17)
  axis(side = 4, col="grey", col.ticks="darkgrey", col.axis="darkgrey", las=1)
  mtext(side = 4, line = 3, "Center of Gravity (Latitude)", cex=0.8, col="darkgrey")
  
}
dev.off()

# =====================================
# = Correlations in stock bio and cog 
# =====================================
### Are stocks further N or S depending on stock size (basin hypothesis?)
mod_bio_cog_sable <- lm(cog ~ log_assessBio,stock_cog[spp_common=="sablefish"])
mod_bio_cog_dover <- lm(cog ~ log_assessBio,stock_cog[spp_common=="Dover sole"])
mod_bio_cog_petrale <- lm(cog ~ log_assessBio,stock_cog[spp_common=="petrale sole"])
mod_bio_cog_thorny <- lm(cog ~ log_assessBio,stock_cog[spp_common=="shortspine thornyhead"])
mod_bio_cog_ling <- lm(cog ~ log_assessBio,stock_cog[spp_common=="lingcod"])



# =====================================
# = Anomalies in COG
# =====================================
# Maybe use Twitter's AnomalyDetection package?
# devtools::install_github("twitter/AnomalyDetection") 
# library(AnomalyDetection)

# OR Rob Hyndman’s forecast::tsoutliers() function available through forecast package

stock_cog[,"mean_cog":=mean(cog), by=list(spp_common)]
stock_cog[,"anom_cog":= cog - mean_cog]
stock_cog[,"median_cog":=median(cog), by=list(spp_common)]


boxplot(anom_cog_N ~ spp_common, stock_cog, notch=F)
abline(h=0, col="red")

# =====================================
# = Relative variance in species distributions 
# =====================================

### Do variances differ between species
# The four tests are:
# 1. Cochran’s test: compares the maximum within-sample variance to the average withinsample variance. 
# A P-value less than 0.05 indicates a significant difference amongst the
# within-sample standard deviations at the 5% significance level. The test is appropriate
# only if all group sizes are equal.
# 2. Bartlett’s test: compares a weighted average of the within-sample variances to their
# geometric mean. A P-value less than 0.05 indicates a significant difference amongst the
# within-sample standard deviations at the 5% significance level. The test is appropriate
# for both equal and unequal group sizes.
# 3. Hartley’s test: computes the ratio of the largest sample variance to the smallest sample
# variance. This statistic must be compared to a table of critical values, such as the one
# contained in Neter et al. (1996). For 6 samples and 62 degrees of freedom for
# experimental error, H would have to exceed approximately 2.1 to be statistically
# significant at the 5% significance level. Note: this test is only
# 4. Levene’s test: performs a one-way analysis of variance on the variables. 
# The tabulated statistic is the F statistic from the ANOVA table.

bartlett.test(cog ~ spp_common, stock_cog) #gives value for all species together, they are not all the same




### According to this https://stackoverflow.com/questions/43646987/multiple-comparison-post-hoc-test-for-levenes-test
### the Levene test is just a ANOVA on sample variance (or residuals) so can do on the residuals themselves
### IF we convert them to absolute values
### Technique corroborated by Boos and Brownie paper Boos, D. D., & Brownie, C. (2004). Comparing Variances and Other Measures of Dispersion. 
### Statistical Science: A Review Journal of the Institute of Mathematical Statistics, 19(4), 571–578.
### Mean absolute deviation from median (MDM) 
### subtracting off the median had better Type I and Type II errors
stock_cog[,"anom_cog_abs":= abs(cog- median_cog)]
boxplot(anom_cog_abs ~ spp_common, stock_cog)

levene_aov <- aov(anom_cog_abs ~ spp_common, stock_cog)
TukeyHSD(levene_aov) # significant differences in variance between thorny--Dover and thorny--sable

# =====================================
# = Significant difference in species distributions 
# =====================================
# Syrjala, S. E. (1996). A Statistical Test for a Difference between the Spatial Distributions of Two Populations. 
# Ecology, 77(1), 75–80.

# Long, J., & Robertson, C. (2018). Comparing spatial patterns. 
# Geography Compass, 12(2), e12356.

# =====================================
# = Correlation with PDO
# =====================================
pdo <- as.data.table(read.table("Data/PDO.latest.txt", skip=28, header=T, nrows=118))
pdo[,"year":=seq(1900,2017)]

pdo.m <- melt(pdo, id.vars="year", measure.vars=c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"))

pdo.annual <- pdo.m[,list(pdo=mean(value)), by=list(year)]
pdo.annual[,"sign":=as.factor(sign(pdo))]

png("Figures/PDO.png", height=5, width=5, units="in", res=300)
ggplot(pdo.annual[year>=1980], aes(x=year, y=pdo, fill=sign))+geom_col()+
  scale_fill_manual(values=c("blue", "red"))+theme(legend.position = "none")+
  labs(y="Pacific Decadal Oscillation") + geom_vline(xintercept=1998, lty=2) + geom_vline(xintercept=2013, lty=3)
dev.off()

# cog_pdo <- merge(stock_cog, pdo.annual, by=c("year"))
# 
# plot(cog ~ pdo, cog_pdo[spp_common=="sablefish"])


# =====================================
# = Plot stock biomass spatially
# =====================================
states.df <- broom::tidy(states.wcoast)
#a <- ggplot(data = states.df, aes(x = long, y = lat, group = group)) + geom_path()

all_states <- map_data("state")
west_states <- subset(all_states, region %in% c("california", "oregon", "washington"))

############### FIGURE 4 ####################
inpfc_utm_lines_cast <- data.table(X_start=inpfc_utm_lines[type=="start"]$X,
                                   X_end=inpfc_utm_lines[type=="end"]$X,
                                   Y_start=inpfc_utm_lines[type=="start"]$Y,
                                   Y_end=inpfc_utm_lines[type=="end"]$Y)

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
                      aes(x=Lon, y=Lat, color=StockBio), size=0.4) + 
    scale_color_distiller(palette="Spectral", limits=zlims) + 
    #scale_x_continuous(limits=c(-127,-119))+
    guides(color = "none")+ 
    geom_hline(aes(yintercept=dat_cog[spp_common==spp & year==yrs[1]]$cog), lty=2)+
    ylab("Latitude")+ xlab("")+
    theme(plot.margin = unit(c(0,0,0,0), "inches"))+ 
    geom_segment(data=inpfc_lines, aes(x=rep(-126,4), xend=lon_end, 
                                       y=lat, yend=lat_end), lty=1)+
    geom_text(data=inpfc_text, aes(x=rep(-122,5), y=lat, label=inpfc), size=3.25)
  # b2 <- b + geom_point(data=port_locs[port %in% port3], aes(x=Lon, y=Lat))+
  #   geom_text(data=port_locs[port %in% port3], aes(x=easting, y=northing, label=port),hjust=-0.15, vjust=0)
  #b2 <- b+ geom_polygon(aes(group = group), fill="gray", col="black", size=0.5)
  #b2 <- b + geom_sf(data=west_states)
  
  c <- ggplot() + 
    labs(title=paste0(yrs[2], " ", spp))
  d <- c + geom_point(data=dat_knot[spp_common==spp & year==yrs[2]], 
                      aes(x=Lon, y=Lat, color=StockBio), size=0.4) + 
    scale_color_distiller(palette="Spectral", limits=zlims) + 
    #scale_x_continuous(limits=c(-127,-119))+
    guides(color = "none")+ 
    geom_hline(aes(yintercept=dat_cog[spp_common==spp & year==yrs[2]]$cog), lty=2)+
    xlab("Longitude") + 
    theme(plot.margin = unit(c(0,0,0,0), "inches"), axis.title.y=element_blank())+ 
    geom_segment(data=inpfc_lines, aes(x=rep(-126,4), xend=lon_end, 
                                       y=lat, yend=lat_end), lty=1)+
    geom_text(data=inpfc_text, aes(x=rep(-122,5), y=lat, label=inpfc), size=3.25)
  
  #d2 <- d+ geom_polygon(aes(group = group), fill="gray", col="black")
  
  
  # d2 <- d + geom_point(data=port_locs[port %in% port3], aes(x=easting, y=northing))+
  #   geom_text(data=port_locs[port %in% port3], aes(x=easting, y=northing, label=port),hjust=-0.15, vjust=0)
  
  
  e <- ggplot() +
    labs(title=paste0(yrs[3], " ", spp))
  f <- e + geom_point(data=dat_knot[spp_common==spp & year==yrs[3]], 
                      aes(x=Lon, y=Lat, color=StockBio), size=0.4) + 
    scale_color_distiller(palette="Spectral", limits=zlims, name="Biomass\n(mt)") + 
    
    #scale_x_continuous(limits=c(-127,-119))+
    xlab("")+ 
    geom_hline(aes(yintercept=dat_cog[spp_common==spp & year==yrs[3]]$cog), lty=2)+
    theme(plot.margin = unit(c(0,0,0,0), "inches"), axis.title.y=element_blank())+ 
    geom_segment(data=inpfc_lines, aes(x=rep(-126,4), xend=lon_end, 
                                       y=lat, yend=lat_end), lty=1)+
    geom_text(data=inpfc_text, aes(x=rep(-122,5), y=lat, label=inpfc), size=3.25)
  
  # f2 <- f + geom_point(data=port_locs[port %in% port3], aes(x=easting, y=northing))+
  #   geom_text(data=port_locs[port %in% port3], aes(x=easting, y=northing, label=port),hjust=-0.15, vjust=0)
  #f2 <- f+ geom_polygon(aes(group = group), fill="gray", col="black")
  
  
  library(cowplot)
  figname <- paste0("Figures/StockDistMaps/", spp, "StockBio_map.png")
  fig <- ggdraw(plot_grid(b,d,f, ncol=3, rel_widths=c(0.8,0.7,1.1), align="h"))
  ggsave(figname, fig, width=8, height=4, units="in")
}

### Figure 3
compare.stockBio.map(dtspling.dist,stock_cog, "sablefish", c(1980, 1992, 2008))
#compare.stockBio.map(dtspling.dist,cog, "Dover sole", c(1986, 1998, 2010))
#compare.stockBio.map(dtspling.dist,cog, "lingcod", c(1980, 1995, 2010))


###########################
### Port-specific trends
ports_loc <- as.data.table(read.csv("Data/ports.lim.loc.csv"))
ports_loc[,"X":=Lon]
ports_loc[,"Y":=Lat]
ports_loc[,"print_X":=ifelse(Pcid == c("BLL", "AST"), X+1, X+2)]
ports_loc[,"print_Y":=ifelse(Pcid=="BRK", Y+0.25, 
                             ifelse(Pcid%in%c("CRS"), Y-0.25,
                                    ifelse(Pcid=="AST", Y-0.85, Y)))]
attr(ports_loc, "projection") <- "LL"
ports_utm <- convUL(ports_loc)

ports_vec <- ports_loc$Pcid



### Import logbook output
logbook_quant_yr <- readRDS("Data/logbook_quantile_dist_yr.rds")
logbook_quant <- readRDS("Data/logbook_quantile_dist.rds")
logbook_raw <- readRDS("Data/logbook_rawdist.rds")



logbook_quant_yr[,"port":=factor(RPCID, ports_vec)]



library(RColorBrewer)
#port_col <- brewer.pal(n=9, "PRGn")[c(1:4,6:9)]
prg.pal <- brewer.pal(n=5, "PRGn")
port_col <- rep(prg.pal[c(1:2,4:5)], each=2)
#rdgy.pal <- brewer.pal(n=5, "RdGy")
#port_col <- rep(rdgy.pal[c(1:2,4:5)], each=2)
#port_pch <- c(7, 10, 16, 17, 15, 18, 19, 5)
#port_pch <- c(7, 15, 16, 1, 6, 17, 9, 18, 10)
port_pch <- c(7, 15, 1, 16, 17, 6, 9, 18, 10)

#port_lty <- rep(c(1,2), 4)







ports_rad <- merge(ports_utm, logbook_quant, by.x="Pcid", by.y="RPCID")
setorder(ports_rad, -Lat)

# Polygons for individual port radii
circle.port <- vector("list", length(ports_vec))
for (i in 1:length(ports_vec)){
  sub <- ports_rad[Pcid==ports_vec[i]]
  circle.port[[i]] <- destPoint(cbind(sub$Lon, sub$Lat), b=1:365, d=sub$q75*1000)
}




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

knot_locs_df <- as.data.table(knot_locs)


dist_port <- knot_locs_df[,j={
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


#######################
### Figure 5
### By species
png("Figures/Fig5_port_bio.png", height=8, width=6, units="in", res=300)
par(mfrow=c(3,2), mar=c(4,4,2,2))
for(i in 1:length(dtspling.spp)){
  plot(StockBio/1000 ~ year,stock_port[spp_common==dtspling.spp[i]], col="white",
       main=dtspling.spp[i], ylab="Available Biomass (thousand mt)", las=1)
  for(j in 1:length(ports_vec)){
    points(StockBio/1000 ~ year, stock_port[port==ports_vec[j] & spp_common==dtspling.spp[i]],
           type="o", col=port_col[j], pch=port_pch[j])
  }
}

# plot(states.wcoast.utm)
# points(Y  ~ X, ports_rad, pch=port_pch, col=port_col)
# symbols(x=ports_rad$X, y=ports_rad$Y, circles=ports_rad$q75, inches=F, add=T, fg=port_col)
plot(states.wcoast)
points(Lat  ~ Lon, ports_rad, pch=port_pch, col=port_col, cex=0.8)
for(i in 1:length(ports_vec)){
  polygon(circle.port[[i]], col=NA, border=port_col[i], lwd=1)
}
legend("topright", legend=ports_vec, pch=port_pch, col=port_col, bty="n", cex=1.5)
dev.off()


### Figure for Packard Proposal; only sable and dover
### By species
spp_packard <- c("Dover sole", "sablefish")
pnames <- c("Bellingham", "Astoria", "Coos Bay", "Brookings", "Crescent City", "Eureka", "Fort Bragg", "Morro Bay")
png("Figures/doversable_port_bio.png", height=3, width=8, units="in", res=300)
par(mfrow=c(1,3), mar=c(4,4,2,2), oma=c(0,0,2,0))
for(i in 1:length(spp_packard)){
  plot(StockBio/1000 ~ year,stock_port[spp_common==spp_packard[i]], col="white",
       main=spp_packard[i], ylab="Available Biomass (thousand mt)", las=1)
  for(j in 1:length(ports_vec)){
    points(StockBio/1000 ~ year, stock_port[port==ports_vec[j] & spp_common==spp_packard[i]],
           type="o", col=port_col[j], pch=port_pch[j])
  }
}
mtext("a)", at=c(0,1), outer=T, adj=0)
mtext("b)", at=c(0.35,1), outer=T, adj=0)
mtext("c)", at=c(0.65,1), outer=T, adj=0)
plot(states.wcoast)
points(Lat  ~ Lon, ports_rad, pch=port_pch, col=port_col, cex=0.8)
for(i in 1:length(ports_vec)){
  polygon(circle.port[[i]], col=NA, border=port_col[i], lwd=1)
}
legend("topleft", inset=c(-0.3,0), legend=pnames, pch=port_pch, col=port_col, bty="n", cex=1.25, xpd=T)
dev.off()


###############################################
### Knot locs inside/outside of RCAs
### 2015-2016
rca1516 <- readOGR("GIS_Data/CommTrawlRCA_2015-16/", "RCA_CommTrawl_2015_16_poly")

rca1516_utm <- spTransform(rca1516, crs(states.wcoast.utm))






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



png("Figures/port_radii_wRCA.png", height=8, width=6, units="in", res=300)
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


#####################
### Figure 1
png("Figures/Fig1_port_radii_noRCA_lat.png", height=8, width=6, units="in", res=300)
plot(states.wcoast, col=NA)
segments(x0=inpfc_lines$lon, y0=inpfc_lines$lat,
         x1=inpfc_lines$lon_end, y1=inpfc_lines$lat_end)
plot(states.wcoast, add=T)
contour(depth, zlim=c(-500,-500), nlevels=1, add=T, drawlabels=F, col="darkgray")
#contour(depth, zlim=c(-1500,-1500), nlevels=1, add=T, drawlabels=F, col="gray")
text(x=inpfc_text$lon, y=inpfc_text$lat, labels=inpfc_text$inpfc)

#plot(rca1516_utm,  col="orange", add=T, border=NULL)
points(Lat ~ Lon, subset(knot_locs_depth[Area_km2>0]), cex=0.3, col="darkgray", pch=16) #subset to sites included in domain
points(Lat  ~ Lon, ports_rad, pch=20, col="black", cex=1)
text(x=ports_loc$print_X, y=ports_loc$print_Y, labels=ports_loc$Pcid)
for(i in 1:length(ports_vec)){
  polygon(circle.port[[i]], col=NA, border="black", lwd=2)
}
# symbols(x=ports_rad$Lon, y=ports_rad$Lat, circles=ports_rad$q75.degree, inches=F, 
#         add=T, fg=port_col, lwd=2)
axis(side=2, las=1, at=seq(34,48, by=2), line=-0.5)
mtext(side=2, "Latitude", line=3)
axis(side=1)
mtext(side=1, "Longitude", line=3)
# par(new=T)
# plot(states.wcoast, col=NA, border=NA)
# axis(side=2, at=c(32,36,40,44,48), line=-2, las=1)
#mtext(side=2, line=0, "Latitude")
#legend("topright", legend=ports_vec, pch=port_pch, col=port_col, bty="n")
dev.off()

png("Figures/inpfc_wport.png", height=5, width=4, units="in", res=300)
plot(states.wcoast, col=NA)
segments(x0=inpfc_lines$lon, y0=inpfc_lines$lat,
         x1=inpfc_lines$lon_end, y1=inpfc_lines$lat_end)
text(x=inpfc_text$lon, y=inpfc_text$lat, labels=inpfc_text$inpfc)
plot(states.wcoast, add=T)
#plot(rca1516_utm,  col="orange", add=T, border=NULL)
points(Lat  ~ Lon, ports_rad, pch=port_pch, col=port_col)
legend("topright", legend=ports_vec, pch=port_pch, col=port_col, bty="n")
dev.off()


# =====================================
# = Species Biomass within Closed Areas =
# =====================================


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


png("Figures/FracBio_closed.png", height=8, width=8, units="in", res=300)
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

### Measures of effort
dtspl.land.yr[,"mtons.per.tix":=mtons/num.tix]
dtspl.land.yr[,"mtons.per.vess":=mtons/num.vess]


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

dtspl.land.combined[,list(med.frac=median(frac.catch)), by=list(port)]

png("Figures/FigS1_DTSPL_dist_import.png", height=5, width=5, units="in", res=300)
a <- ggplot(logbook_quant_yr, aes(x=port, y=q75, fill=port))+geom_boxplot()+
  scale_fill_manual(values=port_col)+
  labs(y="75th quantile of \nDistance from Port (km)\nWeighted by Catch")+
  theme(legend.position = "none")

b <- ggplot(dtspl.land.combined, aes(x=port, y=frac.catch, fill=port))+
  geom_boxplot() + 
  #scale_y_continuous(limits = quantile(dtspl.land.yr.lim$frac.catch, c(0.1,0.9)))+
  scale_fill_manual(values=port_col)+
  labs(y="Proportion of\nTotal Catch")+
  theme(legend.position = "none")
plot_grid(b, a, nrow=2, align="v")
dev.off()



### Merge with availability to port
### Exclude 1980 because landings records start in 1981 (so first year with both is 1983)
land_avail <- merge(stock_port[year>1980], dtspl.land.yr.lim,
                    by.x=c("year", "port", "spp_common"),
                    by.y=c("LANDING_YEAR", "port", "spp"))
land_avail[,"StockBio.thous.mtons":=StockBio/1000]
#land_avail[,"land.thous.mtons":=mtons/1000]

mod_land_avail <- land_avail[,j={
  t.dt <- .SD
  mod <- lm(land.thous.mtons ~ StockBio.thous.mtons, t.dt)
  list(StockBio.est=summary(mod)$coefficients[2,1],
       StockBio.p=summary(mod)$coefficients[2,4],
       intercept.est=summary(mod)$coefficients[1,1])
}, by=list(spp_common, port)]

land_avail_wmod <- merge(land_avail, mod_land_avail, by=c("spp_common", "port"))
land_avail_wmod[,"predicted.land":=intercept.est + StockBio.est*StockBio.thous.mtons]

### Landings by ticket by species
dev.new()
par(mfrow=c(3,2), mar=c(4,4,2,2))
for(i in 1:length(dtspling.spp)){
  plot(mtons.per.tix ~ StockBio.thous.mtons, land_avail[spp_common==dtspling.spp[i]],
       main=dtspling.spp[i], col="white", 
       xlab="Available Stock Biomass (thousand mt)", ylab="Landings per ticket (mt)")
  for(j in 1:length(ports_vec)){
    sub <- land_avail[spp_common==dtspling.spp[i] & port==ports_vec[j]]
    #sub2 <- mod_land_avail[spp_common==dtspling.spp[i] & port==ports_vec[j]]
    points(mtons.per.tix ~ StockBio.thous.mtons, sub,
           col=port_col[j], pch=port_pch[j])
    #points(predicted.land ~ StockBio.thous.mtons, sub, type="l", col=port_col[j])
    #abline(a=sub2$intercept.est, b=sub2$StockBio.est, col=port_col[j])
  }
}

###############################
### Figure 6
### Landings by vessel by species
png("Figures/Fig6_landing_avail_spp_tix.png", height=8, width=6, units="in", res=300)
par(mfrow=c(3,2), mar=c(2,2,2,2), oma=c(2,2,0,0))
for(i in 1:length(dtspling.spp)){
  plot(mtons.per.tix ~ StockBio.thous.mtons, land_avail[spp_common==dtspling.spp[i]],
       main=dtspling.spp[i], col="white", 
       xlab="", ylab="", las=1)
  for(j in 1:length(ports_vec)){
    sub <- land_avail[spp_common==dtspling.spp[i] & port==ports_vec[j]]
    #sub2 <- mod_land_avail[spp_common==dtspling.spp[i] & port==ports_vec[j]]
    points(mtons.per.tix ~ StockBio.thous.mtons, sub,
           col=port_col[j], pch=port_pch[j])
    #points(predicted.land ~ StockBio.thous.mtons, sub, type="l", col=port_col[j])
    #abline(a=sub2$intercept.est, b=sub2$StockBio.est, col=port_col[j])
  }
}
mtext("Available Stock Biomass (thousand mt)", side=1, outer=T, line=0.5)
mtext("Landings per ticket (mt)", side=2, outer=T, line=0.5)

plot(states.wcoast)
points(Lat  ~ Lon, ports_rad, pch=port_pch, col=port_col, cex=0.8)
for(i in 1:length(ports_vec)){
  polygon(circle.port[[i]], col=NA, border=port_col[i], lwd=1)
}
legend("topright", legend=ports_vec, pch=port_pch, col=port_col, bty="n", cex=1.5)
dev.off()


###############################
### Landings by vessel by species
### California only
ca_ports <- c("CRS", "ERK", "BRG", "MRO")
ca_port_pch <- port_pch[port_locs_only$port %in% ca_ports]
ca_port_col <- port_col[port_locs_only$port %in% ca_ports]
png("Figures/CAonly_landing_avail_spp_tix.png", height=8, width=6, units="in", res=300)
par(mfrow=c(3,2), mar=c(2,2,2,2), oma=c(2,2,0,0))
for(i in 1:length(dtspling.spp)){
  plot(mtons.per.tix ~ StockBio.thous.mtons, land_avail[spp_common==dtspling.spp[i] & port %in% ca_ports],
       main=dtspling.spp[i], col="white", 
       xlab="", ylab="", las=1)
  for(j in 1:length(ca_ports)){
    sub <- land_avail[spp_common==dtspling.spp[i] & port==ca_ports[j]]
    #sub2 <- mod_land_avail[spp_common==dtspling.spp[i] & port==ports_vec[j]]
    points(mtons.per.tix ~ StockBio.thous.mtons, sub,
           col=ca_port_col[j], pch=ca_port_pch[j])
    #points(predicted.land ~ StockBio.thous.mtons, sub, type="l", col=port_col[j])
    #abline(a=sub2$intercept.est, b=sub2$StockBio.est, col=port_col[j])
  }
}
mtext("Available Stock Biomass (thousand mt)", side=1, outer=T, line=0.5)
mtext("Landings per ticket (mt)", side=2, outer=T, line=0.5)

plot(states.wcoast)
points(Lat  ~ Lon, ports_rad, pch=port_pch, col=port_col, cex=0.8)
for(i in 1:length(ports_vec)){
  polygon(circle.port[[i]], col=NA, border=port_col[i], lwd=1)
}
legend("topright", legend=ports_vec, pch=port_pch, col=port_col, bty="n", cex=1.5)
dev.off()


### Lingcod only, individual port graphs
par(mfrow=c(3,3))
for(j in 1:length(ports_vec)){
  sub <- land_avail[spp_common=="lingcod" & port==ports_vec[j]]
  #sub2 <- mod_land_avail[spp_common==dtspling.spp[i] & port==ports_vec[j]]
  plot(mtons.per.tix ~ StockBio.thous.mtons, sub)
  #points(predicted.land ~ StockBio.thous.mtons, sub, type="l", col=port_col[j])
  #abline(a=sub2$intercept.est, b=sub2$StockBio.est, col=port_col[j])
}
mtext("Available Stock Biomass (thousand mt)", side=1, outer=T, line=0.5)
mtext("Landings per ticket (mt)", side=2, outer=T, line=0.5)


################################
### Landings by ticket by port
png("Figures/landing_avail_port.png", height=8, width=8, units="in", res=300)
par(mfrow=c(3,3), mar=c(2,2,2,2), oma=c(2,2,4,4))
for(j in 1:length(ports_vec)){
  plot(mtons.per.tix ~ StockBio.thous.mtons, 
       land_avail[port==ports_vec[j] & spp_common !="Dover sole"], col="white",
       main=ports_vec[j], 
       xlab="", 
       ylab="")
  for(i in 2:length(dtspling.spp)){
    sub <- land_avail[spp_common==dtspling.spp[i] & port==ports_vec[j]]
    #sub2 <- mod_land_avail[spp_common==dtspling.spp[i] & port==ports_vec[j]]
    points(mtons.per.tix ~ StockBio.thous.mtons, sub,
           col=dts.color[i], pch=dts.pch[i])
    #points(predicted.land ~ StockBio.thous.mtons, sub, 
    #       type="l", col=dts.color[i], lwd=2)
  }
}

### Position of legend is relative to last plot axes
legend(-8.5,7, horiz=T, cex=1.25,
       legend=dtspling.spp[2:5], col=dts.color[2:5], pch=dts.pch[2:5], 
       bty="n", xpd=NA)

plot(mtons.per.tix ~ StockBio.thous.mtons,
     land_avail[spp_common=="Dover sole"],
     col="white", xlab="", ylab="", main="Dover sole")
for(j in 1:length(ports_vec)){
  sub2 <- land_avail[spp_common=="Dover sole" & port==ports_vec[j]]
  #sub2 <- mod_land_avail[spp_common==dtspling.spp[i] & port==ports_vec[j]]
  points(mtons.per.tix ~ StockBio.thous.mtons, sub2,
         col=port_col[j], pch=port_pch[j])
  #points(predicted.land ~ StockBio.thous.mtons, sub2, type="l", col=port_col[j])
}



# Position of legend is relative to last plot axes
legend(200,15, cex=1.25,
       legend=ports_vec, col=port_col, pch=port_pch, bty="n", xpd=NA)

mtext("Available Stock Biomass (thousand mt)", side=1,line=1,  outer=T)
mtext("Landings per ticket (mt)", side=2, line=0.5, outer=T)
dev.off()

plot(mtons.per.tix ~ StockBio.thous.mtons,
     land_avail[spp_common=="petrale sole"],
     col="white", xlab="", ylab="", main="petrale sole")
for(j in 1:length(ports_vec)){
  sub2 <- land_avail[spp_common=="petrale sole" & port==ports_vec[j]]
  #sub2 <- mod_land_avail[spp_common==dtspling.spp[i] & port==ports_vec[j]]
  points(mtons.per.tix ~ StockBio.thous.mtons, sub2,
         col=port_col[j], pch=port_pch[j])
  #points(predicted.land ~ StockBio.thous.mtons, sub2, type="l", col=port_col[j])
}

################################
### Price Per Pound

### NOAA Commercial Landings statistics by state
noaa_comm <- as.data.table(read.csv("Data/DTSPL_State_landings.csv"))


### Convert numbers with commas to numeric
noaa_comm[,"Pounds_num":= as.numeric(gsub(",", "", Pounds))]
noaa_comm[,"Dollars_num":= as.numeric(gsub(",", "", Dollars))]
noaa_comm[,"Land_kg":=Pounds_num/2.205]

### Price per pound
noaa_comm[,"Price_per_pound":= Dollars_num/Pounds_num]

### Price per kg
noaa_comm[,"Price_per_kg":= Dollars_num/Land_kg]


spp_lookup <- data.table(AFS.Name=c("LINGCOD", "SABLEFISH", "SOLE, DOVER", "SOLE, PETRALE", "THORNYHEAD, SHORTSPINE"),
                         spp_common=c("lingcod", "sablefish", "Dover sole", "petrale sole", "shortspine thornyhead"))


noaa_comm2 <- merge(noaa_comm, spp_lookup, by="AFS.Name")


png("Figures/Price_per_lb_8017.png", height=7, width=7, units="in", res=300)
par(mfrow=c(2,2), mar=c(4,4,2,2))
plot(Price_per_pound ~ Year, noaa_comm2[State=="CALIFORNIA"], col=NA, main="California",
     ylab="Price per Pound ($)", ylim=c(0,4.25))
for(i in 1:length(dtspling.spp)){
  points(Price_per_pound ~ Year, noaa_comm2[State=="CALIFORNIA" & spp_common==dtspling.spp[i]],
         pch=dts.pch[i], col=dts.color[i], type="o")
}
legend("topleft", legend=dtspling.spp, pch=dts.pch, col=dts.color, bty="n")

plot(Price_per_pound ~ Year, noaa_comm2[State=="OREGON"], col=NA, main="Oregon", 
     ylab="Price per Pound ($)", ylim=c(0,4.25))
for(i in 1:length(dtspling.spp)){
  points(Price_per_pound ~ Year, noaa_comm2[State=="OREGON" & spp_common==dtspling.spp[i]],
         pch=dts.pch[i], col=dts.color[i], type="o")
}

plot(Price_per_pound ~ Year, noaa_comm2[State=="WASHINGTON"], col=NA, main="Washington", 
     ylab="Price per Pound ($)", ylim=c(0,4.25))
for(i in 1:length(dtspling.spp)){
  points(Price_per_pound ~ Year, noaa_comm2[State=="WASHINGTON" & spp_common==dtspling.spp[i]],
         pch=dts.pch[i], col=dts.color[i], type="o")
}
dev.off()

states_noaa <- c("CALIFORNIA", "OREGON", "WASHINGTON")
states_col<-port_col[c(8,3,1)]
states_pch<- port_pch[c(8,3,1)]
png("Figures/Price_per_lb_byspp_8017.png", height=7, width=6, units="in", res=300)
par(mfrow=c(3,2), mar=c(4,4,2,2))
for(i in 1:length(dtspling.spp)){
  plot(Price_per_pound ~ Year, noaa_comm2[spp_common==dtspling.spp[i]], col=NA, main=dtspling.spp[i],
       ylab="Price per Pound ($)")
  for(j in 1:length(states_noaa)){
    points(Price_per_pound ~ Year, noaa_comm2[State==states_noaa[j] & spp_common==dtspling.spp[i]],
           col=states_col[j], pch=states_pch[j], type="o")
  }
  legend("topleft", legend=states_noaa, col=states_col, pch=states_pch, bty="n")
}
dev.off()
