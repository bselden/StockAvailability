library(data.table)

# =============================
### Comparison of raw lat lon from ThorsonPinskyWard and FRAM
tpw <- as.data.table(read.csv("Data/ThorsonDryadData/Archive.csv"))

fram <- as.data.table(read.csv("Data/Groundfish_2019-02-06.csv"))

raw_cog_tpw <- tpw[,list(cog_Lat=weighted.mean(Latitude, w=Catch_KG)),
                   by=list(Year, Species)]

raw_cog_fram <- fram[!(is.na(latitude_dd))& !(is.na(total_catch_wt_kg)),list(cog_Lat=weighted.mean(latitude_dd, w=total_catch_wt_kg)),
                   by=list(Year=date_dim.year, common_name)]
setorder(raw_cog_fram, Year)



# =============================
### Comparison with catch data with zeros added
load("Data/catch_for_vast.RData", verbose=T)
vast.catch.dt <- as.data.table(vast.catch)

cog_vast_catch <- vast.catch.dt[,list(cog_Lat=weighted.mean(Latitude, w=Catch_KG)),
                                by=list(Year, Species)]



# =============================
### New density estimates from VAST
knot_locs <- as.data.table(read.csv("Data/VAST_knots/Knot_area_km2_sablefish.csv"))
knot_locs[,"knot_num":=seq(1:length(E_km))]

sable <- as.data.table(read.csv("Data/VAST_density/Dens_DF_sablefish.csv"))
sable[,"spp_common":="sablefish"]
sable[,"year":=Year]

dover <- as.data.table(read.csv("Data/VAST_density/Dens_DF_Dover sole.csv"))
dover[,"spp_common":="Dover sole"]
dover[,"year":=Year]

petr <- as.data.table(read.csv("Data/VAST_density/Dens_DF_petrale sole.csv"))
petr[,"spp_common":="petrale sole"]
petr[,"year":=Year]

vast.new <- rbind(sable, dover, petr)



### Some knots had zero area because outside standardized footprint, remove
length(knot_locs[Area_km2==0]$E_km) #44 knots
vast.new2 <- merge(vast.new, knot_locs[Area_km2>0], by=c("E_km", "N_km"))


### Both have negative values, could Density in VAST output be logged?? Take exp
vast.new2[,"Density_kgkm2":=exp(Density)]
### Calculate Knot Biomass as Density * Knot Area
vast.new2[,"KnotBio":=Density_kgkm2 * Area_km2]



### Get total regional biomass in each year
vast.new.regbio <- vast.new2[,list(regionalBio=sum(KnotBio)), by=list(spp_common, year)]

# =============================
# = Thorson Spatial Knots =
# =============================
spp.fold <- list.files("Data/Thorson Pinsky Ward 2016 Density")

#write.csv(data.table(spp_common=spp.fold), "thorson_spp.csv")

spp.list <- vector("list", length(spp.fold))

yrs <- seq(1977,2013)

### Save mean biomass from original data
mean.bio.mat <- matrix(NA, nrow=length(spp.fold), ncol=length(yrs))
rownames(mean.bio.mat) <- spp.fold
colnames(mean.bio.mat) <- yrs


# library(RColorBrewer)
# yrs.col <- colorRampPalette(brewer.pal(n=8, "Spectral"))(length(yrs))

for(i in 1:length(spp.fold)){
  load(paste0("Data/Thorson Pinsky Ward 2016 Density/", spp.fold[i],"/Density_list.RData"))
  
  knots <- as.data.frame(Density_list[[1]])
  knots$knot_num <- as.integer(dimnames(knots)[[1]])
  
  area_knots <- data.frame(area_knot=Density_list$Area_per_knot_km2)
  area_knots$knot_num <- as.integer(dimnames(area_knots)[[1]])
  
  knots_info <- as.data.table(merge(knots, area_knots, by=c("knot_num")))
  
  dens.mat <- Density_list$Density_kgperkm2
  ### Get mean biomass in each year
  mean.bio.mat[i,] <- colMeans(dens.mat)
  
  ### Melt density matrix
  dens.yr <- as.data.table(melt(dens.mat, varnames=c("knot_num", "year"), value.name="Density_kgkm2"))
  
  dens.yr.loc <- merge(dens.yr, knots_info, by=c("knot_num"))
  dens.yr.loc$spp_common <- spp.fold[i]
  
  ### Total bio= bio (kg per km2) * Area per knot (km2); units = kg
  ### Divide by area per knot to get density
  dens.yr.loc[,"KnotBio":=Density_kgkm2*area_knot]
  
  spp.list[[i]] <- dens.yr.loc
}


cc <- rbindlist(spp.list)
cc[,"LANDING_YEAR":=as.numeric(year)]

knot_locs_orig <- cc[,list(E_km=mean(E_km), N_km=mean(N_km)), by=list(knot_num)]

### 
cc.reg.bio <- cc[,list(regionalBio=sum(KnotBio)), by=list(spp_common, year)]



hist(log(vast.new2[spp_common=="sablefish"]$Density_kgkm2))
hist(log(cc[spp_common=="sablefish"]$Density_kgkm2))

hist(vast.new2[spp_common=="sablefish"]$Area_km2)
hist(cc[spp_common=="sablefish"]$area_knot)

# =====================================
# = Remove years without trawl survey =
# =====================================
tri.yrs <- seq(1980, 2004, by=3)
ann.yrs <- seq(2003,2017)

surv.yrs <- c(tri.yrs, ann.yrs)

cc.lim <- cc[year %in% surv.yrs]
cc.reg.bio.lim <- cc.reg.bio[year %in% surv.yrs]

vast.new.lim <- vast.new2[year %in% surv.yrs]
vast.new.regbio.lim <- vast.new.regbio[year %in% surv.yrs]

plot(regionalBio ~ year, cc.reg.bio.lim[spp_common=="sablefish"], type="o", col="black")
points(regionalBio ~ year, vast.new.regbio.lim[spp_common=="sablefish"], type="o", col="red", lty=2)

save(cc.lim, vast.new.lim, file="Output/TPWvsnewVASToutput.RData")

# =====================================
# = COG =
# =====================================
### COG in Thorson Pinsky Ward by year
cog_orig <- cc.lim[,list(cog_N=weighted.mean(N_km, KnotBio),
                         cog_E=weighted.mean(E_km, KnotBio)),
                     by=list(year, spp_common)]

cog_vast_new <- vast.new.lim[,list(cog_N=weighted.mean(N_km, KnotBio)),
                             by=list(year, spp_common)] 
  


save(cc.lim, vast.new.lim, cog_orig, cog_vast_new, file="Output/VAST_Troubleshoot.RData")




library(rgdal)
# prepare UTM coordinates matrix
proj_utm <- "+proj=utm +zone=10 +ellps=GRS80 +datum=NAD83 +units=km +no_defs";
cog_utm <- data.frame(cog_E=cog_orig$cog_E, cog_N=cog_orig$cog_N, m=seq(1:length(cog_orig$cog_N)))
utmcoor<-SpatialPointsDataFrame(coords=data.frame(cog_Lon=cog_utm$cog_E, cog_Lat=cog_utm$cog_N),
                                data=cog_utm, proj4string=CRS(proj_utm))
#utmdata$X and utmdata$Y are corresponding to UTM Easting and Northing, respectively.
#zone= UTM zone
# converting
longlatcoor<-spTransform(utmcoor,CRS("+proj=longlat"))
longlatcoor.df <- as.data.frame(longlatcoor)

cog_orig2 <- merge(cog_orig, longlatcoor.df, by=c("cog_E", "cog_N"))
setorder(cog_orig2, year)

### Recreate Figure XX in Thorson Pinsky Ward
plot(cog_N ~ year, cog_orig[spp_common=="pacific hake"], type="o", col="darkgreen",
     main="TPW Density\nPacific Hake Fig 4")

### Plot troubleshooting figures
png("Figures/COG_troubleshoot.png", height=8, width=8, units="in", res=300)
par(mfrow=c(2,2), mar=c(4,4,2,4))
plot(cog_Lat ~ Year, raw_cog_tpw[Species=="sablefish"], col="black", type="o", 
     ylim=c(41,45), xlim=c(1980,2017),
     main="Raw COG Sablefish")
points(cog_Lat ~ Year, raw_cog_fram[common_name=="sablefish"], col="blue", type="l", lty=1, lwd=2)
points(cog_Lat ~ Year, cog_vast_catch[Species=="sablefish"], col="red", type="o", lty=2)
legend("bottomleft", legend=c("TPW Raw", "FRAM Raw", "Catch for VAST"), 
       col=c("black", "blue", "red"), lty=c(1,1,2), pch=c(NA,NA,1), lwd=c(1,2,1), bty="n")

plot(cog_Lat ~ Year, raw_cog_tpw[Species=="petrale sole"], col="black", type="o", 
     ylim=c(41,45), xlim=c(1980,2017),
     main="Raw COG petrale sole")
points(cog_Lat ~ Year, raw_cog_fram[common_name=="petrale sole"], col="blue", type="l", lty=1, lwd=2)
points(cog_Lat ~ Year, cog_vast_catch[Species=="petrale sole"], col="red", type="o", lty=2)
legend("topright", legend=c("TPW Raw", "FRAM Raw", "Catch for VAST"), 
       col=c("black", "blue", "red"), lty=c(1,1,2), pch=c(NA,NA,1), lwd=c(1,2,1), bty="n")


plot(cog_N ~ year, cog_orig2[spp_common=="sablefish"], type="o", ylim=c(4400,5000),
     col="black", main="sablefish", xlim=c(1980,2017))
points(cog_N ~ year, cog_vast_new[spp_common=="sablefish"], type="o", col="red", lty=2)
legend("bottomleft", legend=c("TPW", "New VAST"), col=c("black", "red"), 
       lty=c(1,2), pch=1, bty="n")
par(new=T)
plot(cog_Lat ~ year, cog_orig2[spp_common=="sablefish"], col=NA, axes=F, xlab=NA, ylab=NA)
axis(side=4)
mtext("cog_Lat", side=4, line=2)

plot(cog_N ~ year, cog_orig[spp_common=="petrale sole"], 
     type="o", col="black", main="petrale sole", xlim=c(1980,2017), ylim=c(4400,5000), lty=1)
points(cog_N ~ year, cog_vast_new[spp_common=="petrale sole"], type="o", col="red", lty=2)
par(new=T)
plot(cog_Lat ~ year, cog_orig2[spp_common=="petrale sole"], col=NA, axes=F, xlab=NA, ylab=NA)
axis(side=4)
mtext("cog_Lat", side=4, line=2)
legend("topleft", legend=c("TPW", "New VAST"), 
       col=c("black", "red"), lty=c(1,2), pch=1, bty="n")

dev.off()


# =====================================
# = Plot relative biomass spatially
# =====================================
library(ggplot2)
library(cowplot)

compare.Density.map <- function(dat_knot, dat_cog, spp, yrs, dat_source){
  zlims <- c(min(dat_knot[spp_common==spp]$Density_kgkm2), max(dat_knot[spp_common==spp]$Density_kgkm2))
  a <- ggplot(data=dat_knot[spp_common==spp & year==yrs[1]], aes(x=E_km, y=N_km))+ labs(title=paste0(yrs[1], " ", spp))
  b <- a + geom_point(aes(color=Density_kgkm2)) +
    scale_color_distiller(palette="Spectral", limits=zlims) +
    guides(color = "none")+
    geom_hline(aes(yintercept=dat_cog[spp_common==spp & year==yrs[1]]$cog_N), lty=2)
  
  
  
  c <- ggplot(data=dat_knot[spp_common==spp & year==yrs[2]], aes(x=E_km, y=N_km)) + labs(title=paste0(yrs[2], " ", spp))
  d <- c + geom_point(aes(color=Density_kgkm2)) + 
    scale_color_distiller(palette="Spectral", limits=zlims) + 
    guides(color = "none")+ 
    geom_hline(aes(yintercept=dat_cog[spp_common==spp & year==yrs[2]]$cog_N), lty=2)
  
  
  e <- ggplot(data=dat_knot[spp_common==spp & year==yrs[3]], aes(x=E_km, y=N_km)) + labs(title=paste0(yrs[3], " ", spp))
  f <- e + geom_point(aes(color=Density_kgkm2)) + 
    scale_color_distiller(palette="Spectral", limits=zlims)+ 
    geom_hline(aes(yintercept=dat_cog[spp_common==spp & year==yrs[3]]$cog_N), lty=2)
  
  figname <- paste0("Figures/StockDistMaps/", spp, dat_source, "Density_map.png")
  fig <- ggdraw(plot_grid(b,d,f, ncol=3, rel_widths=c(0.7,0.7,1)))
  ggsave(figname, fig, width=10, height=5, units="in")
}

compare.Density.map(vast.new.lim,cog_vast_new, "sablefish", c(1980, 1992, 2013), "new_vast")
compare.Density.map(cc.lim,cog_orig, "sablefish", c(1980, 1992, 2013), "TPW")


### KnotBio
compare.KnotBio.map <- function(dat_knot, dat_cog, spp, yrs, dat_source){
  zlims <- c(min(dat_knot[spp_common==spp]$KnotBio), max(dat_knot[spp_common==spp]$KnotBio))
  a <- ggplot(data=dat_knot[spp_common==spp & year==yrs[1]], aes(x=E_km, y=N_km))+ labs(title=paste0(yrs[1], " ", spp))
  b <- a + geom_point(aes(color=KnotBio)) +
    scale_color_distiller(palette="Spectral", limits=zlims) +
    guides(color = "none")+
    geom_hline(aes(yintercept=dat_cog[spp_common==spp & year==yrs[1]]$cog_N), lty=2)
  
  
  
  c <- ggplot(data=dat_knot[spp_common==spp & year==yrs[2]], aes(x=E_km, y=N_km)) + labs(title=paste0(yrs[2], " ", spp))
  d <- c + geom_point(aes(color=KnotBio)) + 
    scale_color_distiller(palette="Spectral", limits=zlims) + 
    guides(color = "none")+ 
    geom_hline(aes(yintercept=dat_cog[spp_common==spp & year==yrs[2]]$cog_N), lty=2)
  
  
  e <- ggplot(data=dat_knot[spp_common==spp & year==yrs[3]], aes(x=E_km, y=N_km)) + labs(title=paste0(yrs[3], " ", spp))
  f <- e + geom_point(aes(color=KnotBio)) + 
    scale_color_distiller(palette="Spectral", limits=zlims)+ 
    geom_hline(aes(yintercept=dat_cog[spp_common==spp & year==yrs[3]]$cog_N), lty=2)
  
  figname <- paste0("Figures/StockDistMaps/", spp, dat_source, "KnotBio_map.png")
  fig <- ggdraw(plot_grid(b,d,f, ncol=3, rel_widths=c(0.7,0.7,1)))
  ggsave(figname, fig, width=10, height=5, units="in")
}

compare.KnotBio.map(vast.new.lim,cog_vast_new, "sablefish", c(1980, 1992, 2013), "new_vast")
compare.KnotBio.map(cc.lim,cog_orig, "sablefish", c(1980, 1992, 2013), "TPW")