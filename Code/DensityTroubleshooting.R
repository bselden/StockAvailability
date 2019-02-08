library(data.table)

### New estimates from VAST
knot_locs <- as.data.table(read.csv("Data/Knot_area_km2_shortspine thornyhead.csv"))
knot_locs[,"knot_num":=seq(1:length(E_km))]

sable <- as.data.table(read.csv("Data/VAST_output/Dens_DF_sablefish.csv"))
sable[,"spp_common":="sablefish"]
sable[,"year":=Year]

petr <- as.data.table(read.csv("Data/VAST_output/Dens_DF_petrale sole.csv"))
petr[,"spp_common":="petrale sole"]
petr[,"year":=Year]

vast.new <- rbind(sable, petr)

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
  dens.yr <- as.data.table(melt(dens.mat, varnames=c("knot_num", "year"), value.name="Bio_kgkm2"))
  
  dens.yr.loc <- merge(dens.yr, knots_info, by=c("knot_num"))
  dens.yr.loc$spp_common <- spp.fold[i]
  
  ### Total bio= bio (kg per km2) * Area per knot (km2); units = kg
  dens.yr.loc[,"TotalBio":=area_knot * Bio_kgkm2]
  
  spp.list[[i]] <- dens.yr.loc
}


cc <- rbindlist(spp.list)
cc[,"LANDING_YEAR":=as.numeric(year)]

knot_locs_orig <- cc[,list(E_km=mean(E_km), N_km=mean(N_km)), by=list(knot_num)]

# =====================================
# = Remove years without trawl survey =
# =====================================
tri.yrs <- seq(1980, 2004, by=3)
ann.yrs <- seq(2003,2013)

surv.yrs <- c(tri.yrs, ann.yrs)

cc.lim <- cc[year %in% surv.yrs]

vast.new.lim <- vast.new[year %in% surv.yrs]



# =====================================
# = COG =
# =====================================
### COG in Thorson Pinsky Ward by year
cog_orig <- cc.lim[,list(cog_N=weighted.mean(N_km, TotalBio)),
                     by=list(year, spp_common)]

cog_vast_new <- vast.new.lim[,list(cog_N=weighted.mean(N_km, Density)),
                             by=list(year, spp_common)] 
  


save(cc.lim, vast.new.lim, cog_orig, cog_vast_new, file="Output/VAST_Troubleshoot.RData")

  
### Recreate Figure XX in Thorson Pinsky Ward
png("Figures/Hake_Comparison.png", height=5, width=4, res=300, unit="in")
plot(cog_N ~ year, cog_orig[spp_common=="pacific hake"], type="o", main="Pacific Hake")
dev.off()

png("Figures/VAST_troubleshoot.png", height=5, width=8, res=300, unit="in")
par(mfrow=c(1,2))
plot(cog_N ~ year, cog_orig[spp_common=="sablefish"], type="o", ylim=c(4500,4900),
     col="blue", main="ThorsonPinskyWard")
points(cog_N ~ year, cog_orig[spp_common=="petrale sole"], type="o", col="red")
legend("bottomleft", legend=c("sablefish", "petrale"), col=c("blue", "red"), lty=1, bty="n")

plot(cog_N ~ year, cog_vast_new[spp_common=="sablefish"], 
     type="o", col="blue", main="New VAST output", ylim=c(4500,4900))
points(cog_N ~ year, cog_vast_new[spp_common=="petrale sole"], type="o", col="red")
dev.off()