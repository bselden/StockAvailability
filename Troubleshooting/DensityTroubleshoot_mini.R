library(data.table)
### Load minimally processed data from VAST output
### vast.new.lim New VAST output: read in Density_DF.csv, merge with knot area, calculate KnotBio, rbind into one file for all species, limit to survey years
### some knots had an area=0, so were removed in vast.new.lim
### cc.lim Thorson Pinsky Ward VAST output, read in Density_list, melt density matrix in $Density_kgperkm2 (actually KnotBio?), label with knots and years, rbind into one file for all species, limit to survey years
load("Troubleshooting/Output/TPWvsnewVASToutput.RData", verbose=T)
# =====================================
# = COG =
# =====================================
### COG in Thorson Pinsky Ward by year
### TotalBio is what was listed as $Density_kgperkm2 in file originally sent by Jim but appears that it was Density*Area=TotalBio
cog_orig <- cc.lim[,list(cog_N=weighted.mean(N_km, KnotBio),
                         cog_E=weighted.mean(E_km, KnotBio)),
                   by=list(year, spp_common)]

### COG from new VAST output
### KnotBio=Density*KnotArea
cog_vast_new <- vast.new.lim[,list(cog_N=weighted.mean(N_km, KnotBio)),
                             by=list(year, spp_common)] 


### Compare COG in Northings for ThorsonPinskyWard and new VAST output
png("Figures/COG_TPW_vsnewVAST.png", height=5, width=5, units="in", res=300)
plot(cog_N ~ year, cog_orig[spp_common=="dover sole"], type="o", ylim=c(4500,5000),
     col="black", main="sablefish")
points(cog_N ~ year, cog_vast_new[spp_common=="Dover sole"], type="o", col="red", lty=2)
legend("bottomleft", legend=c("TPW", "New VAST"), col=c("black", "red"), 
       lty=c(1,2), pch=1, bty="n")
dev.off()