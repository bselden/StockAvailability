### VAST comparisons through 2017 and through 2018
library(data.table)

### Something very weird is happening with shortspine thornyhead on StockAvailabilitytoPorts_tv.R
### Huge variation in COG that does not appear in any of the original VAST outputs

# Through 2018
new <- as.data.table(read.csv("Data/VAST_density_1April19/Dens_DF_shortspine thornyhead.csv"))

# Through 2017
old <- as.data.table(read.csv("Data/Old_VAST/VAST_density_500/Dens_DF_shortspine thornyhead.csv"))



## Original in TPW
load("Troubleshooting/Data/Thorson Pinsky Ward 2016 Density/shortspine thornyhead/Density_list.RData", verbose=T)
tpw <- as.data.table(Density_list$Density_kgperkm2)
tpw[,"knot_num":=seq(1,500)]
tpw_df <- melt(tpw, id.vars= "knot_num", variable.name = "Year", value.name = "Density_kgperkm2")
tpw_df[,"Year":=as.numeric(as.character(Year))]

knots_tpw <- as.data.table(Density_list$`KnotLoc_East-North_km2`)
knots_tpw[,"knot_num":=seq(1,500)]

tpw_df2 <- merge(dover_tpw_df, knots_tpw, by="knot_num")

# =====================================
# = Remove years without trawl survey =
# =====================================
### Also limit to beginning in 1980, first year with PACFIN landings data
tri.yrs <- seq(1980, 2004, by=3)
ann.yrs <- seq(2003,2017)

surv.yrs <- c(tri.yrs, ann.yrs)

new <- dover_new[Year %in% surv.yrs]
old <- dover_old[Year %in% surv.yrs]
tpw_df3 <- tpw_df2[Year %in% surv.yrs]

# ===========
# = COG=
# ===========

cog_new <- new[,list(cog_N=weighted.mean(N_km, w=exp(Density))), by=list(Year)]

cog_old <- old[,list(cog_N=weighted.mean(N_km, w=exp(Density))), by=list(Year)]

cog_tpw <- tpw_df3[,list(cog_N=weighted.mean(N_km, w=Density_kgperkm2)), by=list(Year)]

plot(cog_N ~ Year, cog_tpw, type="o")
points(cog_N ~ Year, cog_old,  pch=20, col="red")
