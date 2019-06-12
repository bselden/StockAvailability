library(data.table)
library(sp)
library(rgdal)
library(maptools)

### MODIFIED BY JS 101618 TO:
# - USE 1980 AS FIRST YEAR
# - INCLUDE DOTTED LINE DENTOING CHANGE FROM TRIENNIAL TO ANNUAL TRAWL SURVEY



# =============================
# = Thorson Spatial Knots =
# =============================
### NOTE: needed to update to work with new VAST output from Nick Tolimieri 
### because Jim's original output had Density_kgperkm2
### and default output from new runs of VAST is log(Density)

spp.fold <- list.files("Data/VAST_output")

spp.list <- vector("list", length(spp.fold))
spp.names <- gsub(".csv", "", gsub("Dens_DF_", "", spp.fold))


yrs <- seq(1977,2017)

knot_locs <- as.data.table(read.csv("Data/Knot_area_km2_shortspine thornyhead.csv"))
knot_locs[,"knot_num":=seq(1:length(E_km))]

for(i in 1:length(spp.fold)){
  vast <- read.csv(paste0("Data/VAST_output/", spp.fold[i]))
  
  ### 
  dens.yr <- as.data.table(vast)
  
  dens.yr.loc <- merge(dens.yr, knot_locs, by=c("E_km", "N_km"))
  dens.yr.loc$spp_common <- spp.names[i]
  
  ### Total bio= bio (kg per km2) * Area per knot (km2); units = kg
  dens.yr.loc[,"Density_kgperkm2":=exp(Density)]
  dens.yr.loc[,"TotalBio":=Area_km2 * Density_kgperkm2]
  
  spp.list[[i]] <- dens.yr.loc
}


cc <- rbindlist(spp.list)
cc[,"year":=Year]
cc[,"LANDING_YEAR":=as.numeric(year)]

#knot_locs <- cc[,list(E_km=mean(E_km), N_km=mean(N_km)), by=list(knot_num)]

# =====================================
# = Remove years without trawl survey =
# =====================================
tri.yrs <- seq(1980, 2004, by=3)
ann.yrs <- seq(2003,2017)

surv.yrs <- c(tri.yrs, ann.yrs)

cc.lim <- cc[year %in% surv.yrs]

# =====================================
# = Distribution of Biomass (kg) across Space =
# =====================================
region.bio <- cc.lim[,list(regionBio=sum(TotalBio)), by=list(spp_common, year)]

cc.lim2 <- merge(cc.lim, region.bio, by=c("spp_common", "year"))

cc.lim2[,"relBio":=TotalBio/regionBio]

# =====================================
# = Couple with spawning stock biomass from assessment =
# =====================================
### Units= metric tons of spawning biomass
dover <- as.data.table(read.csv("Data/AssessmentSPB/Dover_2015_SPB.csv"))
dover[,"spp_common":="dover sole"]



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


#### Biomass over time
dtspling.spp <- unique(dtspling$spp_common)
library(RColorBrewer)
dts.color <- brewer.pal(n=9, "Spectral")
dts.color <- dts.color[c(1:3, 7,9)]
#dts.pch <- c(0,15,1,16,2,17)
dts.pch <- c(19,3,15,7,17) #to match ggplot in fig 4






# =====================================
# = Merge with distribution =
# =====================================
dtspling.dist <- merge(dtspling, cc.lim2, by=c("year", "spp_common"))

### Calculate stock biomass in each knot by multiplying relative biomass across space by total SSB from assessment
dtspling.dist[,"StockBio":=relBio*Value]


# =====================================
# = COG and Stock Assessment Bio =
# =====================================
### COG by year
cog <- dtspling.dist[,list(cog_N=weighted.mean(N_km, StockBio), 
                           log_assessBio=log(mean(Value)),
                           SSB=mean(Value)),
                     by=list(year, spp_common)]
cog[,"SSB.thous":=SSB/1000]

### Annual mean COG
overall_cog <- dtspling.dist[,list(cog_N_overall=weighted.mean(N_km, StockBio)), by=list(spp_common)]
cog2 <- merge(cog, overall_cog, by=c("spp_common"))

### Anomaly in COG
cog2[,"cog_anom":=cog_N - cog_N_overall]

### Standard deviation of COG anomaly by species
cog_sd <- cog2[,list(sd_cog=sd(cog_N),
                     mad_cog=sum(abs(cog_N-cog_N_overall))/.N,
                     max_cog_anom=max(abs(cog_anom))), by=list(spp_common)]


# ================
# = PacFIN Ports =
# ================
ports <- as.data.table(read.csv("Data/top.trawl.port.loc.csv"))
setorder(ports, Lat)

### Get rid of Puget Sound ports
ports_lim <- ports[!(PACFIN_PORT_CODE %in% c("BLL", "BLN", "ANA")) ]

### Stagger ports for plotting names
ports_lim[,"stagger":=rep(c(0,1), 10)]

# ===================
# = State Shapefile =
# ===================
states <- readOGR("GIS_Data/cb_2016_us_state_500k", "cb_2016_us_state_500k")
states.data <- states@data

proj.states <- proj4string(states)

states.wcoast <- states[states$NAME %in% c("Washington", "California", "Oregon"),]

states.wcoast.utm <- spTransform(states.wcoast, "+proj=utm +zone=10 +ellps=GRS80 +datum=NAD83 +units=km +no_defs")



# ==================================
# = Distance between Port and Knot =
# ==================================

port_locs <- ports_lim[,list(port=PACFIN_PORT_CODE, easting=X, northing=Y)]

port_name <- ports_lim$PACFIN_PORT_CODE
port_easting <-ports_lim$X
port_northing <- ports_lim$Y

dist_port <- knot_locs[,j={
  t.dt <- .SD
  
  temp <- copy(port_locs)
  
  temp[,"dist.E":=t.dt$E_km - easting] # knot easting - port easting  
  temp[,"dist.N":= t.dt$N_km - northing ] #knot northing - port northing  
  list(port=temp$port, port.N=round(temp$northing), dist.E=temp$dist.E, dist.N=temp$dist.N)
  
}, by=list(knot_num)]

dist_port[,"total_dist":=sqrt((dist.E)^2 + (dist.N)^2)]

#### Merge with biomass data
cc_spp_dist <- merge(dtspling.dist, dist_port, by="knot_num", allow.cartesian=T)

# ==================================
# = Biomass Weighted by Inverse Distance =
# ==================================

cc_spp_dist[,"inv.dist":= 1/total_dist ]

cc_spp_bio_port <- cc_spp_dist[,list(wtd.bio =weighted.mean(StockBio, w=inv.dist),
                                     port.N=round(mean(port.N))), 
                               by=list(spp_common, port, LANDING_YEAR, year)]
cc_spp_bio_port[,"thous.wtd.bio":=wtd.bio/1000]
cc_spp_bio_port[,"log.bio":=log(wtd.bio)]
cc_spp_bio_port[,"avg.log.bio":=mean(log.bio), by=list(spp_common, port)]
cc_spp_bio_port[,"bio.anom":=log.bio - avg.log.bio]


# ==================================
# = Distance weighted by biomass =
# ==================================
cc_spp_dist_port <- cc_spp_dist[,list(wtd.distN =weighted.mean(dist.N, w=StockBio),
                                      wtd.totaldist=weighted.mean(total_dist, w=StockBio),
                                      wtd.invdist =weighted.mean(inv.dist, w=StockBio),
                                     port.N=round(mean(port.N))), 
                               by=list(spp_common, port, LANDING_YEAR, year)]





# ==================================
# = Availability vs revenue=
# ==================================
## Restrict to years with >=3 vessels
ann.dtspl.avail <- merge(cc_spp_bio_port, ann.dtspl2[num.vess>=3], 
                         by.x=c("year", "port", "spp_common"),
                         by.y=c("LANDING_YEAR", "PACFIN_PORT_CODE", "spp_common"), all.x=T)

ann.dtspl.avail[,"thous.mtons":=mtons/1000]

ann.dtspl.avail[,"dollar.NA":=ifelse(is.na(dollars), 0.1, dollars)]
ann.dtspl.avail[,"mtons.NA":=ifelse(is.na(mtons), 0.1, mtons)] #this had been dollars in the last argument=mistake corrected May 10, 2018
ann.dtspl.avail[,"percent.rev.NA":=ifelse(is.na(dollars), 0, frac.dollars)]
ann.dtspl.avail[,"percent.mtons.NA":=ifelse(is.na(mtons), 0, frac.mtons)]
ann.dtspl.avail[,"log.rev":=log(dollar.NA)]
ann.dtspl.avail[,"log.mtons":=log(mtons.NA)]

saveRDS(ann.dtspl.avail, "Output/ann.dtspl.avail.rds")

ann.dtspl.perc.all <- ann.dtspl.avail[,list(total.perc.rev=sum(frac.dollars, na.rm=T)), by=list(port, year)]

port.select <- c("MRO", "BRG",  "COS", "AST")

sable.ann <-ann.dtspl.avail[port %in% port.select & spp_common=="sablefish"]
ling.ann <-ann.dtspl.avail[port %in% port.select & spp_common=="lingcod"]
pet.ann <- ann.dtspl.avail[port %in% port.select & spp_common=="petrale sole"]
dover.ann <- ann.dtspl.avail[port %in% port.select & spp_common=="dover sole"]

mro.ann <- ann.dtspl.avail[port=="MRO"]
brg.ann <- ann.dtspl.avail[port=="BRG"]
erk.ann <- ann.dtspl.avail[port=="ERK"]
crs.ann <- ann.dtspl.avail[port=="CRS"]
cos.ann <- ann.dtspl.avail[port=="COS"]
new.ann <- ann.dtspl.avail[port=="NEW"]
ast.ann <- ann.dtspl.avail[port=="AST"]




port3 <- c("MRO", "BRG", "COS", "AST") #ordered by lat
port.col <- c( "gray", "darkorchid4", "black", "#008081")
port.pch <- c(19,1,3, 7)



ann.dtspl.avail.portlim <- ann.dtspl.avail[port %in% port3]
ann.dtspl.avail.portlim[,"port":=factor(port)]


# ==================================
# = Figures =
# ==================================
### Log and exp axes
# land.log.brks <- seq(-1.25,10, by=1.25)
# land.exp.brks <- signif(exp(land.log.brks), digits=2)
# 
# bio.log.brks <- seq(0,8, by=0.5)
# bio.exp.brks <- round(exp(bio.log.brks))
# 
# ssb.log.brks <- seq(7,13, by=1)
# ssb.exp.brks <- exp(ssb.log.brks)
# ssb.exp.brks.thous <- round(ssb.exp.brks/1000)


exp.list <- seq(0,4, by=1)
land.exp.brks <- 10^(exp.list)
land.log.brks <- log(land.exp.brks)


bio.exp.brks <- c(1,5,10,25,50,100, 250,500,1000,2500, 5000,10000)
bio.log.brks <- log(bio.exp.brks)

ssb.exp.brks <- c(2500, 5000,10000, 20000, 50000, 100000, 200000, 500000)
ssb.log.brks <- log(ssb.exp.brks)
ssb.exp.brks.thous <- round(ssb.exp.brks/1000)

# =====================================
# = Plot relative biomass spatially
# =====================================
library(ggplot2)


compare.relBio.map <- function(dat_knot, dat_cog, spp, yrs){
  zlims <- c(min(dat_knot[spp_common==spp]$relBio), max(dat_knot[spp_common==spp]$relBio))
  a <- ggplot(data=dat_knot[spp_common==spp & year==yrs[1]], aes(x=E_km, y=N_km))+ labs(title=paste0(yrs[1], " ", spp))
  b <- a + geom_point(aes(color=relBio, size=0.5)) +
    scale_color_distiller(palette="Spectral", limits=zlims) +
    guides(color = "none")+
    geom_hline(aes(yintercept=dat_cog[spp_common==spp & year==yrs[1]]$cog_N), lty=2)
  

  
  c <- ggplot(data=dat_knot[spp_common==spp & year==yrs[2]], aes(x=E_km, y=N_km)) + labs(title=paste0(yrs[2], " ", spp))
  d <- c + geom_point(aes(color=relBio, size=0.5)) + 
    scale_color_distiller(palette="Spectral", limits=zlims) + 
    guides(color = "none")+ 
    geom_hline(aes(yintercept=dat_cog[spp_common==spp & year==yrs[2]]$cog_N), lty=2)
  
  
  e <- ggplot(data=dat_knot[spp_common==spp & year==yrs[3]], aes(x=E_km, y=N_km)) + labs(title=paste0(yrs[3], " ", spp))
  f <- e + geom_point(aes(color=relBio, size=0.5)) + 
    scale_color_distiller(palette="Spectral", limits=zlims)+ 
    geom_hline(aes(yintercept=dat_cog[spp_common==spp & year==yrs[3]]$cog_N), lty=2)
  
  library(cowplot)
  figname <- paste0("Figures/StockDistMaps/", spp, "RelBio_map.png")
  fig <- ggdraw(plot_grid(b,d,f, ncol=3, rel_widths=c(0.7,0.7,1)))
  ggsave(figname, fig, width=10, height=5, units="in")
}

compare.relBio.map(dtspling.dist,cog, "sablefish", c(1980, 1992, 2013))

# =====================================
# = Plot stock biomass spatially
# =====================================
library(ggplot2)

############### FIGURE 4 ####################

compare.stockBio.map <- function(dat_knot, dat_cog, spp, yrs){
  zlims <- c(min(dat_knot[spp_common==spp]$StockBio), max(dat_knot[spp_common==spp]$StockBio))
  # a <- ggplot(data=dat_knot[spp_common==spp & year==yrs[1]], aes(x=E_km, y=N_km))+ labs(title=paste0(yrs[1], " ", spp))
  # b <- a + geom_point(aes(color=StockBio)) + 
  #   scale_color_distiller(palette="Spectral", limits=zlims) + 
  #   guides(color = "none")+ 
  #   geom_hline(aes(yintercept=dat_cog[spp_common==spp & year==yrs[1]]$cog_N), lty=2)
  
  a <- ggplot()+ labs(title=paste0(yrs[1], " ", spp))
  b <- a + geom_point(data=dat_knot[spp_common==spp & year==yrs[1]], aes(x=E_km, y=N_km, color=StockBio)) + 
    scale_color_distiller(palette="Spectral", limits=zlims) + 
    guides(color = "none")+ 
    geom_hline(aes(yintercept=dat_cog[spp_common==spp & year==yrs[1]]$cog_N), lty=2)
  b2 <- b + geom_point(data=port_locs[port %in% port3], aes(x=easting, y=northing))+
    geom_text(data=port_locs[port %in% port3], aes(x=easting, y=northing, label=port),hjust=-0.15, vjust=0)
  
  c <- ggplot()+ labs(title=paste0(yrs[2], " ", spp))
  d <- c + geom_point(data=dat_knot[spp_common==spp & year==yrs[2]], aes(x=E_km, y=N_km, color=StockBio)) + 
    scale_color_distiller(palette="Spectral", limits=zlims) + 
    guides(color = "none")+ 
    geom_hline(aes(yintercept=dat_cog[spp_common==spp & year==yrs[2]]$cog_N), lty=2)
  d2 <- d + geom_point(data=port_locs[port %in% port3], aes(x=easting, y=northing))+
    geom_text(data=port_locs[port %in% port3], aes(x=easting, y=northing, label=port),hjust=-0.15, vjust=0)
  
  
  e <- ggplot()+ labs(title=paste0(yrs[3], " ", spp))
  f <- e + geom_point(data=dat_knot[spp_common==spp & year==yrs[3]], aes(x=E_km, y=N_km, color=StockBio)) + 
    scale_color_distiller(palette="Spectral", limits=zlims) + 
    geom_hline(aes(yintercept=dat_cog[spp_common==spp & year==yrs[3]]$cog_N), lty=2)
  f2 <- f + geom_point(data=port_locs[port %in% port3], aes(x=easting, y=northing))+
    geom_text(data=port_locs[port %in% port3], aes(x=easting, y=northing, label=port),hjust=-0.15, vjust=0)
  
  
  library(cowplot)
  figname <- paste0("Figures/StockDistMaps/", spp, "StockBio_map.png")
  fig <- ggdraw(plot_grid(b2,d2,f2, ncol=3, rel_widths=c(0.7,0.7,1)))
  ggsave(figname, fig, width=10, height=5, units="in")
}

compare.stockBio.map(dtspling.dist,cog, "sablefish", c(1980, 1992, 2013))
compare.stockBio.map(dtspling.dist,cog, "dover sole", c(1986, 1998, 2010))
compare.stockBio.map(dtspling.dist,cog, "lingcod", c(1980, 1995, 2010))



# =====================================
# = Plot Stock Biomass =
# =====================================
# ### Sablefish only
# png("Figures/RegionBioCOG_SableOnly.png", height=5, width=8, units="in", res=300)
# par(mfrow=c(1,2))
# plot(log_assessBio ~ year,cog2, col="white", ylab="Assessed Spawning Biomass (log(mt))")
# points(log_assessBio  ~ year, cog2[spp_common=="sablefish"], col=dts.color[2], pch=dts.pch[2], type="o")
# plot(cog_anom ~ year,cog2, col="white", ylab="Center of Gravity (km; Anomaly)")
# points(cog_anom~year, cog2[spp_common=="sablefish"],
#        col=dts.color[2], pch=dts.pch[2], type="o")
# dev.off()
# 

########################## FIGURE 2 ##########################



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

# png("Figures/RegionBioCOG_DTSPling_anom.png", height=5, width=8, units="in", res=300)
# par(mfrow=c(1,2))
# plot(log_assessBio ~ year,cog2, col="white", ylab="Assessed Spawning Biomass (log(mt))")
# for(i in 1:length(dtspling.spp)){
#   sub <- cog2[spp_common==dtspling.spp[i]]
#   points(log_assessBio  ~ year, sub, col=dts.color[i], pch=dts.pch[i], type="o")
# }
# 
# legend(1980, 14.5, ncol=3, legend=dtspling.spp, pch=dts.pch, col=dts.color, xpd=NA, bty="n")
# 
# plot(cog_anom ~ year,cog2, col="white", ylab="Center of Gravity (km; Anomaly)")
# #out <- matrix(nrow=length(unique(cog2$year)), ncol=length(dtspling.spp))
# for(i in 1:length(dtspling.spp)){
#   sub <- cog2[spp_common==dtspling.spp[i]]
#   points(cog_anom ~ year, sub, col=dts.color[i], pch=dts.pch[i], type="o")
#   # temp <- smooth.spline(x=sub$year, y=sub$cog_anom)
#   # out[,i] <- temp$y
#   # points(unique(cog2$year), out[,i], type="l", col=dts.color[i])
# }
# dev.off()

# =====================================
# = Plot Port Location =
# =====================================
png("Figures/DistanceTraveled by Port.png", height=7, width=8, units="in", res=300)
### Following Lecture 4 by Branch in Beautiful Graphics Course
par(mfrow=c(1,1), oma=c(0,0,0,0), mar=c(1,1,1,1))
par(mar=c(0,0,0,0), fig=c(0,0.4,0.4,1))
plot(states.wcoast.utm, xlim=c(200,600), ylim=c(3800,5200))
points(Y ~ X, ports_lim[PACFIN_PORT_CODE %in% ports.dtspl$PACFIN_PORT_CODE], col="black", pch=20)
text(x=ports_lim[stagger==0 & PACFIN_PORT_CODE %in% ports.dtspl$PACFIN_PORT_CODE]$X - 75, 
     y=ports_lim[stagger==0& PACFIN_PORT_CODE %in% ports.dtspl$PACFIN_PORT_CODE]$Y-20, 
     labels=ports_lim[stagger==0 &PACFIN_PORT_CODE %in% ports.dtspl$PACFIN_PORT_CODE]$PACFIN_PORT_CODE, cex=0.8)
text(x=ports_lim[stagger==1& PACFIN_PORT_CODE %in% ports.dtspl$PACFIN_PORT_CODE]$X + 75, 
     y=ports_lim[stagger==1&PACFIN_PORT_CODE %in% ports.dtspl$PACFIN_PORT_CODE]$Y+20, 
     labels=ports_lim[stagger==1&PACFIN_PORT_CODE %in% ports.dtspl$PACFIN_PORT_CODE]$PACFIN_PORT_CODE, cex=0.8)
#text(x=250, y=ports_lim[PACFIN_PORT_CODE=="AST"]$Y, labels=paste0("Northing=",round(ports_lim[PACFIN_PORT_CODE=="AST"]$Y)))
#text(x=250, y=ports_lim[PACFIN_PORT_CODE=="AVL"]$Y, labels=paste0("Northing=", round(ports_lim[PACFIN_PORT_CODE=="AVL"]$Y)))
text(x=0,y=5200, labels="(A)")
text(x=0,y=3800, labels="(B)")
#mtext( text="(a)", side=3, adj=0)


par(new=T)
par(fig=c(0.05, 0.55, 0, 0.4), mar=c(4,4,1,1))
barplot(cog_sd$sd_cog, names.arg=rev(c("shortspine\nthornyhead","sablefish", "petrale sole", "lingcod", "dover sole")), horiz=T, 
         las=1, xlab="COG standard deviation\n(km)", xlim=c(0,150), axes=F)
axis(side=1, at=seq(0,150, by=25))
#mtext( text="(b)", side=3, adj=-0.5)

#boxplot(cog_anom ~ spp_common, cog2, horizontal=T, las=1)

# boxplot(percent.mtons ~ Y_port, 
#         decade.ports.dtspl, horizontal=T,
#         xlab="Percent DTSPL of\nNon-whiting Trawl Landings", ylab="", las=1)

par(new=T)
par(fig=c(0.48,1,0,1), mar=c(4,4,1,1))
boxplot(wtd.dist ~ PORT, 
        dist.wtd.port, horizontal=T, outline=F,
        xlab="Distance between port and harvest\nweighted by catch (km)", ylab="", las=1, ylim=c(0,150), xaxt="n")
axis(side=1, at=seq(0,150, by=25))


mtext( text="(C)", side=3, adj=-0.25)


dev.off()


# ==================================
# = Plot Availability vs revenue=
# ==================================

# png("Figures/biovsrev_spp.png", height=8, width=6, units="in", res=300)
# par(mfrow=c(3,2), mar=c(4,4,2,2))
# ann.dtspl.avail.portlim[,j={
#   t.dt <- .SD
#   plot(log.bio ~log(dollar.NA), t.dt, col="white", main=spp_common)
#   for(i in 1:length(ports.dtspl.name)){
#     sub <- t.dt[port==ports.dtspl.name[i]]
#     points(log.bio ~ log(dollar.NA), sub, col=port.col.all[i], pch=port.pch.all[i])
#   }
#   
# } , by=list(spp_common)]
# dev.off()
# 
png("Figures/biovsland_spp_allgear.png", height=8, width=6, units="in", res=300)
par(mfrow=c(3,2), mar=c(4,4,2,2))
ann.dtspl.avail.portlim[port %in% c("MRO", "BRG", "COS", "AST"),j={
  t.dt <- .SD
  plot(log.mtons ~ log.bio, t.dt[log.mtons>0], col="white", main=spp_common,
       ylab="Landings (log(mt))", xlab="Stock Availability (log(mt))")
  legend("bottomright", port.select, col=port.col, pch=port.pch, bty="n")
  for(i in 1:length(port.select)){
    sub <- t.dt[port==port.select[i]]
    mod <- lm(log.mtons ~log.bio, sub[log.mtons>0])
    pred.mod <- predict(mod, sub[log.mtons>0])
    points(log.mtons ~log.bio, sub[log.mtons>0], col=port.col[i], pch=port.pch[i])
    points(sub[log.mtons>0]$log.bio, pred.mod, type="l", col=port.col[i], lwd=2)
  }
  
} , by=list(spp_common)]
dev.off()

png("Figures/biovsland_spp_allgear_noMRO.png", height=8, width=6, units="in", res=300)
par(mfrow=c(3,2), mar=c(4,4,2,2))
ann.dtspl.avail.portlim[port %in% c("BRG", "COS", "AST"),j={
  t.dt <- .SD
  plot(log.mtons ~ log.bio, t.dt[log.mtons>0], col="white", main=spp_common,
       ylab="Landings (log(mt))", xlab="Stock Availability (log(mt))")
  legend("bottomright", rev(port.select[2:4]), col=rev(port.col[2:4]), pch=rev(port.pch[2:4]), bty="n")
  for(i in 2:length(port.select)){
    sub <- t.dt[port==port.select[i]]
    mod <- lm(log.mtons ~log.bio, sub[log.mtons>0])
    pred.mod <- predict(mod, sub[log.mtons>0])
    points(log.mtons ~log.bio, sub[log.mtons>0], col=port.col[i], pch=port.pch[i])
    points(sub[log.mtons>0]$log.bio, pred.mod, type="l", col=port.col[i], lwd=2)
  }
  
} , by=list(spp_common)]
dev.off()

png("Figures/biovsland_spp_allgear_MROonly.png", height=8, width=6, units="in", res=300)
par(mfrow=c(3,2), mar=c(4,4,2,2))
ann.dtspl.avail.portlim[port %in% c("MRO"),j={
  t.dt <- .SD
  plot(log.mtons ~ log.bio, t.dt[log.mtons>0], col="white", main=spp_common,
       ylab="Landings (log(mt))", xlab="Stock Availability (log(mt))")
  legend("bottomright", port.select, col=port.col, pch=port.pch, bty="n")
  for(i in 1:1){
    sub <- t.dt[port==port.select[i]]
    mod <- lm(log.mtons ~log.bio, sub[log.mtons>0])
    pred.mod <- predict(mod, sub[log.mtons>0])
    points(log.mtons ~log.bio, sub[log.mtons>0], col=port.col[i], pch=port.pch[i])
    points(sub[log.mtons>0]$log.bio, pred.mod, type="l", col=port.col[i], lwd=2)
  }
  
} , by=list(spp_common)]
dev.off()






### Plot revenue vs availability
png("Figures/biovsrev_spp_allgear.png", height=8, width=6, units="in", res=300)
par(mfrow=c(3,2), mar=c(4,4,2,2))
ann.dtspl.avail.portlim[port %in% c("MRO", "BRG", "COS", "AST"),j={
  t.dt <- .SD
  plot(log.rev ~ log.bio, t.dt[log.mtons>0], col="white", main=spp_common,
       ylab="Revenue (log($))", xlab="Stock Availability (log(mt))")
  legend("bottomright", port.select, col=port.col, pch=port.pch, bty="n")
  for(i in 1:length(port.select)){
    sub <- t.dt[port==port.select[i]]
    mod <- lm(log.rev ~log.bio, sub[log.mtons>0])
    pred.mod <- predict(mod, sub[log.mtons>0])
    points(log.rev ~log.bio, sub[log.mtons>0], col=port.col[i], pch=port.pch[i])
    points(sub[log.mtons>0]$log.bio, pred.mod, type="l", col=port.col[i], lwd=2)
  }
  
} , by=list(spp_common)]
dev.off()

# png("Figures/biovsrev_focal_port.png", height=8, width=6, units="in", res=300)
# par(mfrow=c(2,2), mar=c(4,4,2,2))
# ann.dtspl.avail.portlim[port %in% c("MRO", "BRG", "COS", "AST"),j={
#   t.dt <- .SD
#   plot(log.bio ~log(dollar.NA), t.dt, col="white", main=port)
#   for(i in 1:length(dtspling.spp)){
#     sub <- t.dt[spp_common==dtspling.spp[i]]
#     points(log.bio ~ log(dollar.NA), sub, col=dts.color[i], pch=dts.pch[i])
#   }
#   
# } , by=list(port)]
# #plot(x=c(0,1), y=c(0,1), col="white", axes=F, xlab="", ylab="")
# #legend("topleft", legend=dtspling.spp, lty=1, pch=dts.pch, col=dts.color, xpd=T, lwd=1.5, bty="n")
# 
# dev.off()

# ==================================
# = ggplot connected points time series =
# ==================================

library(ggplot2)
library(ggrepel)
library(cowplot)
library(gridExtra)




### Get rid of shortspine for MRO because overlapping with sablefish
mro1 <- ggplot(mro.ann[!(is.na(mtons))& spp_common != "shortspine thornyhead"], 
             aes(y=log.mtons, x=log.bio, shape=spp_common)) +
  geom_point(size=2)+ggtitle("MRO")+ylab("Landings (mt)") + xlab("Stock Availability (mt)")+ 
  geom_path(aes(colour=year)) + scale_x_continuous(breaks=bio.log.brks, labels=bio.exp.brks)+scale_y_continuous(breaks=land.log.brks, labels=land.exp.brks, limits=c(-2,9))+
  theme_classic(base_size = 16) + scale_colour_gradientn(colours = terrain.colors(20))+theme(legend.position="none")

### Inset for shortspine
mro.sspn.inset <- ggplot(mro.ann[!(is.na(mtons)) & spp_common == "shortspine thornyhead"], 
               aes(y=log.mtons, x=log.bio)) +
  geom_point(size=2, shape=7)+ggtitle("")+ylab("") + xlab("")+
  geom_path(aes(colour=year)) + scale_x_continuous(breaks=bio.log.brks, labels=bio.exp.brks, limits=c(4.6,5.6))+scale_y_continuous(breaks=land.log.brks, labels=land.exp.brks)+
  theme_classic() + scale_colour_gradientn(colours = terrain.colors(20))+theme(legend.position="none", plot.margin=unit(c(0,0,0,0), "pt"))

mro2 <- ggdraw() + draw_plot(mro1, x=0, y=0, width=1, height=1) + draw_plot(mro.sspn.inset, x=0.25, y=0.62, width=0.4, height=0.3)

### Get rid of shortspine for BRG because overlapping with sablefish
brg1 <- ggplot(brg.ann[!(is.na(mtons)) & spp_common != "shortspine thornyhead"], 
               aes(y=log.mtons, x=log.bio, shape=spp_common)) +
  geom_point(size=2)+ggtitle("BRG")+ylab("Landings (mt)") + xlab("Stock Availability (mt)")+
  geom_path(aes(colour=year)) + scale_x_continuous(breaks=bio.log.brks, labels=bio.exp.brks)+scale_y_continuous(breaks=land.log.brks, labels=land.exp.brks, limits=c(-2,9))+
  theme_classic(base_size = 16) + scale_colour_gradientn(colours = terrain.colors(20)) +theme(legend.position="none")

### Inset for shortspine
brg.sspn.inset <- ggplot(brg.ann[!(is.na(mtons)) & spp_common == "shortspine thornyhead"], 
                         aes(y=log.mtons, x=log.bio)) +
  geom_point(size=2, shape=7)+ggtitle("")+ylab("") + xlab("")+
  geom_path(aes(colour=year)) + scale_x_continuous(breaks=bio.log.brks, labels=bio.exp.brks, limits=c(4.6,5.6))+scale_y_continuous(breaks=bio.log.brks, labels=bio.exp.brks)+
  theme_classic() + scale_colour_gradientn(colours = terrain.colors(20))+theme(legend.position="none", plot.margin=unit(c(0,0,0,0), "pt"))

brg2 <- ggdraw() + draw_plot(brg1, x=0, y=0, width=1, height=1) + draw_plot(brg.sspn.inset, x=0.56, y=0.16, width=0.4, height=0.3)



cos1 <- ggplot(cos.ann[!(is.na(mtons))], 
               aes(y=log.mtons, x=log.bio, shape=spp_common)) +
  geom_point(size=2)+ggtitle("COS")+ylab("Landings (mt)") + xlab("Stock Availability (mt)")+
  geom_path(aes(colour=year)) + scale_x_continuous(breaks=bio.log.brks, labels=bio.exp.brks)+scale_y_continuous(breaks=land.log.brks, labels=land.exp.brks, limits=c(-2,9))+
  theme_classic(base_size = 16) + scale_colour_gradientn(name="Year", colours = terrain.colors(20))+ scale_shape_discrete(name="Species")

legend <- get_legend(cos1) 


cos2 <- ggplot(cos.ann[!(is.na(mtons))& spp_common != "shortspine thornyhead"], 
               aes(y=log.mtons, x=log.bio, shape=spp_common)) +
  geom_point(size=2)+ggtitle("COS")+ylab("Landings (mt)") + xlab("Stock Availability (mt)")+
  geom_path(aes(colour=year)) + scale_x_continuous(breaks=bio.log.brks, labels=bio.exp.brks, limits=c(1.2,7))+scale_y_continuous(breaks=land.log.brks, labels=land.exp.brks, limits=c(-2,9))+
  theme_classic(base_size = 16) + scale_colour_gradientn(colours = terrain.colors(20))+theme(legend.position="none", plot.margin=unit(c(0,0,0,0), "pt"))

### Inset for shortspine
cos.sspn.inset <- ggplot(cos.ann[!(is.na(mtons)) & spp_common == "shortspine thornyhead"], 
                         aes(y=log.mtons, x=log.bio)) +
  geom_point(size=2, shape=7)+ggtitle("")+ylab("") + xlab("")+
  geom_path(aes(colour=year)) + scale_x_continuous(breaks=bio.log.brks, labels=bio.exp.brks, limits=c(5.5,6.3))+scale_y_continuous(breaks=bio.log.brks, labels=bio.exp.brks)+
  theme_classic() + scale_colour_gradientn(colours = terrain.colors(20))+theme(legend.position="none", plot.margin=unit(c(0,0,0,0), "pt"))

cos3 <- ggdraw() + draw_plot(cos2, x=0, y=0, width=1, height=1) + draw_plot(cos.sspn.inset, x=0.56, y=0.13, width=0.4, height=0.3)




#
ast1 <- ggplot(ast.ann[!(is.na(mtons))& spp_common != "shortspine thornyhead"], 
               aes(y=log.mtons, x=log.bio, shape=spp_common)) +
  geom_point(size=2)+ggtitle("AST")+ylab("Landings (mt)") + xlab("Stock Availability (mt)")+
  geom_path(aes(colour=year)) + scale_x_continuous(breaks=bio.log.brks, labels=bio.exp.brks)+scale_y_continuous(breaks=land.log.brks, labels=land.exp.brks, limits=c(-2,9))+
  theme_classic(base_size = 16) + scale_colour_gradientn(colours = terrain.colors(20))+theme(legend.position="none", plot.margin=unit(c(0,0,0,0), "pt"))

### Inset for shortspine
ast.sspn.inset <- ggplot(ast.ann[!(is.na(mtons)) & spp_common == "shortspine thornyhead"], 
                         aes(y=log.mtons, x=log.bio)) +
  geom_point(size=2, shape=7)+ggtitle("")+ylab("") + xlab("")+
  geom_path(aes(colour=year)) + scale_x_continuous(breaks=bio.log.brks, labels=bio.exp.brks, limits=c(5.5,6.25))+scale_y_continuous(breaks=bio.log.brks, labels=bio.exp.brks)+
  theme_classic() + scale_colour_gradientn(colours = terrain.colors(20))+theme(legend.position="none", plot.margin=unit(c(0,0,0,0), "pt"))

ast2 <- ggdraw() + draw_plot(ast1, x=0, y=0, width=1, height=1) + draw_plot(ast.sspn.inset, x=0.56, y=0.13, width=0.4, height=0.3)


png("Figures/BiovsLand_byFocalPort_allgear.png", height=8.5, width=11, units="in", res=300)
grid.arrange(mro2, brg2, legend, cos3, ast2,  ncol=3, widths=c(1,1,0.5))
dev.off()



# ggdraw(plot_grid(plot_grid(mro2, brg2, cos1, ast1, 
#                            labels=c("A", "B", "C", "D"), ncol=2, align='h'),
#                  plot_grid(NULL, legend, ncol=1),
#                  rel_widths=c(1, 0.3)))

# ==================================
# = Time Series Landings and Availability =
# ==================================
pdf("Figures/TS_LandvsAvail.pdf", height=7, width=7)
par(mfrow=c(2,2), mar=c(4,4,2,2), oma=c(0,0,2,0))
for(i in 1:length(port3)){
  plot(mtons ~ year, ann.dtspl.avail[port==port3[i] & spp_common=="shortspine thornyhead"], type="o", main=port3[i],
       ylim=c(0,700), pch=dts.pch[4], ylab="Landings (mt)")
  points(wtd.bio ~ year, ann.dtspl.avail[port==port3[i] & spp_common=="shortspine thornyhead"], type="o", col="blue",
         pch=dts.pch[4])
  mtext(side=4, "Stock Availability (mt)", cex=0.8, col="blue")
  
}
mtext(side=3, outer=T, "shortspine thornyhead")

par(mfrow=c(2,2), oma=c(0,0,2,0))
for(i in 1:length(port3)){
  plot(mtons ~ year, ann.dtspl.avail[port==port3[i] & spp_common=="sablefish"], type="o", main=port3[i],
       ylim=c(0,1500), pch=dts.pch[2], ylab="Landings (mt)")
  points(wtd.bio ~ year, ann.dtspl.avail[port==port3[i] & spp_common=="sablefish"], type="o", col="blue",
         pch=dts.pch[2])
  mtext(side=4, "Stock Availability (mt)", cex=0.8, col="blue")
  
}
mtext(side=3, outer=T, "sablefish")


par(mfrow=c(2,2), oma=c(0,0,2,0))
for(i in 1:length(port3)){
  plot(mtons ~ year, ann.dtspl.avail[port==port3[i] & spp_common=="dover sole"], type="o", main=port3[i],
       ylim=c(0,2500), pch=dts.pch[1])
  points(wtd.bio ~ year, ann.dtspl.avail[port==port3[i] & spp_common=="dover sole"], type="o", col="blue",
         pch=dts.pch[1])
}
mtext(side=3, outer=T, "dover sole")


par(mfrow=c(2,2), oma=c(0,0,2,0))
for(i in 1:length(port3)){
  plot(mtons/10 ~ year, ann.dtspl.avail[port==port3[i] & spp_common=="petrale sole"], type="o", main=port3[i],
       ylim=c(0,50), pch=dts.pch[3], ylab="Landings (10 mts)")
  points(wtd.bio ~ year, ann.dtspl.avail[port==port3[i] & spp_common=="petrale sole"], type="o", col="blue",
         pch=dts.pch[3])
  mtext(side=4, "Stock Availability (mt)", cex=0.8, col="blue")
}
mtext(side=3, outer=T, "petrale sole")



par(mfrow=c(2,2), oma=c(0,0,2,0))
for(i in 1:length(port3)){
  plot(mtons/10 ~ year, ann.dtspl.avail[port==port3[i] & spp_common=="lingcod"], type="o", main=port3[i],
       ylim=c(0,100), pch=dts.pch[5], ylab="Landings (10 mts)")
  points(wtd.bio ~ year, ann.dtspl.avail[port==port3[i] & spp_common=="lingcod"], type="o", col="blue",
         pch=dts.pch[5])
  mtext(side=4, "Stock Availability (mt)", cex=0.8, col="blue")
  
}
mtext(side=3, outer=T, "lingcod")
dev.off()

# ==================================
# = Revenue vs Availability =
# ==================================

mro3 <- ggplot(mro.ann[!(is.na(mtons))], 
               aes(y=percent.rev, x=log.bio, shape=spp_common)) +
  geom_point(size=2)+ggtitle("MRO")+ylab("% Revenue") + xlab("Stock Availability (log(mt))")+
  geom_path(aes(colour=year)) +
  theme_classic() + scale_colour_gradientn(colours = terrain.colors(20))

legend <- get_legend(mro3)
mro4 <- mro3 +theme(legend.position="none")


brg3 <- ggplot(brg.ann[!(is.na(mtons))], 
               aes(y=percent.rev, x=log.bio, shape=spp_common)) +
  geom_point(size=2)+ggtitle("BRG")+ylab("% Revenue") + xlab("Stock Availability (log(mt))")+
  geom_path(aes(colour=year)) +
  theme_classic() + scale_colour_gradientn(colours = terrain.colors(20)) +theme(legend.position="none")

erk3 <- ggplot(erk.ann[!(is.na(mtons))], 
               aes(y=percent.rev, x=log.bio, shape=spp_common)) +
  geom_point(size=2)+ggtitle("ERK")+ylab("% Revenue") + xlab("Stock Availability (log(mt))")+
  geom_path(aes(colour=year)) +
  theme_classic() + scale_colour_gradientn(colours = terrain.colors(20))+theme(legend.position="none")

crs3 <- ggplot(crs.ann[!(is.na(mtons))], 
               aes(y=percent.rev, x=log.bio, shape=spp_common)) +
  geom_point(size=2)+ggtitle("CRS")+ylab("% Revenue") + xlab("Stock Availability (log(mt))")+
  geom_path(aes(colour=year)) +
  theme_classic() + scale_colour_gradientn(colours = terrain.colors(20))+theme(legend.position="none")

cos3 <- ggplot(cos.ann[!(is.na(mtons))], 
               aes(y=percent.rev, x=log.bio, shape=spp_common)) +
  geom_point(size=2)+ggtitle("COS")+ylab("% Revenue") + xlab("Stock Availability (log(mt))")+
  geom_path(aes(colour=year)) +
  theme_classic() + scale_colour_gradientn(colours = terrain.colors(20))+theme(legend.position="none")

new3 <- ggplot(new.ann[!(is.na(mtons))], 
               aes(y=percent.rev, x=log.bio, shape=spp_common)) +
  geom_point(size=2)+ggtitle("NEW")+ylab("% Revenue") + xlab("Stock Availability (log(mt))")+
  geom_path(aes(colour=year)) +
  theme_classic() + scale_colour_gradientn(colours = terrain.colors(20))+theme(legend.position="none")

#
ast3 <- ggplot(ast.ann[!(is.na(mtons))], 
               aes(y=percent.rev, x=log.bio, shape=spp_common)) +
  geom_point(size=2)+ggtitle("AST")+ylab("% Revenue") + xlab("Stock Availability (log(mt))")+
  geom_path(aes(colour=year)) +
  theme_classic() + scale_colour_gradientn(colours = terrain.colors(20))+theme(legend.position="none")


png("Figures/BiovsRev_byFocalPort.png", height=7, width=7, units="in", res=300)
ggdraw(plot_grid(plot_grid(mro4, brg3, cos3, ast3, 
                           labels=c("A", "B", "C", "D"), ncol=2, align='v'),
                 plot_grid(NULL, legend, ncol=1),
                 rel_widths=c(1, 0.3)))
dev.off()

# ==================================
# = Stock Availability by Port  =
# ==================================
# 
# pdf("Figures/StockBiobyPort_DTSPling.pdf", height=6, width=6)
# par(mfrow=c(2,2))
# cc_spp_bio_port[,j={
#   t.dt <- .SD
#   plot(wtd.bio ~ year, .SD, col="white", 
#        main=paste0(port, " N=", unique(port.N)), ylab="Stock Biomass")
#   for(i in 1:length(dtspling.spp)){
#     sub <- t.dt[spp_common==dtspling.spp[i]]
#     points(wtd.bio ~ year, sub, type="o", col=dts.color[i], pch=dts.pch[i], lwd=1)
#   }
#   abline(h=0, lty=2)
# }, by=list(port)]
# plot(x=c(0,1), y=c(0,1), col="white", axes=F, xlab="", ylab="")
# legend("topleft", legend=dtspling.spp, lty=1, pch=dts.pch, col=dts.color, xpd=T, lwd=1.5, bty="n")
# dev.off()
# 
# pdf("Figures/logStockBiobyPort_DTSPling.pdf", height=6, width=6)
# par(mfrow=c(2,2))
# cc_spp_bio_port[,j={
#   t.dt <- .SD
#   plot(log(wtd.bio) ~ year, .SD, col="white", ylim=c(1,8),  
#        main=paste0(port, " N=", unique(port.N)), ylab="Log(Stock Biomass)")
#   for(i in 1:length(dtspling.spp)){
#     sub <- t.dt[spp_common==dtspling.spp[i]]
#     points(log(wtd.bio) ~ year, sub, type="o", col=dts.color[i], pch=dts.pch[i], lwd=1)
#   }
#   abline(h=0, lty=2)
# }, by=list(port)]
# plot(x=c(0,1), y=c(0,1), col="white", axes=F, xlab="", ylab="")
# legend("topleft", legend=dtspling.spp, lty=1, pch=dts.pch, col=dts.color, xpd=T, lwd=1.5, bty="n")
# dev.off()

############### FIGURE S1 ####################

png("Figures/logStockBiobySelectPort_DTSPling.png", height=7, width=6, units="in", res=300)
par(mfrow=c(2,2), mar=c(4,4,2,2), oma=c(0,0,4,0))
cc_spp_bio_port[port %in% port.select,j={
  t.dt <- .SD
  plot(log(wtd.bio) ~ year, .SD, col="white",  ylim=c(1,8), 
       main=paste0(port), ylab="Stock Availability ((mt))", yaxt="n")
  axis(side=2, at=bio.log.brks, labels=bio.exp.brks, las=1)
  for(i in 1:length(dtspling.spp)){
    sub <- t.dt[spp_common==dtspling.spp[i]]
    points(log(wtd.bio) ~ year, sub, type="o", col=dts.color[i], pch=dts.pch[i], lwd=1)
  }
  abline(h=0, lty=2)
  abline(v=2003, col="grey", lty=2)
}, by=list(port)]
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("top", legend=dtspling.spp, pch=dts.pch, col=dts.color, xpd = TRUE, ncol=3, inset = c(0, 
    0), bty = "n", cex = 1)
# plot(x=c(0,1), y=c(0,1), col="white", axes=F, xlab="", ylab="")
# legend("topleft", legend=dtspling.spp, lty=1, pch=dts.pch, col=dts.color, xpd=T, lwd=1.5, bty="n")
dev.off()

# pdf("Figures/loganomStockBiobyPort_DTSPling.pdf", height=6, width=6)
# par(mfrow=c(2,2))
# cc_spp_bio_port[,j={
#   t.dt <- .SD
#   plot(bio.anom ~ year, .SD, col="white",  
#        main=paste0(port, " N=", unique(port.N)), ylab="Log(Stock Biomass) Anomaly")
#   for(i in 1:length(dtspling.spp)){
#     sub <- t.dt[spp_common==dtspling.spp[i]]
#     points(bio.anom ~ year, sub, type="o", col=dts.color[i], pch=dts.pch[i], lwd=1)
#   }
#   abline(h=0, lty=2)
# }, by=list(port)]
# plot(x=c(0,1), y=c(0,1), col="white", axes=F, xlab="", ylab="")
# legend("topleft", legend=dtspling.spp, lty=1, pch=dts.pch, col=dts.color, xpd=T, lwd=1.5, bty="n")
# dev.off()
# 
# 
# pdf("Figures/StockDistbyPort_DTSPling.pdf", height=6, width=6)
# par(mfrow=c(2,2))
# cc_spp_dist_port[,j={
#   t.dt <- .SD
#   plot(wtd.distN ~ year, .SD, col="white",  
#        main=paste0(port, " N=", unique(port.N)), ylab="Weighted Distance from Port (km N)")
#   for(i in 1:length(dtspling.spp)){
#     sub <- t.dt[spp_common==dtspling.spp[i]]
#     points(wtd.distN ~ year, sub, type="o", col=dts.color[i], pch=dts.pch[i], lwd=1)
#   }
#   abline(h=0, lty=2)
# }, by=list(port)]
# plot(x=c(0,1), y=c(0,1), col="white", axes=F, xlab="", ylab="")
# legend("topleft", legend=dtspling.spp, lty=1, pch=dts.pch, col=dts.color, xpd=T, lwd=1.5, bty="n")
# dev.off()
# 
# 
# 
# 
# 

###################### FIGURE 3 ######################

png("Figures/logStockBiobySpp_DTSPling.png", height=8, width=6, units="in", res=300)
par(mfrow=c(3,2), mar=c(4,4,2,2))
cc_spp_bio_port[port %in% port3,j={
  t.dt <- .SD
  plot(log(wtd.bio) ~ year, .SD, col="white",  
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



# ==================================
# = Number of Vessels Landing by Port and Gear  =
# ==================================
ann.vess <- melt(ann.dtspl, id.vars=c("LANDING_YEAR", "PACFIN_PORT_CODE", "spid"), 
                 measure.vars=c("num.vess.twl", "num.vess.hkl", "num.vess.pot"),
                 variable.name="Type", value.name="NumVess")
ann.vess[,"Gear":=as.factor(ifelse(Type=="num.vess.twl", "twl", ifelse(Type=="num.vess.hkl", "hkl", "pot")))]

ann.land.gear <- melt(ann.dtspl, id.vars=c("LANDING_YEAR", "PACFIN_PORT_CODE", "spid"), 
                      measure.vars=c("land.twl", "land.hkl", "land.pot"),
                 variable.name="Type", value.name="Land")
ann.land.gear[,"Gear":=as.factor(ifelse(Type=="land.twl", "twl", ifelse(Type=="land.hkl", "hkl", "pot")))]


mro.sspn.vess <- ggplot(data=ann.vess[spid=="SSPN" & PACFIN_PORT_CODE=="MRO" & LANDING_YEAR>=1995], 
       aes(x=LANDING_YEAR, y=NumVess, fill=Gear)) + 
  labs(title="Shortspine thornyhead", x ="Year", y = "Number of vessels")+
  geom_bar(stat="identity") + geom_vline(xintercept=2006, linetype=2)+ geom_vline(xintercept=2011, linetype=3)

mro.sspn.land <- ggplot(data=ann.land.gear[spid=="SSPN" & PACFIN_PORT_CODE=="MRO" & LANDING_YEAR>=1995], 
       aes(x=LANDING_YEAR, y=Land, fill=Gear)) +
  labs(title="Shortspine thornyhead", x ="Year", y = "Landings (mt)")+
  #scale_fill_discrete(name="Gear",breaks=c("num.vess.twl", "num.vess.hkl", "num.vess.pot"),labels=c("twl", "hkl", "pot"))+
  geom_bar(stat="identity")+ geom_vline(xintercept=2006, linetype=2)+ geom_vline(xintercept=2011, linetype=3)
  

mro.sable.vess <- ggplot(data=ann.vess[spid=="SABL" & PACFIN_PORT_CODE=="MRO" & LANDING_YEAR>=1995], 
                        aes(x=LANDING_YEAR, y=NumVess, fill=Gear)) +
  labs(title="Sablefish", x ="Year", y = "Number of vessels")+
  geom_bar(stat="identity")+ geom_vline(xintercept=2006, linetype=2)+ geom_vline(xintercept=2011, linetype=3)

mro.sable.land <- ggplot(data=ann.land.gear[spid=="SABL" & PACFIN_PORT_CODE=="MRO" & LANDING_YEAR>=1995], 
                        aes(x=LANDING_YEAR, y=Land, fill=Gear)) +
  labs(title="Sablefish", x ="Year", y = "Landings (mt)")+
  geom_bar(stat="identity")+ geom_vline(xintercept=2006, linetype=2) + geom_vline(xintercept=2011, linetype=3)

png("Figures/MRO_SSPNvsSABL.png", height=7, width=7, units="in", res=300)
ggdraw(plot_grid(mro.sspn.vess, mro.sable.vess, mro.sspn.land, mro.sable.land, 
                 labels=c("A", "B", "C", "D"), ncol=2, align='h'))
dev.off()

#### Astoria
ast.sspn.vess <- ggplot(data=ann.vess[spid=="SSPN" & PACFIN_PORT_CODE=="AST" & LANDING_YEAR>=1995], 
                        aes(x=LANDING_YEAR, y=NumVess, fill=Gear)) + 
  labs(title="Shortspine thornyhead", x ="Year", y = "Number of vessels")+
  geom_bar(stat="identity") +geom_vline(xintercept=2011, linetype=3)

ast.sspn.land <- ggplot(data=ann.land.gear[spid=="SSPN" & PACFIN_PORT_CODE=="AST" & LANDING_YEAR>=1995], 
                        aes(x=LANDING_YEAR, y=Land, fill=Gear)) +
  labs(title="Shortspine thornyhead", x ="Year", y = "Landings (mt)")+
  #scale_fill_discrete(name="Gear",breaks=c("num.vess.twl", "num.vess.hkl", "num.vess.pot"),labels=c("twl", "hkl", "pot"))+
  geom_bar(stat="identity")+  geom_vline(xintercept=2011, linetype=3)


ast.sable.vess <- ggplot(data=ann.vess[spid=="SABL" & PACFIN_PORT_CODE=="AST" & LANDING_YEAR>=1995], 
                         aes(x=LANDING_YEAR, y=NumVess, fill=Gear)) +
  labs(title="Sablefish", x ="Year", y = "Number of vessels")+
  geom_bar(stat="identity")+  geom_vline(xintercept=2011, linetype=3)

ast.sable.land <- ggplot(data=ann.land.gear[spid=="SABL" & PACFIN_PORT_CODE=="AST" & LANDING_YEAR>=1995], 
                         aes(x=LANDING_YEAR, y=Land, fill=Gear)) +
  labs(title="Sablefish", x ="Year", y = "Landings (mt)")+
  geom_bar(stat="identity")+geom_vline(xintercept=2011, linetype=3)

png("Figures/AST_SSPNvsSABL.png", height=7, width=7, units="in", res=300)
ggdraw(plot_grid(ast.sspn.vess, ast.sable.vess, ast.sspn.land, ast.sable.land, 
                 labels=c("A", "B", "C", "D"), ncol=2, align='h'))
dev.off()