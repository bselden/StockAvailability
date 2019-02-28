library(data.table)
library(Hmisc)
library(rgdal)

#### Load logbook distance data (from PacFIN_Logbook_Analysis.R)
#### Catch records were limited to those catching DTSPL
#### Distance between harvest location and port were calculate with geosphere
#### Identifying information was removed, saving only year, port, species, distance
lb <- readRDS("Data/logbook_rawdist.rds")

### Weighted quantiles of distance between harvest location and port weighted by catch of DTSPL
### all records pooled
quant.dist <- lb[,list(q10=wtd.quantile(dist_set_km, weights=APOUNDS, probs=c(0.1)),
                       q25=wtd.quantile(dist_set_km, weights=APOUNDS, probs=c(0.25)),
                       q50=wtd.quantile(dist_set_km, weights=APOUNDS, probs=c(0.5)),
                       q75=wtd.quantile(dist_set_km, weights=APOUNDS, probs=c(0.75)),
                       q90=wtd.quantile(dist_set_km, weights=APOUNDS, probs=c(0.9)),
                       n=length(unique(record))),
                 by=list(PORT, group=as.numeric(PORT))]

setorder(quant.dist, group)
port_lim <- quant.dist$PORT
port_lim2 <- port_lim[!(port_lim %in% c("AVL", "BRK"))]

quant.dist2 <- quant.dist[PORT %in% port_lim2]

### Use bxp to plot boxplot from these quantiles
bxp.data <- list(stats=t(data.matrix(quant.dist2[,c("q10", "q25", "q50", "q75", "q90")])), n=quant.dist2$n,
                 names=quant.dist2$PORT)

bxp(bxp.data, horizontal=T, las=1,
    ylab="Port", 
    xlab="Raw Distance\nWeighted by Catch (km)", 
    main="DTSPL Catch Location 1981-2015")



### Weighted mean of distance between harvest location and port weighted by catch of DTSPL by YEAR
dist.wtd.port <- lb[!(is.na(APOUNDS)) & PORT %in% port_lim2,
                    list(wtd.dist=weighted.mean(dist_set_km, APOUNDS, na.rm=T)), by=list(PORT, RYEAR)]
dist.wtd.port[,"PORT2":=factor(PORT)] #get rid of unused port levels

### Boxplot of weighted mean showing distribution of this mean value over all years 1981-2015
boxplot(wtd.dist ~ PORT2, dist.wtd.port, 
        ylab="Port", 
        xlab="Annual Mean Distance\nWeighted by Catch (km)", 
        main="DTSPL by Year 1981-2015", 
        horizontal=T, las=1)



### Read in port locations and state shapefile
port_locs<- read.csv("Data/PacFIN & BEF ports LUT.csv")
states <- readOGR("GIS_Data/cb_2016_us_state_500k", "cb_2016_us_state_500k")

w_states <- states[states$NAME %in% c("California", "Oregon", "Washington"),]

png("Figures/RawDistLogbook.png", height=5, width=8, units="in", res=300)
par(mfrow=c(1,2))
plot(w_states)
points(Lat ~ Lon, subset(port_locs, Pcid %in% port_lim2), pch=19)
text(x=subset(port_locs, Pcid %in% port_lim2)$Lon - 2, y=subset(port_locs, Pcid %in% port_lim2)$Lat,
     labels=subset(port_locs, Pcid %in% port_lim2)$Pcid)

bxp(bxp.data, horizontal=T, las=1,
    ylab="Port", 
    xlab="Raw Distance\nWeighted by Catch (km)", 
    main="DTSPL Catch Location 1981-2015")
dev.off()

png("Figures/AnnualMeanDistLogbook.png", height=5, width=8, units="in", res=300)
par(mfrow=c(1,2))
plot(w_states)
points(Lat ~ Lon, subset(port_locs, Pcid %in% port_lim2), pch=19)
text(x=subset(port_locs, Pcid %in% port_lim2)$Lon - 2, y=subset(port_locs, Pcid %in% port_lim2)$Lat,
     labels=subset(port_locs, Pcid %in% port_lim2)$Pcid)

boxplot(wtd.dist ~ PORT2, dist.wtd.port[PORT %in% port_lim2], 
        ylab="Port", 
        xlab="Annual Mean Distance\nWeighted by Catch (km)", 
        main="DTSPL by Year 1981-2015", 
        horizontal=T, las=1)
dev.off()