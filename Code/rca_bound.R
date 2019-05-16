### RCA Boundaries
library(rgdal)

### 2015-2016 from FRAM
ogrListLayers("GIS_Data/FRAM_RCA_CommTrawl_2015_16_poly.kml")



### 2019-2020 from PFMC
# https://www.pcouncil.org/habitat-and-communities/area-closures/

ogrListLayers("GIS_Data/Trawl RCA 201920 Polylines.kml")

rca1920 <- readOGR("GIS_Data/Trawl RCA 201920 Polylines.kml", "Trawl RCA 201920  Polylines")
