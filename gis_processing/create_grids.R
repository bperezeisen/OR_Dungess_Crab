library(tidyverse)
library(sf)
setwd("C:/Users/brian/OneDrive/CrabCode")
#R script to switch GIS processing into R from ARCGIS
#oregon shapefile layer downloaded from https://geohub.oregon.gov/datasets/oregon-geo::oregon-state-boundary/explore?location=43.524638%2C-123.512203%2C8.93
#with additional processing in ARCGIS to remove objects not part of mainland mass
#read in the oregon shapefile in the GIS folder
oregon <- st_read("GIS/Oregon_State_Boundary_1.shp")

#these define the evisceration order boundary lines in decimal degrees
coords <- matrix(c(
  -125, 44.13833333, -123, 44.13833333,
  -125, 43.36, -123, 43.36,
  -125, 42.83333333, -123, 42.83333333
), ncol = 2, byrow = TRUE)

ESRI.102422 <- "+proj=eqc +lat_ts=41.12682127 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# Create an sf object for each border
f.g.border <- st_linestring(coords[1:2,]) %>%
  st_sfc(crs = 4326) %>%  
  st_sf() %>%
  st_transform(crs = ESRI.102422)
h.i.border <- st_linestring(coords[3:4,]) %>%
  st_sfc(crs = 4326) %>%  # Define the coordinate reference system (CRS) as WGS 84 (EPSG:4326)
  st_sf()%>%
  st_transform(crs = ESRI.102422)
j.k.border <- st_linestring(coords[5:6,]) %>%
  st_sfc(crs = 4326) %>%
  st_sf()%>%
  st_transform(crs = ESRI.102422)

#"Evisceration order 43° 21.60′ to 44° 08.30′
#43.36 to 44.1383333
#Coos Bay North Jetty(43º 21.60' N. Lat) to Heceta Head (44° 08.30' N. Lat)"
# Heceta Head
#workflow is to create the buffer for the border and then create 1km square grids
f_g<-st_buffer(f.g.border, dist = 30000, endCapStyle = "FLAT")
tmp<-st_make_grid(f_g, cellsize = c(1000,1000), square = TRUE)
tmp <- st_as_sf(tmp)
#Coos Bay
h_i <- st_buffer(h.i.border, dist = 30000, endCapStyle = "FLAT")
tmp2 <- st_make_grid(h_i, cellsize = c(1000,1000), square = TRUE)
tmp2 <- st_as_sf(tmp2)
#combine both grids for 2016-2017
grids<- rbind(tmp, tmp2)
# Cape Blanco to OR/CA Border 42º 50' 00" N. Lat. To 42º 00' 00" N. Lat.
#Cape Blanco border
j_k <- st_buffer(j.k.border, dist = 30000, endCapStyle = "FLAT")
tmp3<- st_make_grid(j_k, cellsize = c(1000, 1000), square = TRUE)
tmp3<- st_as_sf(tmp3)
# Can be computationally intensive 
#creates grids with land erased for 2016-2017
system.time(int.idx <- st_intersects(grids, st_transform(oregon, crs = ESRI.102422))) 
int.idx.which <- which(sapply(int.idx, length) > 0)
system.time(grid.lint <- st_difference(grids[int.idx.which, ], st_transform(oregon, crs = ESRI.102422))) 
grid.lint<-grid.lint %>% dplyr::select(x)
grids.16.17 <- rbind(grids[-int.idx.which, ], grid.lint)
#creates grids with land erased for 2017-2018
int.idx <- st_intersects(tmp3, oregon) 
int.idx.which <- which(sapply(int.idx, length) > 0)
system.time(grid.lint <- st_difference(tmp3[int.idx.which, ], oregon)) 
grid.lint<-grid.lint %>% dplyr::select(x)
grids.17.18 <- rbind(tmp3[-int.idx.which, ], grid.lint)
#plot check
ggplot() + geom_sf(data = grids.16.17)
ggplot() + geom_sf(data = grids.17.18)
st_write(grids.16.17, "GIS/grids_16_17_lno.shp")
st_write(grids.17.18, "GIS/grids_17_18_lno.shp")
