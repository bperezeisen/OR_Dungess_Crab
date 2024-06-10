
setwd("C:/Users/brian/OneDrive/CrabCode")


library(tidyverse)
library(lubridate)
library(rdrobust)
library(sf)
library(purrr)
library(haven)
library(magick)
library(ggmap)
library(basemaps)
library(patchwork)
library(elevatr)
library(terra)
library(tidyterra)
library(ggspatial)
register_stadiamaps(key = "82f84587-7c05-4f62-8090-fe441344db03")
classic_blue <- "#0F4C81"
sapphire <- "#255F85"
sky_blue <- "#6596CD"
prussian <- "#293E66"
rainy_day <- "#474C5c"
glossy_grape <- "#A799B7"
pink <- "#EFAAC4"
raspberry <- "#C33149"
violet <- "#5f4b8b"
dark_violet <- "#351C45"
onyx <- "#383C42"
dim_gray <- "#6B717E"
gainsboro <- "#D8D8D8"
metallic <- "#837A75"
slate <- "#3f3f3f"
charcoal <- "#2E4057"
claret <- "#8B1E3F"
pepper <- "#9b1b30"
dark_magenta <- "#861B54"
eton <- "#739F8F"
emerald <- "#00997b"
dark_emerald <- "#2d6d66"
saffron <- "#F49D37"
light_grey = "#EFEFEF"

usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")
rivers <- st_read("GIS/ne_10m_rivers_lake_centerlines.shp")
places <- st_read("GIS/ne_10m_populated_places.shp")
dem <- rast("GIS/GRAY_HR_SR.tif")
dem
oregon <- st_read("GIS/Oregon_State_Boundary.shp")
oregon <- st_transform(oregon, crs = 4326)
crop_region_with_polygon <- function() {
  region_vect <- terra::vect(oregon$geometry)
  region_raster <- terra::crop(dem, region_vect)
  region_raster_final <- terra::mask(
    region_raster, region_vect
  )
  return(region_raster_final)
}
region_vect <- terra::vect(oregon$geometry)
region_raster_final <- crop_region_with_polygon()
region_raster_final<- region_raster_final %>% as.data.frame(xy=T) 
ggplot()+  
  geom_hline(data  = mgt_zones, mapping=aes(yintercept=top_lat), color = violet, linetype="dashed", size=0.5)+
  geom_text(data=mgt_zones, mapping=aes(y=avg_lat, label=id), x=-124.8, hjust=0, size=4, show.legend = F, family="sans")+
  geom_tile(data = region_raster_final, aes(x=x, y =y, fill = GRAY_HR_SR)) + 
  scale_fill_gradientn(colours = hcl.colors(7, "Grays")) + 
  coord_sf(crs = 4326, xlim = c(-125, -123.8), ylim = c(42, 46.25)) +
 # scale_x_continuous(labels = scales::label_number(accuracy = 0.01), limits = c(-125,-124.5), breaks =seq (-125,-124.5, .5)) +
#  scale_y_continuous(labels = scales::label_number(accuracy = 0.01), limits = c(42, 46.25))+
  ylab(label = "Latitude")+ xlab(label = "Longitude")+
  annotation_north_arrow(location = "br", which_north = "true", pad_x = unit(.2, "in"), pad_y = unit(0.5, "in"),style = north_arrow_fancy_orienteering) +
  theme(
    panel.spacing = unit(c(0, 0, 0, 0), "cm"),
    axis.title.y = element_text(size = 12, face="bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    legend.position = "none",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text.x=element_text(angle = 0, hjust = .5, vjust = 1, size =12),
    axis.text.y = element_text(angle = 0, hjust = .5, vjust = 1, size =12),
    plot.margin=unit(c(0,0,0,0),"null"),
    axis.ticks.length = unit(0,"pt"),
    text = element_text(family = "sans"),
    aspect.ratio = 3)
  

region_raster_final |>
  as.data.frame(xy = T) |>
  ggplot() +
  geom_tile(
    aes(x = x, y = y, fill = file514862c13e19)
  ) +
  geom_sf(
    data = country_sf,
    fill = "transparent", color = "black", size = .25
  ) +
  theme_void()
oregon_vector <- oregon %>% st_transform(crs = terra::crs(dem)) %>% vect()
or_ext<- ext(-124.2,-123, 42, 47)
oregon_dem <- crop(dem, or_ext) %>% mask(oregon_vector)
ggplot()+ geom_spatvector(data=region_vect) + scale_fill_viridis_c()
#OR Harvest Areas
#using decimal formatting
mgt_zones <- data.frame(id = c("50-A","50-B","50-C","50-D","50-E","50-F","50-G","50-H","50-I","50-J","50-K","50-L"),
                        top_lat = c(46.25,45.766667,45.338333,45.06667,44.773333,44.4333333,44.138333,43.783333,43.416667,43.116667,42.833333,42.433333),
                        bottom_lat = c(45.76667,45.338333,45.066667,44.773333,44.433333,44.138333,43.783333,43.416667,43.116667,42.833333,42.433333,42))
#For geom text
mgt_zones$avg_lat = (mgt_zones$top_lat+mgt_zones$bottom_lat)/2
areas<-ggplot() + 
  geom_hline(data  = mgt_zones, mapping=aes(yintercept=top_lat), color = violet, linetype="dashed", size=0.5)+
  geom_text(data=mgt_zones, mapping=aes(y=avg_lat, label=id), x=-124.8, hjust=0, size=4, show.legend = F, family="sans") +
  geom_hline(yintercept = 42, color = violet, linetype="dashed", size=0.5)+
  geom_sf(data=foreign, fill = light_grey, color = "white", lwd=.03)+
  geom_sf(data=usa, fill = light_grey, color = "white", lwd=.03)+
  geom_sf(data = rivers, color = "blue")+
  geom_text(data = places, mapping = aes(y=LATITUDE, x = LONGITUDE,label=NAME),hjust=0, size=8, show.legend = F, family="sans" )+
  geom_sf(data = places)+
  coord_sf(crs = 4326, xlim = c(-125, -123.8), ylim = c(42, 46.25)) +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.01), limits = c(-125,-124.5), breaks =seq (-125,-124.5, .5)) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01), limits = c(42, 46.25))+
  ylab(label = "Latitude")+ xlab(label = "Longitude")+
  annotation_north_arrow(location = "br", which_north = "true", pad_x = unit(.2, "in"), pad_y = unit(0.5, "in"),style = north_arrow_fancy_orienteering) +
  theme(
    panel.spacing = unit(c(0, 0, 0, 0), "cm"),
    axis.title.y = element_text(size = 12, face="bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    legend.position = "none",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text.x=element_text(angle = 0, hjust = .5, vjust = 1, size =12),
    axis.text.y = element_text(angle = 0, hjust = .5, vjust = 1, size =12),
    plot.margin=unit(c(0,0,0,0),"null"),
    axis.ticks.length = unit(0,"pt"),
    text = element_text(family = "sans"),
    aspect.ratio = 3)

print(areas)
register_stadiamaps(key = "82f84587-7c05-4f62-8090-fe441344db03")
#ggsave(areas, filename="Figures/harvestareas.png",
  #     width=6.5, height=5.75, units="in", dpi=1200)
ext<- st_bbox(c(xmin = -125, xmax = -123, ymax = 47, ymin = 42), crs = st_crs(4326))
# Get the map from ggmap
map <- get_stadiamap(bbox  = c(left = -125, bottom = 40, right = -123, top = 47),
               source = "stamen", maptype = "alidade_smooth", zoom = 9)
ggmap(map) +
  geom_hline(data  = mgt_zones, mapping=aes(yintercept=top_lat), color = violet, linetype="dashed", size=0.5)+
  geom_text(data=mgt_zones, mapping=aes(y=avg_lat, label=id), x=-124.8, hjust=0, size=4, show.legend = F, family="sans") +
  geom_hline(yintercept = 42, color = violet, linetype="dashed", size=0.5)+
  coord_sf(crs = 4326, xlim = c(-125, -123.8), ylim = c(42, 46.25))+
  scale_x_continuous(labels = scales::label_number(accuracy = 0.01), limits = c(-125,-124.5), breaks =seq (-125,-124.5, .5)) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01), limits = c(42, 46.25))+
  annotation_north_arrow(location = "br", which_north = "true", pad_x = unit(.2, "in"), pad_y = unit(0.5, "in"),style = north_arrow_fancy_orienteering) +
  theme(
    panel.spacing = unit(c(0, 0, 0, 0), "cm"),
    axis.title.y = element_text(size = 12, face="bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    legend.position = "none",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text.x=element_text(angle = 0, hjust = .5, vjust = 1, size =12),
    axis.text.y = element_text(angle = 0, hjust = .5, vjust = 1, size =12),
    plot.margin=unit(c(0,0,0,0),"null"),
    axis.ticks.length = unit(0,"pt"),
    text = element_text(family = "sans"))
