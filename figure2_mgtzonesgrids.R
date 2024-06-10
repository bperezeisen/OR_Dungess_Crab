library(tidyverse)
library(sf)
library(patchwork)
library(ggspatial)
library(ggmap)
library(ggrepel)
library(basemaps)
setwd("C:/Users/brian/OneDrive/CrabCode")
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
mgt_zones <- data.frame(id = c("50-A","50-B","50-C","50-D","50-E","50-F","50-G","50-H","50-I","50-J","50-K","50-L"),
                        top_lat = c(46.25,45.766667,45.338333,45.06667,44.773333,44.4333333,44.138333,43.783333,43.416667,43.116667,42.833333,42.433333),
                        bottom_lat = c(45.76667,45.338333,45.066667,44.773333,44.433333,44.138333,43.783333,43.416667,43.116667,42.833333,42.433333,42))
#For geom text
mgt_zones$avg_lat = (mgt_zones$top_lat+mgt_zones$bottom_lat)/2
basemap <- get_stadiamap(bbox = c(left = -125, bottom = 42.5, right = -124.2, top = 43.4),
                   source = "stadia", maptype = "stamen_toner", zoom = 10)
grids_F_G <- st_read("GIS/grids_F_G.shp")
grids_F_G <- grids_F_G %>% mutate(grid=row_number())
grids_H_I <- st_read("GIS/grids_H_I.shp")
grids_H_I <- grids_H_I %>% mutate(grid = row_number())
grids_J_K <- st_read("GIS/grids_J_K.shp")
grids_J_K <- grids_J_K %>% mutate(grid = row_number())

grids_16_17 <- rbind(grids_F_G, grids_H_I)
ggplot() + geom_sf(data = grids_16_17)

grid1<-ggplot() + geom_sf(data = grids_16_17, fill = sky_blue) +
  geom_hline(data = mgt_zones, mapping=aes(yintercept=top_lat), color = violet, linetype="dashed", size=0.5) +
  geom_hline(yintercept = 43.36, color = claret)+ 
  geom_hline(yintercept = 44.1383333, color = claret)+ 
  geom_sf(data=usa, fill = light_grey, color = "white", lwd=.03)+
  geom_text_repel(data=mgt_zones, mapping=aes(y=avg_lat, label=id), x=-123.8, hjust=0, size=4, show.legend = F, family="sans") +
  coord_sf(crs = 4326, xlim = c(-125, -123.8), ylim = c(43, 44.6)) +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.01), limits = c(-125,-124.2), breaks =seq (-125,-124.5, .5)) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01), limits = c(42, 46.25), breaks =seq (42,46.25, .2))+
  labs(title = "2016-2017")+
  ylab(label = "Latitude")+ xlab(label = "Longitude")+
  annotation_scale(location = "bl", style = "ticks", width_hint = 0.2)+
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

print(grid1)
  
grid2<-ggplot() + geom_sf(data = grids_J_K, fill = sky_blue) +
  geom_hline(data = mgt_zones, mapping=aes(yintercept=top_lat), color = violet, linetype="dashed", size=0.5) +
  geom_hline(yintercept = 42.83333, color = claret, size = 1)+
  geom_sf(data=usa, fill = light_grey, color = "white", lwd=.03)+
  geom_text_repel(data=mgt_zones, mapping=aes(y=avg_lat, label=id), x=-124.3, hjust=0, size=4, show.legend = F, family="sans") +
  coord_sf(crs = 4326, xlim = c(-125, -124.2), ylim = c(42.5, 43.4)) +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.01), limits = c(-125,-124.2), breaks =seq (-125,-124.5, .5)) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01), limits = c(42, 46.25), breaks =seq (42,46.25, .2))+
  labs(title = "2017-2018")+
  theme(
    panel.spacing = unit(c(0, 0, 0, 0), "cm"),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "none",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text.x=element_text(angle = 0, hjust = .5, vjust = 1, size =12),
    axis.text.y = element_text(angle = 0, hjust = .5, vjust = 1, size =12),
    plot.margin=unit(c(0,0,0,0),"null"),
    axis.ticks.length = unit(0,"pt"),
    text = element_text(family = "sans"))
grids <- grid1+ grid2 + plot_layout(axis_titles = "collect")
print(grids) 
#ggsave(grids, filename = "Figures/harvestarea_grids.png", width=6.5, height=5.75, units="in", dpi=1200)

