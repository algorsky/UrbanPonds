setwd("/Users/adriannagorsky/Documents/UrbanPonds/code/MapExample/Madison_GIS/")


library(sf)
library(tidyverse)
library(raster)
library(viridis)
library(FedData)
library(lattice)
library(rasterVis)
library(rgdal)

# Load pond sites
sites<- read_csv("sites.csv")
sites_sf <- st_as_sf(sites, coords = c("Longitude", "Latitude"), crs = 4326)
sites_sf_jit <- st_jitter(sites_sf)

# Load Lakes
lakes = st_read('../Madison_GIS/Madison_Lakes/Madison_Lakes.shp')
# Load Parks
parks = st_read('../Madison_GIS/Parks/Parks.shp') 

dane_layers <- st_layers(dsn = "Dane_Parks_2018.gdb")
dane_layers
parks_Dane<- st_read("Dane_Parks_2018.gdb", layer = "Dane_Parks_2018")
parks_unq<- parks_Dane%>%
  unique()

ggplot()+
  geom_sf(data = parks_unq$Shape, fill = "lightgreen", color = NA)
box<- c(-89.58, 43, -89.24, 43.15)
# Load Roads
box<-st_bbox(roads)
roads = st_read('../Madison_GIS/Madison_Street_Centerline_Pavement/Street_Centerlines_and_Pavement_Data.shp')
roads_dane = st_read('Dane_Roads_2016/Dane_Roads_2016.shp') 
ggplot() + geom_sf(data = roads, color = 'grey70',lwd = 0.1) +
  geom_sf(data = parks_unq, fill = "#77dd77", color = NA, alpha = 0.6)+
  geom_sf(data = roads_dane,  color = 'grey70',lwd = 0.1)+
  geom_sf(data = lakes, fill = adjustcolor('darkslategray3',0.7),lwd=0.1, color = NA) +
  geom_sf(data = sites_sf_jit, aes(fill = Depth), size = 4, shape =21, alpha = 0.8)+
  scale_fill_viridis()+
  theme_bw(base_size = 12)+
  coord_sf(xlim = box[c(1,3)],
           ylim = box[c(2,4)],
           expand = FALSE)

ggsave(filename = 'urban_pond.png',width = 6,height = 4,units = 'in')
