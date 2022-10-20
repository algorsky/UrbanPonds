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

# Load Lakes
lakes = st_read('../Madison_GIS/Madison_Lakes/Madison_Lakes.shp') %>%
  st_transform(st_crs(rds.sf))
# Load Parks
parks = st_read('../Madison_GIS/Parks/Parks.shp') %>%
  st_transform(st_crs(rds.sf))
# Load Roads
box<-st_bbox(roads)
roads = st_read('../Madison_GIS/Madison_Street_Centerline_Pavement/Street_Centerlines_and_Pavement_Data.shp') %>%
  st_transform(st_crs(rds.sf))
roads_dane = st_read('Dane_Roads_2016/Dane_Roads_2016.shp') 


dane<-ggplot() + geom_sf(data = roads, color = 'grey70',lwd = 0.1) +
  geom_sf(data = roads_dane,  color = 'grey70',lwd = 0.1)+
  geom_sf(data = sites_sf, color = values, size = 2, fill = 'red3', shape =22, stroke = 0.2,  
          show.legend = "point", inherit.aes = FALSE)+
  geom_sf(data = parks, fill = adjustcolor('darkgreen',0.3),lwd=0.1) +
  geom_sf(data = lakes, fill = adjustcolor('darkslategray3',0.7),lwd=0.1) +
  theme_bw()
                     
dane+
  coord_sf(xlim = box[c(1,3)],
           ylim = box[c(2,4)],
           expand = FALSE)
ggsave(filename = 'saltsoils_map.png',width = 6,height = 4,units = 'in')
