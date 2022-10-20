library(ggmap)
library(tidyverse)
library(sf)

sites<- read_csv("data/ancillary/sites.csv")


sites_sf <- st_as_sf(sites, coords = c("Longitude", "Latitude"), crs = 4326)
#myLocation<- c(-89.55, 43, -89.24, 43.13)

if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup")
register_google(key = "AIzaSyC8tGJ9a-bGzr4Y8TL9mYIkymgQUewvlG4")

myMap<- get_map(location = myLocation, source="google", maptype="hybrid")


myLocation<- c(-89.5, 43, -89, 43.5)
myMap <- get_map("madison",maptype="hybrid", zoom = 11)
ggmap(myMap)

ggma

ggmap(myMap) +
  geom_point(data = sites, mapping = aes(x = Longitude, y = Latitude),shape = 20, size=5, alpha = 0.8)+

  theme_void()+theme(legend.position = "none")
ggsave("figures/satellite_sitemap.png", width = 8, height = 6, units = 'in')

ggsave("figures/AllSiteMap.png", width = 8, height = 6, units = 'in', map)
