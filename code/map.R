library(ggmap)
library(tidyverse)
library(sf)

sites<- read_csv("data/sites.csv")

sites_sf <- st_as_sf(sites, coords = c("Longitude", "Latitude"), crs = 4326)

if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup")
register_google(key = "AIzaSyC8tGJ9a-bGzr4Y8TL9mYIkymgQUewvlG4")

qmap<- get_map(c(left = -89.55, right = -89.24, bottom = 43.0, top = 43.13), source="google", maptype='roadmap', color="bw")


map<- ggmap(qmap) +
  geom_point(data = sites, mapping = aes(x = Longitude, y = Latitude, color = as.factor(Pair)), shape = 20, size=5)+
  theme_void()+theme(legend.position = "none")

ggsave("figures/AllSiteMap.png", width = 8, height = 6, units = 'in', map)
