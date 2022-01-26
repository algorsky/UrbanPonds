library(ggmap)
sites<- read_csv("data/sites.csv")

sites_sf <- st_as_sf(sites, coords = c("Longitude", "Latitude"), crs = 4326)

if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup")
register_google(key = "AIzaSyC8tGJ9a-bGzr4Y8TL9mYIkymgQUewvlG4")

qmap<- get_map(location = c(lon = -89.430, lat = 43.04), zoom=15, source="google", maptype='satellite', color="bw")


map<- ggmap(qmap) +
  geom_point(data = sites, mapping = aes(x = Longitude, y = Latitude, color = as.factor(Pond)), size=4)+
  theme(legend.title = element_blank())

ggsave("figures/HabitatMap.png", width = 8, height = 6, units = 'in', map)
