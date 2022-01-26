library(ggspatial)
library(sf)
library(ggrepel)
library(tidyverse)
library(maps)
library(mapdata)
library(patchwork)

#Linnea Groundwater Wells
gw.sparklingwells <- data.frame(site = c("Well 5", "Well 7a/b", "Well 8"), 
                                lat = c(46.0030612, 46.0036897, 46.0065147), 
                                lon = c(-89.6967096, -89.6958901, -89.6962271)) %>% 
  st_as_sf(coords = c('lon','lat'),crs = 4326)


# esri_land <-    paste0('https://services.arcgisonline.com/arcgis/rest/services/NatGeo_World_Map/MapServer/tile/${z}/${y}/${x}.jpeg')
# esri_streets <- paste0('https://services.arcgisonline.com/arcgis/rest/services/World_Street_Map/MapServer/tile/${z}/${y}/${x}.jpeg')
# world_gray <-   paste0('https://services.arcgisonline.com/arcgis/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/${z}/${y}/${x}.jpeg')
# basemap <- paste0('https://tiles.wmflabs.org/osm-no-labels/${z}/${x}/${y}.png')

## Read in road layers 
trunks = st_read('OpenStreets_WI/wiroads_TrunkPrimaryMotorway.shp') %>% 
  st_crop(xmin = -89.8, xmax = -89.4, ymin = 45.8, ymax = 46.1)
# roads = st_read('Map/OpenStreets_WI/WI_openstreets/gis_osm_roads_free_1.shp') %>% 
#   st_crop(xmin = -89.8, xmax = -89.4, ymin = 45.8, ymax = 46.1)
# st_write(obj = roads, 'Map/OpenStreets_WI/wiroads_sparkling.shp')
roads = st_read('OpenStreets_WI/wiroads_sparkling.shp')

# lakes
lakes = st_read('WaterBodies/lakes.shp')
lakes.sp = lakes %>% filter(WATERBODY_ %in% c('Sparkling Lake','Big Muskellunge Lake', 'Trout Lake', 'Crystal Lake'))
lakes.sp2 = lakes %>% filter(WATERBODY_ %in% c('Trout Lake'))

# map.wells = 
s1 = ggplot(gw.sparklingwells) +
  # annotation_map_tile(type = basemap, zoom = 14) +  
  # annotation_map_tile(type = esri_streets, zoom = 14) +  
  geom_sf(data = trunks) +
  geom_sf(data = roads, alpha = 0.6, size = 0.1) +
  geom_sf(data = lakes, fill = alpha('lightsteelblue1',1), size = 0.2) +
  geom_sf_label(data = lakes.sp, aes(label = WATERBODY_), label.size = 0.1, alpha = 0.7, size = 2) +
  geom_sf_label(data = lakes.sp2, aes(label = WATERBODY_), label.size = 0.1, alpha = 0.7, size = 2, nudge_y = -0.02, nudge_x = -0.01) + # Trout Lake label
  geom_sf(data = gw.sparklingwells, size = 0.8, fill = 'red3', shape =22, stroke = 0.2, #Sparkling groundwater wells
          show.legend = "point", inherit.aes = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.2, height = unit(0.05,'in'), text_cex = 0.6) + # Scale bar
  annotation_north_arrow(location = "tr", which_north = "true", 
                         # pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         height = unit(0.3,'in'), width = unit(0.5,'in'),
                         style = north_arrow_minimal) + # North Arrow
  theme_bw(base_size = 9) +
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_sf(xlim = c(-89.71, -89.58), ylim = c(45.99, 46.04)) +
  NULL


## Inset Map of Wisconsin
# All code used to simply and output the shapefiles below (won't work without necessary input files)
# wi = st_read('Wisconsin/Wisconsin_State_Boundary_24K-shp/Wisconsin_State_Boundary_24K.shp')
# greatLakes = st_read('GLC_shapefiles/GreatLakes/main_lakes.shp')
# states = st_read('Wisconsin/cb_2018_us_state_20m/cb_2018_us_state_20m.shp') %>% 
#   filter(NAME %in% c('Wisconsin','Minnesota','Michigan','Illinois','Iowa'))
# NHD = st_read('/Users/hilarydugan/Dropbox/Documents/NHDPlus/NHDWaterbody_Med_WI_102003_lakepond.shp')
# 
# wi.simple <- st_simplify(wi, preserveTopology = TRUE, dTolerance = 2000) %>% 
#   st_transform(crs = 4326)
# 
# NHD.simple = #st_simplify(NHD, preserveTopology = TRUE, dTolerance = 2000) %>% 
#   NHD %>% 
#   st_transform(crs = 4326) %>% 
#   filter(AREASQKM > 1)

states = st_read('InsetMap/WI_borderstates.shp')
NHD.simple = st_read('InsetMap/NHD_simple.shp')
wi.simple = st_read('InsetMap/Wisconsin_State_Boundary_simple.shp')
greatLakes = st_read('InsetMap/greatLakes.shp')

w1 = ggplot(wi.simple) +
  geom_sf(data = states, col = 'grey50', fill = 'grey90', alpha = 0.5, size = 0.2) +
  geom_sf(data = NHD.simple, col = NA, fill = 'lightsteelblue2') +
  geom_sf(data = greatLakes, fill = 'lightsteelblue2', col = 'lightsteelblue2') +
  geom_sf(data = st_as_sfc(st_bbox(lakes.sp)), fill = 'red4', color = 'red4', size = 0.3) + # Inset box
  # geom_sf(data = wi.simple) +
  coord_sf(ylim = c(42.3,47.5), xlim = c(-93, -86), expand = FALSE) +# limit axes
  theme_bw(base_size = 7) +
  theme(#plot.background = element_rect(fill = "transparent", colour = NA),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(),
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.background = element_rect(fill = 'white', colour = 'black ', size = 0.3))

# Combine maps
s1 + inset_element(w1, left = 0.6, bottom = 0, right = 1.18, top =  0.4)

ggsave('Maptest.pdf', width = 6.5, height = 5, dpi = 500, bg = "transparent")



