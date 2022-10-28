setwd("/Users/adriannagorsky/Documents/UrbanPonds/code/MapExample/Madison_GIS/")

library(sf)
library(tidyverse)
library(raster)
library(viridis)
library(FedData)
library(lattice)
library(rasterVis)
library(rgdal)


NLCD<- raster('NLCD/NLCD_2019.tiff')
NLCD_df <- as.data.frame(NLCD, xy = TRUE)


legend<- pal_nlcd()
legend
vals<- unique(r)
df<- legend[legend$ID %in% vals,]

NLCD_df_att<- NLCD_df%>%
  rename(ID =NLCD_2019)%>%
  merge(df, by = "ID")

  
ggplot() +
  geom_raster(data = NLCD_df_att , aes(x = x, y = y, fill = as.factor(Color)))+
  scale_fill_manual(name = "Land Cover",
                    values = NLCD_df_att$Color,
                    labels = NLCD_df_att$Class)+
  coord_quickmap()

GDALinfo('NLCD/NLCD_2019.tiff')
r<- raster('NLCD/NLCD_2019.tiff')
plot(r)
legend<- pal_nlcd()
legend
vals<- unique(r)
df<- legend[legend$ID %in% vals,]

values<- extract(rat, sites_sf)

lc<- merger(df, values, by = "ID")

sites_sf<- sites_sf%>%
  mutate(ID = values)%>%
  merge(df)

plot(rat)

rat<- ratify(r)

myKey <- list(rectangles=list(col = df$Color),
              text=list(lab=df$Class),
              space='left',
              columns=1,
              size=2,
              cex=.6)

levelplot(rat, att = 'ID')
levelplot(rat, att='ID', 
          col.regions=df$Color,
          par.settings = list(axis.line = list(col = "transparent"), 
                              strip.background = list(col = 'transparent'), 
                              strip.border = list(col = 'transparent')), 
          scales = list(col = "transparent"),
          colorkey=F,
          key=myKey)

ggplot()+
  geom_raster(data = r, aes(y = Latitude, x = Longitude))+
  geom_raster()