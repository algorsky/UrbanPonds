library(tidyverse)
library(lubridate)

# Read in data 
pond = read_csv('data/max_depth.csv') 

# Read in data 
SA = read_csv('data/pond_size.csv') 

depth<- pond%>%
  drop_na()%>%
  group_by(pond)%>%
  summarize(depth = mean(water_depth_m))


median(depth$depth)
median(SA$SA_ha)
