library(tidyverse)
library(lubridate)

# Read in data 
chloride <- read_csv('data/chloride/chloride.csv') 


ggplot(chloride)+
  geom_point(aes(x = date, y = chloride_mgL))+
  facet_wrap(~as.factor(pond))+
  theme_bw()
