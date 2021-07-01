library(tidyverse)
library(devtools)
library(lubridate)

# Read in data 
df = read_csv('data/210630_carbon.csv')  %>% 
  mutate(sampleID = str_replace_all(sampleid, pattern = "2021_01_", replacement = "2021-01-")) %>%  #because you switch from underscores to dashes
  separate(col = sampleid, into = c('filtered','pond','date'), sep = "_", remove = FALSE) %>% 
  mutate(date = as.Date(date))

ponds<- df%>%
  drop_na(date)


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 
git clone https://github.com/username/repo.git
