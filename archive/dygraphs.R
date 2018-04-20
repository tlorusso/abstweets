

vg_gsg <- readRDS("vg_gsg.rds")

data_rt <- readRDS("retweets.rds")

activity <- readRDS("activity.rds")

library(timetk)
library(dygraphs)
library(tidyverse)

lungDeaths <- cbind(mdeaths, fdeaths)
dygraph(lungDeaths)

data<-vg_gsg %>% spread(vorlage,anzahl) %>% 
  filter(!is.na(dmy)) %>% 
  #extended timeseries 
  tk_xts(date_var=dmy)

saveRDS(data,"datatest.RDS")

        
dygraph(data) %>%
  dySeries("vg", label = "Vollgeld",color = "black") %>%
  dySeries("vl", label = "Geldspielgesetz",color = "grey") %>% 
  dyOptions(drawPoints = TRUE, pointSize = 2)



  

