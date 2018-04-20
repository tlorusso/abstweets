setwd("~/abstweets")
library(tidyverse)
library(lubridate)

vg_gsg <- readRDS("vg_gsg.rds")
# %>% tidyr::spread(vorlage,anzahl)

data_rt <- readRDS("retweets.rds")

activity <- readRDS("activity.rds")

vg_gsg <-vg_gsg %>% mutate(countdown=as.Date("2018-06-10")-dmy) %>% filter(!is.na(dmy))

# sesh %>% 
#   echart(Country) %>% 
#   emap() %>% 
#   emap_choropleth(Sessions) %>% 
#   ecolorbar(min = 1, max = 270, calculable = TRUE, text = list("high", "low")) %>% 
#   etitle("Sessions origins in 2016", "john-coene.com", sublink = "http://john-coene.com/") %>% 
#   etheme("roma")

# install.packages("devtools")
devtools::install_github("JohnCoene/echarts4r")

library(echarts4r)

mtcars %>% 
  e_charts(wt) %>% # initialise and set x
  e_line(mpg) # add a line

#mit echarts version 4

vg_gsg %>% 
  e_charts(dmy) %>% 
  e_line(vg,name="Vollgeld") %>% 
  e_line(gsg,name="Geldspielgesetz") %>% 
  e_tooltip(trigger = "axis") %>% 
  e_toolbox(feature = "saveAsImage",title="Bild speichern", textPosition='top') %>% 
  e_toolbox(feature = "dataView",title="Data", lang="")

USArrests %>% 
  e_charts(UrbanPop) %>% 
  e_line(Assault) %>% 
  e_area(Murder, y.index = 1, x.index = 1) %>% 
  e_toolbox(feature = "saveAsImage", title = "Save As Image",right='20%') %>% 
  e_toolbox(feature = "dataView",title="Data",lang="")
  
  

#mit version 2

devtools::install_github("JohnCoene/echarts")

library(echarts)

vg_gsg %>% 
  echart(dmy) %>% 
  eline(vg,name="Vollgeld") %>% 
  eline(vl,name="Geldspielgesetz") %>% 
  etooltip(trigger = "axis") %>% 
  etheme("infographic") %>% 
  etoolbox_full()
  

