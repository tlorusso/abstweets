setwd("~/abstweets")

vg_gsg <- readRDS("vg_gsg.rds") %>% tidyr::spread(vorlage,anzahl)

data_rt <- readRDS("retweets.rds")

activity <- readRDS("activity.rds")


sesh %>% 
  echart(Country) %>% 
  emap() %>% 
  emap_choropleth(Sessions) %>% 
  ecolorbar(min = 1, max = 270, calculable = TRUE, text = list("high", "low")) %>% 
  etitle("Sessions origins in 2016", "john-coene.com", sublink = "http://john-coene.com/") %>% 
  etheme("roma")

# install.packages("devtools")
devtools::install_github("JohnCoene/echarts4r")

library(echarts4r)

mtcars %>% 
  e_charts(wt) %>% # initialise and set x
  e_line(mpg) # add a line

vg_gsg %>% 
  e_charts(dmy) %>% 
  e_line(vg,name="Vollgeld") %>% 
  e_line(vl,name="Geldspielgesetz") %>% 
  e_tooltip(trigger = "axis")