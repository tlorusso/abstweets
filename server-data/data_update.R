source("/home/thomaswilli/APPs/abstweets/helper.R")

#load packages
pacman::p_load(googlesheets,stringr,purrr)

pacman::p_load("cld2")
pacman::p_load("dplyr")

#für datums-parsing
Sys.setenv(TZ='GMT')
Sys.setlocale("LC_ALL","English")

#load authentication token for googlesheets
gs_auth(token = "/home/thomaswilli/APPs/abstweets/googlesheets_token.rds")


############################################################
#sheets which contains gsg_mu in title
vgi <-gs_ls("vgi_mehrere")

#save google object ref
vgl <-map(vgi$sheet_title,gs_title)

datenaufbereiter(vgl, "vg")


############################################################
gsg <-gs_ls("gsg_multi")

#save google object ref
gsgl <-map(gsg$sheet_title,gs_title)
datenaufbereiter(gsgl, "gsg", lang.det=T)

############################################################
#sheets which contains gsg_mu in title
sd <-gs_ls("Sozialdetektive")

#save google object ref
sd <-map(sd$sheet_title,gs_title)
datenaufbereiter(sd, "sd", lang.det=T)



filenames <- list.files(saveort)

plots <- filenames[grep("plotdf", filenames)]

herewego <- paste0(saveort, plots)

nobi <-readRDS("/home/thomaswilli/APPs/abstweets/data/nobillagbaseline.RDS")

container <- readRDS(herewego[1])
for(i in 2:length(herewego)){
  
  container <- readRDS(herewego[i]) %>%
    left_join(container, by="dmy")
  
  
}
container <-container%>% 
  mutate(countdown=as.Date("2018-06-10")-dmy)%>% 
  left_join(nobi) %>% 
  filter(dmy>"2018-04-19")

weg <- which.max(container$dmy)
container <- container[-weg,]


saveRDS(container,"/home/thomaswilli/APPs/abstweets/data/vg_gsg.rds")




cat("Fetched Data from Googlesheets\n\n")