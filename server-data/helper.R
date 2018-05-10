

datenaufbereiter <- function(vgl, obname=NULL, lang.det=F){
  
  corrupt_ht<-c("#gsg")
  
  #pfadname um daten abzuspeichern
  saveort <- paste0("/home/thomaswilli/APPs/abstweets/data/")
  
  #save all the identified objects in dataframe
  tweets <- 1:length(vgl) %>% 
    map_dfr(~vgl[[.]] %>% 
              gs_read(col_names = FALSE))
  
  colnames(tweets)<- c("screen_name","text","link","date")
  
  tweets$date<- as.POSIXct(strptime(tweets$date,"%B %d, %Y at %I:%M%p"))
  
  tweets<-tweets%>% 
    mutate(year= format(date, "%Y"),
           week= format(date, "%W"),
           dmy=as.Date(date),
           #extrahiere User-handle welcher geretweetet wurde
           rt_user=str_extract(text, "RT @(.*?):"),
           #extrahiere hash-tags!
           hashtags=tolower(str_extract_all(text, "#(.*?) |#(.*)")))
  
  message("Tweets DF successfull\n")
  
  if(lang.det==T){
    tweets <- tweets %>% 
      mutate(
        cld2 = cld2::detect_language(text = text, plain_text = FALSE)) %>%  
      #cld3 = cld3::detect_language(text = text)
      filter( 
        #all the tweets without the hashtags that is used in other context
        !(corrupt_ht %in%  hashtags)|
          #all the tweets with the corrupt hashtags which are classified in a swiss language
          corrupt_ht %in%  hashtags  & cld2 %in% c("de","fr","it") 
      )
  }
  
  #Aktivste User
  useractivity_vg<- tweets %>% 
    group_by(screen_name) %>% 
    summarize(n=n())
  
  #Meiste Retweets
  userretweets_vg<-tweets %>% 
    mutate(rt_user=str_replace_all(rt_user,"RT |:","")) %>% 
    group_by(rt_user) %>% 
    summarize(n=n()) %>% 
    filter(!is.na(rt_user))
  
  #files for shiny app tables
  saveRDS(useractivity_vg, paste0(saveort, "activity_", obname, ".rds"))
  
  saveRDS(userretweets_vg, paste0(saveort, "retweets_", obname, ".rds"))
  

  #file for rawdata download (single tweets)
  write.csv(tweets, paste0(saveort, "rawdata_", obname, ".csv"))
  
  
  
  #combine with baseline
  tweets_n_compare <-tweets%>% 
    filter(is.na(rt_user))%>%
    group_by(dmy) %>% 
    summarize(a=n())
  
  names(tweets_n_compare)[names(tweets_n_compare)=="a"] <- obname
  saveRDS(tweets_n_compare, paste0(saveort, "plotdf_", obname, ".rds"))

  tweets_n_sd<-tweets %>% 
    filter(is.na(rt_user))%>%
    group_by(dmy) %>% 
    summarize(vg=n())
    
  saveRDS(tweets_n_sd,"/home/thomaswilli/APPs/abstweets/data/tweets_sd.rds")
  
  
  
  
}