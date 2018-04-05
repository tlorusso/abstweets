rm(list=ls(all=T))
options(stringsAsFactors = F)
library(dplyr)
library(politantheme)
library(shiny)
library(shinythemes)
library(rhandsontable)
source("plothelper.R")

# Define server logic 
shinyServer(function(session, input, output) {
  
  values <- reactiveValues()
  
  
  #Read in all data
  all_data <- readRDS("data/all_data.Rds") %>%
    mutate(acceptpo = as.numeric(acceptpo)) %>%
    mutate(year = format(datevote, "%Y"))
  
  ################################################
  ######## Table vergangene Umfragen
  
  subdata <- reactive({
    neu <- all_data %>%
      filter(year==input$Jahr) %>%
      filter(aktuell==FALSE)
    return(neu)
  })
  
  
  #neu <- all_data %>%
  #  filter(year==2016) %>%
  #  filter(aktuell==FALSE)
  
  
  output$overview <- renderRHandsontable({
    ola <- subdata()
    #ola <- neu
    ola$chart_tam <- sapply(1:nrow(ola),
                                 function(x) jsonlite::toJSON(list(values=c(as.numeric(ola$welle_1_ja[x]),
                                                                            as.numeric(ola$welle_2_ja[x]),
                                                                            as.numeric(ola$welle_3_ja[x]),
                                                                            as.numeric(ola$acceptpo[x])),
                                                                   options = list(type = "line"))))
    
    ola$chart_srf <- sapply(1:nrow(ola),
                                 function(x) jsonlite::toJSON(list(values=c(as.numeric(ola$srf1ja[x]),
                                                                            as.numeric(ola$srf2ja[x]),
                                                                            as.numeric(ola$acceptpo[x])),
                                                                   options = list(type = "line"))))
    
    
    
    
    
    ola$acceptpo <- paste0('<span style="width:100%;height:100%;background:#d2f2d7;display:inline-block;color:black;text-align:center;">', ola$acceptpo, '</span>')
    #ola$srf1ja <- paste0('<span style="display:inline-block;color:black;text-align:center;">', ola$srf1ja, '</span>')

    
    ola %>%
      select(srf1ja, srf2ja, chart_srf, acceptpo, welle_1_ja, welle_2_ja, welle_3_ja, chart_tam, shorttitle)%>%
      setNames(., c('Welle 1 gfs', 'Welle 2 gfs', 'Trend gfs', "Resultat", "Welle 1 TA", "Welle 2 TA", "Welle 3 TA", "Trend TA", "Vorlage"))%>%
      rhandsontable(readOnly = TRUE, width = 700,
                    allowedTags = "<em><b><span><strong><a><big>") %>%
      hot_cols(colWidths = c(60, 50, 50, 70, 50, 50, 50, 50, 200, 70)) %>%
      hot_col("Welle 1 gfs", format = "0")%>%
      hot_col("Welle 2 gfs", format = "0") %>%
      hot_col("Welle 1 TA", format = "0") %>%
      hot_col("Welle 2 TA", format = "0") %>%
      hot_col("Welle 3 TA", format = "0") %>%
      hot_col("Resultat", color="green")%>%
      hot_col("Trend TA", renderer = htmlwidgets::JS("renderSparkline")) %>%
      hot_col("Trend gfs", renderer = htmlwidgets::JS("renderSparkline")) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
      hot_col("Resultat", renderer = htmlwidgets::JS("safeHtmlRenderer"))
    
    
  })
  
  ################################################
  ######## Table Undecided
  
  undec <- reactive({
    unent <- all_data %>%
      filter(year==input$Jahr) %>%
      #filter(year==2014)%>%
      filter(aktuell==FALSE) %>%
      select(shorttitle, welle_1_undec, welle_2_undec, welle_3_undec, srf1undec, srf2undec)
    return(unent)
  })
  
  output$undecided <- renderRHandsontable({
    unwiss <- undec()
    
    unwiss$ta_undec_trend <- sapply(1:nrow(unwiss),
                            function(x) jsonlite::toJSON(list(values=c(as.numeric(unwiss$welle_1_undec[x]),
                                                                       as.numeric(unwiss$welle_2_undec[x]),
                                                                       as.numeric(unwiss$welle_3_undec[x]),
                                                                       as.numeric(0)),
                                                              options = list(type = "bar"))))
    
    unwiss$srf_undec_trend <- sapply(1:nrow(unwiss),
                            function(x) jsonlite::toJSON(list(values=c(as.numeric(unwiss$srf1undec[x]),
                                                                       as.numeric(unwiss$srf2undec[x]),
                                                                       as.numeric(0)),
                                                              options = list(type = "bar"))))
    unwiss$x <- NA
    unwiss %>%
      select(x, srf1undec, srf2undec, srf_undec_trend, shorttitle, welle_1_undec, welle_2_undec, welle_3_undec, ta_undec_trend)%>%
      setNames(., c('x', 'Welle 1 gfs', 'Welle 2 gfs', 'Trend gfs', 'Vorlage', "Welle 1 TA", "Welle 2 TA", "Welle 3 TA", "Trend TA"))%>%
      rhandsontable(readOnly = TRUE, width = 800, 
                    allowedTags = "<em><b><span><strong><a><big>") %>%
      hot_cols(colWidths = c(1, 50, 50, 70,200, 50, 50, 50, 70)) %>%
      hot_col("Welle 1 gfs", format = "0")%>%
      hot_col("Welle 2 gfs", format = "0") %>%
      hot_col("Welle 1 TA", format = "0") %>%
      hot_col("Welle 2 TA", format = "0") %>%
      hot_col("Welle 3 TA", format = "0") %>%
      hot_col("Trend TA", renderer = htmlwidgets::JS("renderSparkline")) %>%
      hot_col("Trend gfs", renderer = htmlwidgets::JS("renderSparkline")) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE) 
    
    
  })
  
  
  
  
  
  ########################################################
  #aktuelle vorlagen
  #table
  
  output$aktuelleVorlagen <- renderRHandsontable({
    
    bfsnrr <- as.numeric(input$aktuelleAbst)
    #bfsnrr <- 6051
    aha <- readRDS("data/aktuell.Rds") %>%
      filter(bfsnr==bfsnrr)
    
    aha %>%
      select(ja, undec, house, welle, fehlerbereich) %>%
      rename(Ja = ja)%>%
      rename(Fehlerbereich = fehlerbereich)%>%
      rename(Unentschiedene = undec)%>%
      rename(Institut = house)%>%
      rename(Welle = welle)%>%
      rhandsontable(readOnly = TRUE, width = 1000, height = 200,
                  allowedTags = "<em><b><span><strong><a><big>") %>%
      hot_cols(colWidths = c(50, 120, 70, 50, 120)) %>%
      hot_col("Welle", format = "0") %>%
      hot_col("Unentschiedene", format = "0") %>%
      hot_col("Ja", format = "0")
    
    
  })
  
  
  ########################################################################
  ################## Plot
  plotter <- eventReactive(input$goButton,{
    
    values[["bfsnrr"]] <- as.numeric(input$aktuelleAbst)
    
    titel <- readRDS("data/vinames.Rds") %>%
      filter(bfsnr==values[["bfsnrr"]]) %>%
      select(shorttitle) %>%
      as.character
    
    mapper(values[["bfsnrr"]], titel)
  
    })
  
  
  output$timetrend <- renderPlot({
    
    grid.draw(plotter())
    
  })
  

  


  #Text
  trend_text <- eventReactive(input$goButton,{
    
    
    aktuell <- readRDS("data/aktuell.Rds") %>%
      filter(bfsnr==as.numeric(input$aktuelleAbst))
    
    texxt <- HTML("<br></br><p>Die Kreuze zeigen die beiden Extreme an. Das obere, falls alle «Unentschlossenen» «Ja» stimmen würden und das untere zeigt den umgekehrten Fall an. Realistischerweise käme der «wahre» «Ja»-Stimmenanteil irgendwo zwischen den beiden Kreuzen zu liegen.</p>")
    
    
    if(!is.na(aktuell$ja[aktuell$welle==3&aktuell$house=="TA"])){
      
      texxt <- HTML("<p>Die beiden Kreuze oberhalb und unterhalb des Punktes zeigen die jeweiligen Extreme an. Das obere, falls 
                    alle «Unentschlossenen» «Ja» stimmen würden und das untere zeigt den 
                    Ja-Stimmenanteil, falls alle «Unentschlossenen» «Nein» stimmen würden. Realistischerweise kommt der «wahre» «Ja»-Stimmenanteil 
                    irgendwo zwischen den beiden Kreuzen zu liegen.</p><br></br>
                    <p>Die Punkte am Ende der gestrichelten Linie stellen Projektionen dar. Diese entstehen, wenn die jeweils letzten beiden Ja-Anteile der Vorumfragen verbunden werden.
                    Dieser Punkt würde dem Ja-Anteil entsprechen, wenn der aktuelle Trend gleich weiterverlaufen würde.</p>")
    }
    
    if(!is.na(aktuell$ja[aktuell$welle==2&aktuell$house=="TA"])&is.na(aktuell$ja[aktuell$welle==3&aktuell$house=="TA"])){
      
      texxt <- HTML("<p>Die Kreuze zeigen die beiden Extreme an. Das obere, falls alle «Unentschlossenen» «Ja» stimmen würden und das untere zeigt den 
                    Ja-Stimmenanteil, falls alle «Unentschlossenen» ein «Nein» einlegen würden. Realistischerweise käme der «wahre» Ja-Stimmenanteil 
                    irgendwo zwischen den beiden Kreuzen zu liegen.</p><br></br>")
    }
    
    
    
    
    
    return(texxt)

  })
  
  output$trendText <- renderUI({
    trend_text()
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
})


