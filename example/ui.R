library(dplyr)
library(politantheme)
library(shiny)
library(shinythemes)
library(rhandsontable)


shinyUI(fluidPage(theme = shinytheme("journal"),
                  tags$head(includeScript("js/google-analytics.js")),
                  tags$header(list(tags$style("img {display:inline-block;background-repeat:no-repeat;position:relative;left:10px;z-index:3;}"),
                                     tags$a(href="http://www.politan.ch", tags$img(src="logo_bw.png", height="70%"), target="_blank")),
                              tags$style("header {background-color: #333333;padding-top:10px;border-bottom:1px solid #474747;height:50px}")),
                  
                  tags$script(HTML("var header = $('.navbar > .container');
                                   header.append('<div style=\"float:right\"><a href=\"https://twitter.com/share\" class=\"twitter-follow-button\" aling=\"middle\" data-url=\"www.politan.ch:3838/vorumfragen\" data-text=\"Visit www.politan.ch:3838/vorumfragen\" data-size=\"large\">Tweet</a></div>');
                                   console.log(header)")),
                  tags$script(HTML("!function(d,s,id){
                                   var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';
                                   if(!d.getElementById(id)){
                                   js=d.createElement(s);
                                   js.id=id;
                                   js.src=p+'://platform.twitter.com/widgets.js';
                                   fjs.parentNode.insertBefore(js,fjs);
                                   }
                                   }(document, 'script', 'twitter-wjs');")),
                             
                   titlePanel("  Vorumfragen bei nationalen Abstimmungen"),
                   mainPanel(
                     
                     tabsetPanel(
                      tabPanel("Aktuelle Vorlagen",
                               br(),
                               column(9, 
                                      br(),
                                       selectInput("aktuelleAbst", label=h5("Wählen Sie die gewünschte Vorlage"),
                                                   choices=c("Finanzordnung"="6160", "Nobillag"="6170"),
                                                   selected=NULL),
                                      br(),
                                      br(),
                                      HTML('<a class="twitter-share-button" 
                                           data-size="large"
                                           data-url="http://www.politan.ch/app/vorumfragen/"
                                           data-via="politan_ch"
                                           data-related="twitterapi,twitter"
                                           data-hashtags="abst16"
                                           data-text="Vorumfragen bei nationalen Abstimmungen:">
                                           Tweet
                                           </a>'
                                      ),
                                       rHandsontableOutput("aktuelleVorlagen"),
                    
                                      br(),
                                      br(),
                                       tags$p("Bei Vorumfragen stellt sich unter anderem die Frage, wie sich Personen entscheiden, die in der Umfrage angeben, noch unentschlossen zu sein. Werden diese Personen ein
                                              «ja» oder «nein» einlegen?"),
                                       br(),
                                      
                                       actionButton("goButton", "Plotten"),
                                       br(),
                                       br(),
                                       br(),
                                       plotOutput("timetrend"),
                                       br(),
                                      #https://dev.twitter.com/web/tweet-button/parameters#button
                                      br(),
                                       br(),
                                       htmlOutput("trendText"),
                                       br(),
                                       br()
                                      )
                               ),
                      tabPanel("Vergangene Vorlagen ",
                               selectInput("Jahr", label=h5("Wählen Sie das gewünschte Jahr"), 
                                           choices=c("2014"=2014, "2015"=2015, "2016"=2016), 
                                           selected=NULL),
                               tags$p("Bitte beachten Sie, dass die Tamedia-Umfrage erst seit Juni 2016 durchgeführt wird. Die Umfragewerte davor beziehen sich auf die Umfrage von 20 Minuten."),
                               br(),
                               h4("Trend-Tabelle"),
                               br(),
                               rHandsontableOutput("overview"),
                               br(),
                               br(),
                               br(),
                               h4("Unentschlossene"),
                               br(),
                               rHandsontableOutput("undecided"),
                               br(),
                               br(),
                               br()
                               ),
                      tabPanel("Gut zu wissen!",
                               br(),
                               fluidRow(
                                 column(5,
                                        h2("Fehlerbereich"),
                                        p("Üblicherweise werden bei Vorumfragen sogenannte Stichprobenfehler oder Fehlerbereiche ausgewiesen. Dieser Fehlerbereich weist darauf hin, dass es sich beim Ja-Anteil (respektive dem Nein-Anteil) lediglich um Schätzungen des wahren Ja-Anteils (respektive des wahren Nein-Anteils) handelt. Dies ist deshalb der Fall, weil es nicht möglich ist, alle stimmberechtigen Personen in der Schweiz zu befragen."),
                                        p("Die hier ausgewiesenen Fehlerbereiche enstammen aus den Berichten/Artikel zu den jeweiligen Vorumfragen. Als Faustregel gilt: Je grösser der Fehlerbereich, desto kleiner das Vertrauen in den Schätzwert."),
                                        br(),
                                        h4("Beispiel:"),
                                        p("Bei der Volksinitiative «Grüne Wirtschaft» weist die 1. Welle der Tamedia-Umfrage einen Schätzwert von 49% für den «Ja»-Anteil aus und der Fehlerbereich beträgt 1.9 Prozentpunkte."),
                                        tags$img(src = "fehler_example.png", style="display: block; margin-left: auto; margin-right: auto;", height = "100%")
                                        #
                                 ),
                                 column(5, 
                                        h2("Unentschiedene"),
                                        p("Die Vorumfragen weisen jeweils die geschätzten «Ja»-Anteile, «Nein»-Anteile sowie die «Unentschiedenen» aus. Letztere haben noch für kein Lager Partei ergriffen und stellen damit ein Wählerpotenzial dar. "),
                                        h4("Beispiel:"),
                                        p("Im Falle der 1. Welle der Tamedia-Umfrage zur Volksinitiative «Grüne Wirtschaft» werden 18% Unentschiedene ausgewiesen. Wenn sich diese Gruppe geschlossen dem «Ja»-Lager anschliessen würde, würde die Initiative angenommen. Im umgekehrten Fall würde die Initiative deutlich verworfen werden. Es ist realistischer, dass die Gruppe der «Unentschlossenen» nicht geschlossen abstimmt..."),
                                        tags$img(src = "unentschlossene.png", style="display: block; margin-left: auto; margin-right: auto;", height = "100%"),
                                        
                                        p("Die Kreuze zeigen die beiden Extreme an. Das obere, falls alle «Unentschlossenen» «Ja» stimmen würden und das untere zeigt den umgekehrten Fall an. Realistischerweise kommt der «wahre» «Ja»-Stimmenanteil irgendwo zwischen den beiden Kreuzen zu liegen."),
                                        br(),
                                        br()
                                 ),
                                 
                                 column(5, 
                                        h2("Trend"),
                                        p("Der Trend wird erst ausgewiesen, wenn die letzten Vorumfragen publiziert sind. Das geschieht typischerweise am Mittwoch in der Woche vor dem Abstimmungssonntag."),
                                        p("Der Trend wird von politan errechnet und ausgewiesen. Er ist nicht in den Berichten zu den Vorumfragen enthalten.")
                                 ),
                                 column(5, 
                                        h2("Daten"),
                                        p("Die verwendeten Daten entstammen den Trendstudien des gfsbern, welches die Umfragen im Auftrag des SRF duchführt."),
                                        p("Die Daten der Tamedia-Umfragen, respektive jene der 20 Minuten-Umfragen entstammen den jeweiligen Zeitungsartikeln.")
                                 )
                               )
                      )
                    )
                  ),
                  a("Follow @politan_ch", href="http://twitter.com/politan_ch", class="twitter-follow-button", target="_blank"),
                  #tags$a(href="http://twitter.com/politan_ch", "Follow @politan_ch", class="twitter-follow-button", target="_blank"),
                  tags$head(tags$script(src="http://platform.twitter.com/widgets.js", type="text/javascript"))
                  
                 
                  
                  
))