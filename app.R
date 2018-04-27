#
# Politan.ch
# 
#
# Twitter - Abstimmungskampfmonitor
#
# Shiny-App   
#


library(pacman)

pacman::p_load(tidyverse,echarts4r,shinythemes,DT,shiny)


#Daten für Abstimmungen von 8.Juni
vg_gsg <- readRDS("vg_gsg.rds") 
#GSG
rt_gsg <- readRDS("retweets_gsg.rds")

activity_gsg <- readRDS("activity_gsg.rds")
#VGI
rt_vg <- readRDS("retweets_vg.rds")

activity_vg <- readRDS("activity_vg.rds")

#Daten für Ref. zu Überwachung von Versicherten
rt_sd <- readRDS("retweets_sd.rds")

activity_sd <- readRDS("activity_sd.rds")

tweets_sd<- readRDS("tweets_sd.rds")

# Define UI for application 
ui <- fluidPage(#theme = shinytheme("journal"),
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
                titlePanel("  Twitter-Monitor "),
                mainPanel(
                tabsetPanel(
                         tabPanel("Plot", sidebarLayout(
                          sidebarPanel(
                            h4("Am Puls des Abstimmungskampfes auf Twitter"),
                           #   selectizeInput('e7', label= 'Choose District',choices = districts, 
                           #                  multiple = TRUE, selected = "Total Canton of Zurich", options= list(maxItems = 13)),
                           #   hr(),
                           includeMarkdown("intro.Rmd")),
                           mainPanel(h4("Tweets"),
                                     echarts4rOutput("tweetplot")))),
                  #Tab - Panel zu Geldspielgesetz       
                           tabPanel("Vorlagen vom 8.Juni 2018", titlePanel("Retweets & Aktivität"),
                                    selectInput("df", "Vorlage wählen", choices = c('Geldspielgesetz'='activity_gsg','Vollgeld'='activity_vg'), selected = 'Geldspielgesetz'),
                                    fluidRow(
                                      h4("Tweets"),
                                      DT::dataTableOutput("actgsg")),
                                    h4("Retweets pro User"),
                                    fluidRow(
                                      DT::dataTableOutput("rtgsg"))
                           ),
                  
                  tabPanel("#Versicherungsspione", titlePanel("Retweets & Aktivität"),
                           fluidRow(
                             echarts4rOutput("tweetplot_sd"),
                             h4("Tweets"),
                             DT::dataTableOutput("actsd")),
                           h4("Retweets pro User"),
                           fluidRow(
                             DT::dataTableOutput("rtsd"))
                          ),
                  
                  #Tab-panel datendownload etc.
                           tabPanel(p(icon("Info"), "Infos & Datendownload"),
                                    h4("Am Puls des Abstimmungskampfes auf Twitter"),
                                     includeMarkdown("about.Rmd")
                                    )
                ),a("Follow @politan_ch", href="http://twitter.com/politan_ch", class="twitter-follow-button", target="_blank"),
                tags$head(tags$script(src="http://platform.twitter.com/widgets.js", type="text/javascript"))
                )
)

# Define server logic required to draw plot
server <- function(input, output) {
  
output$tweetplot <-  renderEcharts4r({

vg_gsg %>% 
  e_charts(dmy) %>% 
  e_line(vg,name="Vollgeld") %>% 
  e_line(gsg,name="Geldspielgesetz") %>% 
  e_line(nb,name="#NoBillag") %>% 
  e_tooltip(trigger = "axis") %>% 
  e_toolbox(right="3%") %>% 
  e_toolbox_feature(feature = "saveAsImage",title="Bild speichern") %>% 
  e_toolbox_feature(feature = "dataView",title="Daten", lang="") %>% 
  e_title("", "politan.ch", sublink = "http://politan.ch/") %>% 
  e_legend(orient=c("vertical"),top="-0%",padding="20")
  
  })


output$tweetplot_sd <-  renderEcharts4r({
  
  tweets_sd %>% 
    e_charts(dmy) %>% 
    e_line(vg,name="Referendum ASTG") %>% 
    e_tooltip(trigger = "axis") %>% 
    e_toolbox(right="3%") %>% 
    e_toolbox_feature(feature = "saveAsImage",title="Bild speichern") %>% 
    e_toolbox_feature(feature = "dataView",title="Daten", lang="") %>% 
    e_title("", "politan.ch", sublink = "http://politan.ch/")
  
})


#tabellen für gsg

df <- reactive({
  x <- get(input$df)
})
  
  output$actgsg = renderDT(
    df() %>% 
      mutate(screen_name=paste0("<a href='twitter.com/",screen_name,"' target='_blank'>",
                         screen_name,"</a>")) %>% 
      arrange(desc(n)),
    options = list(lengthChange = FALSE),
    escape = FALSE)


  output$rtgsg = renderDT(
    rt_gsg %>% 
      mutate(rt_user=paste0("<a href='twitter.com/",rt_user,"' target='_blank'>",
                                rt_user,"</a>")) %>% 
      arrange(desc(n)),
    options = list(lengthChange = FALSE),
   escape = FALSE)
  
  #tabellen für vgi
  
  output$actvg = renderDT(
    activity_vg %>% 
      mutate(screen_name=paste0("<a href='twitter.com/",screen_name,"' target='_blank'>",
                                screen_name,"</a>")) %>% 
      arrange(desc(n)),
    options = list(lengthChange = FALSE),
    escape = FALSE)
  
  
  output$rtvg = renderDT(
    rt_vg %>% 
      mutate(rt_user=paste0("<a href='twitter.com/",rt_user,"' target='_blank'>",
                            rt_user,"</a>")) %>% 
      arrange(desc(n)),
    options = list(lengthChange = FALSE),
    escape = FALSE)
  

  #tabellen für ATSG ("Überwachung Versicherte")
  
  output$actsd= renderDT(
    activity_sd %>% 
      mutate(screen_name=paste0("<a href='twitter.com/",screen_name,"' target='_blank'>",
                                screen_name,"</a>")) %>% 
      arrange(desc(n)),
    options = list(lengthChange = FALSE),
    escape = FALSE)
  
  
  output$rtsd = renderDT(
    rt_sd %>% 
      mutate(rt_user=paste0("<a href='twitter.com/",rt_user,"' target='_blank'>",
                            rt_user,"</a>")) %>% 
      arrange(desc(n)),
    options = list(lengthChange = FALSE),
    escape = FALSE)
  
    
}

# Run the application 
shinyApp(ui = ui, server = server)

