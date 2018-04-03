#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(pacman)
library(DT)
library(tidyverse)
library(politantheme)
library(shinythemes)

vg_gsg <- readRDS("vg_gsg.rds")

data_rt <- readRDS("retweets.rds")

activity <- readRDS("activity.rds")

# Define UI for application 
ui <- navbarPage("AbsTweets",
                theme = shinytheme("united"),
                 tabPanel("Plot", sidebarLayout(
                  sidebarPanel(
                     h4("Tweets"),
                   #   selectizeInput('e7', label= 'Choose District',choices = districts, 
                   #                  multiple = TRUE, selected = "Total Canton of Zurich", options= list(maxItems = 13)),
                   #   hr(),
                   includeMarkdown("about.Rmd")),
                   mainPanel(h4("Tweets"),
                             plotOutput("tweetplot")))),
                 tabPanel("User", titlePanel("Retweets & Aktivität"),
                          # Create a new Row in the UI for selectInputs
                          # fluidRow(
                          #   column(4,
                          #          selectInput("districtinput4",
                          #                      "District:",
                          #                      c("All",
                          #                        unique(as.character(swr_data$GEBIET_NAME))))),
                          #   column(4,
                          #          selectInput("yearinput",
                          #                      "Year:",
                          #                      c("All",
                          #                        unique(as.character(swr_data$INDIKATOR_JAHR)))))),
                          # Create a new row for the table.
                          fluidRow(
                            h4("Tweets"),
                            DT::dataTableOutput("act")),
                          h4("Retweets pro User"),
                          fluidRow(
                            DT::dataTableOutput("rt"))
                 ),
                 tabPanel(p(icon("Info"), "Infos & Datendownload")
                          # ,includeMarkdown("about.Rmd")
                          )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  # print(str(swr_data))
  # plot
  
  output$tweetplot <- renderPlot({
    
   vg_gsg %>% 
      ggplot(aes(dmy,anzahl,color=vorlage,group=vorlage))+
      geom_line()+
      politantheme::theme_politan()+
      labs(caption="politan.ch")
    
  })
  
  
  output$act = renderDT(
    activity %>% 
      mutate(screen_name=paste0("<a href='twitter.com/",screen_name,"' target='_blank'>",
                         screen_name,"</a>")) %>% 
      arrange(desc(n)),
    options = list(lengthChange = FALSE),
    escape = FALSE)


  output$rt = renderDT(
    data_rt %>% 
      mutate(rt_user=paste0("<a href='twitter.com/",rt_user,"' target='_blank'>",
                                rt_user,"</a>")) %>% 
      arrange(desc(n)),
    options = list(lengthChange = FALSE),
   escape = FALSE)
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

