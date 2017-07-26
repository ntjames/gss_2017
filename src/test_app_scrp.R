#set path depending on user
user<-system2("whoami", stdout=TRUE)
wd<-switch(user,
           nathan=file.path("~/Dropbox/njames/school/PhD/misc/gss_2017"),
           jacquelynneal=file.path("~jacquelynneal/Desktop/gss_2017"))

setwd(wd)

load(file.path(wd,"cache","com_plt.RData"))

library(shiny)
library(plotly) # https://plot.ly/r/

cats<-select(com_plt,cat1) %>% distinct() %>% flatten() %>% unlist()

ui <- fluidPage(
  
  titlePanel("CES Demo App"),
  
  sidebarLayout(
    sidebarPanel( 
      dateRangeInput("yr","Year", start = "2014-01-01", end = "2015-12-31",
                     min ="2005-01-01", max="2015-12-31", 
                     startview="decade", format = "yyyy"),
      selectInput("cat","Category", 
                  choices = c("Average annual expenditures","Food","Housing"),
                  multiple=TRUE, selected="Average annual expenditures"),
      uiOutput("ui"),
      checkboxGroupInput("cuchar", "CU category",
                         c("All" = "all",
                           "Age" = "age",
                           "Income" = "income")),
      actionButton("plotButton", "Plot!")
    ),
    mainPanel(
      plotlyOutput("plot"),
      verbatimTextOutput("event")
    ),
    position="left",fluid=TRUE)
)


server <- function(input, output) {
  
  output$ui<-renderUI({
    if (is.null(input$cat))
      return()

  #  selsub<-unique(filter(com_plt, cat1 %in% input$cat) %>% select(subcat))
   # selectInput("data", "Expenditure subcategory:", selsub, multiple=TRUE)
    selectInput("data", "Expenditure subcategory:", 
                list("foo"=c("a","b"),"bar"=c("c","d")), multiple=TRUE)
  })
  
  
  # using ggplotly & eventReactive to only plot when all options set
  plt<-eventReactive(input$plotButton,{
    p<-ggplot(filter(com_plt, cat1 %in% input$cat, cuchar %in% input$cuchar), 
              aes(yr,Mean,group=cugrp))+geom_line(aes(color=factor(cugrp)))
    
    # need to group by cugrp, cat1
    # add checkbox for SE          
    
    # p<-ggplot(filter(com_plt, cat1 %in% input$cat), 
    #           aes(yr,Mean,group=subcat))+
    #   geom_line(aes(color=subcat))
    
    ggplotly(p)
  })
  
  output$plot <- renderPlotly({
    plt()
  })
  

}


shinyApp(ui, server)


#filter(com_plt, cat1 %in% "Average annual expenditures", cuchar %in% "all")
