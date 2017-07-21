#set path depending on user
user<-system2("whoami", stdout=TRUE)
wd<-switch(user,
           nathan=file.path("~/Dropbox/njames/school/PhD/misc/gss_2017"),
           jacquelynneal=file.path("~jacquelynneal/Desktop/gss_2017"))

#wd<- file.path("~/Desktop/gss_2017")##! need to edit this line
setwd(wd)

load(file.path(wd,"cache","expn.RData"))

library(shiny)
library(plotly) # https://plot.ly/r/

ui <- fluidPage(
  
  titlePanel("CES Demo App"),
  
  sidebarLayout(
    sidebarPanel( 
      sliderInput("qtr","Quarter",min=2015,max=2016,value=c(2015.25,2015.5)),
      selectInput("cat","Category", 
                  choices = c("Expenditure - VOT" = "vot",
                              "Expenditure - VEQ" = "veq",
                              "Expenditure - CLA" = "cla",
                              "Expenditure - RNT" = "rnt"),
                  multiple=TRUE, selected="veq"),
      uiOutput("ui"),
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

    selsub<-unique(filter(expn, cat %in% input$cat) %>% select(subcat))
    selectInput("data", "Expenditure subcategory:", selsub, multiple=TRUE)
   # selectInput("data", "Expenditure subcategory:", list("foo"=c("a","b"),"bar"=c("c","d")), multiple=TRUE)
  })
  
  # using plotly
  # output$plot <- renderPlotly({
  #   plot_ly(filter(expn,cat %in% input$cat & subcat %in% input$data),
  #           x=~qyear, y=~val, split=~subcat, type="scatter", mode="lines") %>%
  #     layout(xaxis = list(title="quarter"),
  #            yaxis = list(title="expenditure ($)"))
  # })
  
  # output$event <- renderPrint({
  #   d <- event_data("plotly_hover")
  #   if (is.null(d)) "Hover on a point!" else d
  # })
  
  # using ggplotly & eventReactive to only plot when all options set
  plt<-eventReactive(input$plotButton,{
    p<-ggplot(filter(expn,cat %in% input$cat & subcat %in% input$data), aes(qyear,val,group=subcat))+
      geom_line(aes(color=subcat))
    
    ggplotly(p)
  })
  
  output$plot <- renderPlotly({
    plt()
  })
  

}


shinyApp(ui, server)

