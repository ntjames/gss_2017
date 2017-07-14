library(shiny)
library(plotly)
# https://plot.ly/r/

ui <- fluidPage(
  
  titlePanel("CES Demo App"),
  
  sidebarLayout(
    sidebarPanel( 
      sliderInput("qtr","Quarter",min=2015,max=2016,value=c(2015.25,2015.5)),
      selectInput("cat","Category", 
                  choices = c("Expenditure - VOT" = "vot","Expenditure - VEQ" = "veq"),
                  multiple=TRUE, selected="veq"),
      uiOutput("ui")
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
  })
  
  # renderPlotly()
  output$plot <- renderPlotly({
    plot_ly(filter(expn,cat %in% input$cat & subcat %in% input$data), 
            x=~qyear, y=~val, split=~subcat, type="scatter", mode="lines") %>%
      layout(xaxis = list(title="quarter"), 
             yaxis = list(title="expenditure ($)")) 
  })
  
   output$event <- renderPrint({
     d <- event_data("plotly_hover")
     if (is.null(d)) "Hover on a point!" else d
   })

}


shinyApp(ui, server)