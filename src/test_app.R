wd<-file.path("~/Dropbox/njames/school/PhD/misc/GSS_data_challenge/gss_2017")
setwd(wd)
library(ProjectTemplate)
load.project()

library(shiny)
library(plotly)
# https://plot.ly/r/

#test app mock-up, not yet functional

ui <- fluidPage(
  
  titlePanel("CES Demo App"),
  
  sidebarLayout(
    sidebarPanel( 
      selectInput("cat","Category", 
                  choices = c("Expenditure - VOT" = "vot","Expenditure - VEQ" = "veq"),
                  multiple=TRUE, selected="veq"),
      checkboxGroupInput("data", "Expenditure subcategory:",
                         c("Fuel - Gasoline" = "a",
                           "Fuel - Diesel" = "b",
                           "Parking" = "c")),
      checkboxInput("famSize", "Split by Family Size",value = FALSE)
      ),
    mainPanel(
      plotlyOutput("plot"),
      verbatimTextOutput("event")
      ),
    position="left",fluid=TRUE)
)


server <- function(input, output) {
  
  # renderPlotly() 
  output$plot <- renderPlotly({
    plot_ly(filter(expn,cat %in% input$cat), x=~qyear, y=~val, split=~subcat, type="scatter", mode="lines") %>%
      layout(xaxis = list(title="quarter"), 
             yaxis = list(title="expenditure ($)")) 
  })
  
  output$event <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover on a point!" else d
  })
}


shinyApp(ui, server)