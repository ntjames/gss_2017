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
                choices = c("Expenditure - XPA" = "zzz","Income" = "xxx")),
      checkboxGroupInput("data", "Expenditure subcategory:",
                  c("Food - Dining" = "jdineoqv",
                    "Food - Grocery" = "jmkgrcqv",
                    "Food - Other" = "jothstqv")),
      checkboxInput("famSize", "Split by Family Size",value = FALSE)),
  mainPanel(plotlyOutput("plot"),verbatimTextOutput("event")),
    position="left",fluid=TRUE)
)

server <- function(input, output) {
  
  # renderPlotly() 
  output$plot <- renderPlotly({
    plot_ly(xpa15mn2,x=~Group.1, y=~jdineoqv,type="scatter",mode="lines",
            color=I("#66C2A5"), name="Food - Dining",split=~Group.2) %>% 
      layout(xaxis = list(title="quarter"), 
             yaxis = list(title="expenditure ($)"))  %>%
      add_trace(y=~jmkgrcqv,type="scatter",name="Food - Grocery",
                color=I("#FC8D62"))
  })
  
  output$event <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover on a point!" else d
  })
}



ui <- fluidPage(
  
  titlePanel("CES Demo App"),
  
  sidebarLayout(
    sidebarPanel( 
      selectInput("cat","Category", 
                  choices = c("Expenditure - VOT" = "zzz","Income" = "xxx")),
      checkboxGroupInput("data", "Expenditure subcategory:",
                         c("Fuel - Gasoline" = "a",
                           "Fuel - Diesel" = "b",
                           "Parking" = "c")),
      checkboxInput("famSize", "Split by Family Size",value = FALSE)),
    mainPanel(plotlyOutput("plot"),verbatimTextOutput("event")),
    position="left",fluid=TRUE)
)


server <- function(input, output) {
  
  # renderPlotly() 
  output$plot <- renderPlotly({
    plot_ly(vot15mn,x=~Group.1, y=~jgasoxqv,type="scatter",mode="lines",
            name="fuel - gasoline") %>%
      layout(xaxis = list(title="quarter"), 
             yaxis = list(title="expenditure ($)")) 
  })
  
  output$event <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover on a point!" else d
  })
}


shinyApp(ui, server)