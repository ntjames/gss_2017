#
# Inputting the different names of the datasets or variables? Choose
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Consumer Expenditure Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("variable",
                     "Choose a variable:",
                     choices=c('Dining Out','Alcohol Expenses')),
         numericInput("obs", "Number of observations to view:", 10),
         
         selectInput("variable",
                     "Choose a variable:",
                     choices=c('Dining Out','Alcohol Expenses'))
      ),
      
      
      # Show a plot of the generated distribution
      mainPanel(
        verbatimTextOutput("summary"),
        
        tableOutput("view")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  dat <- read.csv('/Users/jacquelynneal/Downloads/intrvw15/expn15/xpa15.csv',stringsAsFactors = FALSE)
  
  datasetInput <- reactive({
    switch(input$variable,
           "Dining Out" = dat$JDINEOQV,
           "Alchohol Expenses" = dat$JALOUTQV)
  })
  
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  # Show the first "n" observations
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      
      x <- dat$QYEAR
      mean.dineout <- c(mean(food.dat$JDINEOQV[food.dat$QYEAR==20151]),mean(food.dat$JDINEOQV[food.dat$QYEAR==20152]),mean(food.dat$JDINEOQV[food.dat$QYEAR==20153]),
                        mean(food.dat$JDINEOQV[food.dat$QYEAR==20154]),mean(food.dat$JDINEOQV[food.dat$QYEAR==20161]))
      timePlot <- plot(y=mean.dineout,x=x,type='p')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

