#
# Inputting the different names of the datasets or variables? Choose
#

#set path depending on user
user<-system2("whoami", stdout=TRUE)
wd<-switch(user,
           nathan=file.path("~/Dropbox/njames/school/PhD/misc/gss_2017"),
           another_user=file.path("path_to_gss_2017_on_other_computer")) ##! need to edit this line
setwd(wd)

load(file.path(wd,"cache","xpa15_cache.RData"))

library(shiny)
library(tidyverse)
library(magrittr)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Consumer Expenditure Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("variable",
                     "Choose a variable:",
                     choices=c('Dining Out'='JDINEOQV','Alcohol Expenses'='JALOUTQV')),
         numericInput("obs", "Number of observations to view:", 5),
         
         selectInput("variable",
                     "Choose a variable:",
                     choices=c('Dining Out','Alcohol Expenses'))
      ),
      
      
      # Show a plot of the generated distribution
      mainPanel(
        verbatimTextOutput("summary"),
        
        tableOutput("view"),
        
        plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # dat <- read.csv('/Users/jacquelynneal/Downloads/intrvw15/expn15/xpa15.csv',stringsAsFactors = FALSE)
  
  # use %<>% from magrittr
  names(xpa15) %<>% toupper
  dat<- xpa15
  
#  datasetInput <- reactive({
#    switch(input$variable,
#           "Dining Out" = dat$JDINEOQV,
#           "Alchohol Expenses" = dat$JALOUTQV)
#  })
  
  output$summary <- renderPrint({
#    dataset <- datasetInput()
#    summary(dataset)
    summary(select(dat,input$variable))
  })
  
  # Show the first "n" observations
  output$view <- renderTable({
    head(select(dat,input$variable), n = input$obs)
  })
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      
      # x <- dat$QYEAR
      # mean.dineout <- c(mean(food.dat$JDINEOQV[food.dat$QYEAR==20151]),mean(food.dat$JDINEOQV[food.dat$QYEAR==20152]),mean(food.dat$JDINEOQV[food.dat$QYEAR==20153]),
      #                  mean(food.dat$JDINEOQV[food.dat$QYEAR==20154]),mean(food.dat$JDINEOQV[food.dat$QYEAR==20161]))
      # timePlot <- plot(y=mean.dineout,x=x,type='p')
     
      dat %>% group_by(QYEAR) %>% summarize_at(input$variable,funs(mean(.,na.rm=TRUE))) %>% 
       ggplot(aes(QYEAR,get(input$variable),group=1)) + geom_line()
      
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

