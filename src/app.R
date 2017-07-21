#
# Inputting the different names of the datasets or variables? Choose
#

#set path depending on user
user<-system2("whoami", stdout=TRUE)
wd<-switch(user,
           nathan=file.path("~/Dropbox/njames/school/PhD/misc/gss_2017"),
           jacquelynneal=file.path("~jacquelynneal/Desktop/gss_2017"))
setwd(wd)

load(file.path(wd,"cache","xpa15_cache.RData"))

library(shiny)
library(tidyverse)
library(magrittr)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   #titlePanel("Consumer Expenditure Data"),
   
   navbarPage("Consumer Expenditure Data",
                      tabPanel("About",
                               h1( 'Data Challenge 2017'), h3('Government Statistics Section'),
                               p('For the GSS Data Challenge, we were interested in exploring the possibilities of
                                 interactive exploratory data analysis. FILL IN MORE HERE')),
                      tabPanel("Descriptives",
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     
      sidebarPanel(
         selectInput("variable",
                     "Choose a variable:",
                     choices=names(dat)),
         selectInput("subcatvar",
                     "Choose a category:",
                     choices=unique(dat$subcat)),
         numericInput("obs", "Number of observations to view:", 5)
         
         #selectInput("variable",
         #             "Choose a variable:",
         #             choices=c('Dining Out','Alcohol Expenses'))
      ),
      
      
      
      # Show a plot of the generated distribution
      mainPanel(
          verbatimTextOutput("summary"),
          tableOutput("view"),
          plotOutput("distPlot")
        )
   )),
tabPanel("Annual"),
tabPanel("Quarterly"))
   )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # dat <- read.csv('/Users/jacquelynneal/Downloads/intrvw15/expn15/xpa15.csv',stringsAsFactors = FALSE)
  
  # use %<>% from magrittr
  names(xpa15) %<>% toupper
  dat<- data.frame(expn)
  dat$qyear <- factor(dat$qyear,levels=c('20151','20152','20153','20154','20161'),labels=c('Q1','Q2','Q3','Q4','Q5'))
  
  #output$summary <- renderPrint({
#    dataset <- datasetInput()
#    summary(dataset)
    #str(select(dat,input$variable))
  #})
  
  # Show the first "n" observations
  output$view <- renderTable({
  summary(dat)
  })
   
   output$distPlot <- renderPlot({
     
       dat[dat$subcat==input$subcatvar,] %>% group_by(qyear)  %>% 
        ggplot(aes(qyear,get(input$variable),group=1)) + geom_line()
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

