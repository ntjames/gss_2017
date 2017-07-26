#
# Inputting the different names of the datasets or variables? Choose
#

#set path depending on user
user<-system2("whoami", stdout=TRUE)
wd<-switch(user,
           nathan=file.path("~/Dropbox/njames/school/PhD/misc/gss_2017"),
           jacquelynneal=file.path("~jacquelynneal/Desktop/gss_2017"))
setwd(wd)

#load libraries
library(shiny)
library(tidyverse)
library(magrittr)

# use %<>% from magrittr
#names(xpa15) %<>% toupper
#dat<- data.frame(expn)
#dat$qyear <- factor(dat$qyear,levels=c('20151','20152','20153','20154','20161'),labels=c('Q1','Q2','Q3','Q4','Q5'))

#load data
load(file.path(wd,"cache","fmli.RData"))
load(file.path(wd,"cache","dict.RData"))

#### Define UI ####
ui <- fluidPage(
   
   # Application title
   #titlePanel("Consumer Expenditure Data"),
   
navbarPage("Consumer Expenditure Data",
  
  ### Tab 0 input ###
  tabPanel("About",
         h1( 'Data Challenge 2017'), h3('Government Statistics Section'),
         p('For the GSS Data Challenge, we were interested in exploring the possibilities of
           interactive exploratory data analysis. FILL IN MORE HERE')
  ), #close tab 0
  
  ### Tab 1 input ###
  tabPanel("Descriptives",
   # Sidebar with a slider input for number of bins 
    sidebarLayout(
      
      sidebarPanel(
        selectInput("data_t1",
                     "Choose a dataset:",
                     choices=c("FMLI"="fmli")),
        selectInput("year_t1",
                    "Choose a year:",
                    choices=c("2015"="15","2016"="16")),        
        selectInput("qtr_t1",
                     "Choose a quarter:",
                     choices=c("1x","1")),
        selectInput("var_t1",
                    "Choose a variable:",
                    choices=c("childage","rwaterpc")),
      #  numericInput("obs_t1", "Number of observations to view:", 5),
        actionButton("dispButton_t1", "Display")
      ),
      
      mainPanel(
        verbatimTextOutput("summary_t1"),
        tableOutput("view_t1"),
        plotOutput("distPlot_t1"),
        verbatimTextOutput("event")
      )
      
    ) #close sidebarLayout 
  ), #close tab 1 input
  
  ### Tab 2 input ### 
  tabPanel("Quarterly",
    sidebarLayout(
             
      sidebarPanel(
        selectInput("variable_t2",
                    "Choose a variable:",
                    choices=names(dat)),
        numericInput("obs_t2", "Number of observations to view:", 5)
      ),
             
      mainPanel(
        verbatimTextOutput("summary_t2"),
        tableOutput("view_t2"),
        plotOutput("distPlot_t2")
      )
    ) # close sidebarLayout
  ), # close tab 2 input
  
  ### Tab 3 input ###
  tabPanel("Annual"
           
  )
  
) # close navbarPage
) # close ui fluidPage

#### Define server ####
server <- function(input, output) {
  
  #input$data_t1
  #input$year_t1
  #input$qtr_t1
  #input$var_t1
  
  t1<-eventReactive(input$dispButton_t1,{
    data_t1_upper<- toupper(input$data_t1) 
    var_t1_upper<-  toupper(input$var_t1) 
  # filter & select data
  
  # file, year (& qtr) determines available vars to select
  #  fdat_t1<- filter(eval(parse(text=input$data_t1)),fileyear==input$year_t1,fileqtr==input$qtr) %>% 
  #    select(input$var_t1) %>% as.tibble()
    # fdat_t1<-filter(get(input$data_t1),fileyear==input$year_t1) %>% 
    #   slice(1:5) %>%
    #   select(input$var_t1)
    # fdat_t1[[1]] %>% typeof()
  fdat_t1<- filter(get(input$data_t1),fileyear==input$year_t1,fileqtr==input$qtr_t1) %>%
   select(input$var_t1)
  
  #determine var type
 vartype_t1<-fdat_t1[[1]] %>% typeof()
 catvar_t1<-switch(vartype_t1,
                character=TRUE,
                integer=FALSE)

  # filter info from data dictionary
 fdv_t1<-filter(data_dic_vars,File==data_t1_upper ,`Variable Name`==var_t1_upper)
 
  
  #filter info from codes (if applicable?)
 # if (catvar_t1){
 #   fdc_t1<-filter(data_dic_codes,File==data_t1_upper,`Variable Name`==var_t1_upper)
 # 
 #   fdat_fc_t1<-factor(fdat_t1[[1]],
 #                   levels=unlist(flatten(select(fdc_t1,`Code Value`))),
 #                   labels=unlist(flatten(select(fdc_t1,`Code Description`))))
 #  }
})
  
### tab 1 outputs ###
output$summary_t1 <- renderPrint({
  t1()
  #display data dict info
 # data.frame(fdv_t1)
#  if(catvar_t1){
#    data.frame(fdc_t1)
#  }
})
  
#output$view_t1 <- renderTable({
#  t1()
  #display summary
#   if (catvar_t1){
#     table(fdat_fc_t1)
#   } else {
#     summary(fdat_t1[[1]])
#   }
#   
# })
   
output$distPlot_t1 <- renderPlot({
#  filter(dat,subcat==input$subcatvar_t1) %>% group_by(qyear) %>% 
#    ggplot(aes(qyear,get(input$variable_t1),group=1)) + geom_line()
})
  
#use this to check input vals
 output$event <- renderPrint({
   c(input$data_t1,
   input$year_t1,
   input$qtr_t1,
   input$var_t1)
 })

### tab 2 outputs ###

  
### tab 3 outputs ###
    


} #close server

# Run the application 
shinyApp(ui = ui, server = server)

