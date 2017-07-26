#set path depending on user
user<-system2("whoami", stdout=TRUE)
wd<-switch(user,
           nathan=file.path("~/Dropbox/njames/school/PhD/misc/gss_2017"),
           jacquelynneal=file.path("~jacquelynneal/Desktop/gss_2017"))

setwd(wd)
load(file.path(wd,"cache","com_plt.RData"))

library(tidyverse)
library(lubridate)
library(shiny)
library(plotly) # https://plot.ly/r/

cats<-filter(com_plt,level<=2) %>% select(cat1) %>% distinct() %>% flatten() %>% unlist()

ui <- fluidPage(
  
  titlePanel("CES Demo App"),
  
  sidebarLayout(
    sidebarPanel( 
      dateRangeInput("yr","Year", start = "2013-01-01", end = "2015-12-31",
                     min ="2005-01-01", max="2015-12-31", 
                     startview="decade", format = "yyyy"),
      selectInput("cat","Category", 
                  choices = cats,
                  multiple=TRUE, selected="Average annual expenditures"),
      uiOutput("ui"),
      checkboxInput("showse","Show SE"),
      checkboxGroupInput("cuchar", "CU category",
                         c("All" = "all",
                           "Age" = "age",
                           "Income" = "inc",
                           "Region" = "reg"),
                         selected="all"),
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
    
    #make and clean-up subcategory list
    din<-data.frame(select(com_plt,lev2,lev3))
    ulevs<-na.omit(unique(com_plt$lev2))
    sublist<-lapply(ulevs, function(x) unique(subset(din,lev2==x,select=lev3,drop=T)))
    names(sublist)<-ulevs
    sublist<-lapply(sublist, function(x) x[!is.na(x)]) #rm NAs within lists
    naonly<-sapply(sublist,function(x) all(is.na(x)) ) #rm items with only NA
    sublist[naonly]<-NULL
    
    #keep items of sublist that match categories in input$cat
    sublist2<-sublist[input$cat]
    if ( all( sapply(sublist2,function(x) all(is.null(x))) ) )
      return()
    
    naonly2<-sapply(sublist2,function(x) all(is.null(x)) ) #rm items with only NULL
    sublist2[naonly2]<-NULL
    
    selectInput("subcat", "Expenditure subcategory:", sublist2 , multiple=TRUE)
  })
  
  
  # using ggplotly & eventReactive to only plot when all options set
  plt<-eventReactive(input$plotButton,{
    
      yrs<-year(input$yr)
      filtered_plt<-filter(com_plt, cat1 %in% c(input$cat,input$subcat), 
             cuchar %in% input$cuchar, 
             between(yr,yrs[1],yrs[2]))
      
      p<-ggplot(filtered_plt, aes(yr,Mean,group=interaction(cugrp,cat1),color=cugrp,linetype=cat1))+
            geom_line() +  theme(legend.title=element_blank())
      #https://github.com/chriddyp/ggplot2-plotly-cookbook/blob/master/legends.R
      if (input$showse){
        p<-p+geom_linerange(data=filtered_plt,aes(yr,ymin=Mean-SE,ymax=Mean+SE))
      }
      
    # add additional years    
    # append category name in from of cugrp  
    # fix margins, axes, legends of plot


    ggplotly(p,tooltip=c("x","y","colour","linetype"),width=1000,height=700)
  })
  
  output$plot <- renderPlotly({
    plt()
  })
  
  #use this to check input vals
  # output$event <- renderPrint({
  #  year(input$yr)
  # })
  

}


shinyApp(ui, server)


#filter(com_plt, cat1 %in% "Average annual expenditures", cuchar %in% "inc")

#ggplot(filter(com_plt, cat1 %in% c("Average annual expenditures"), cuchar %in% c("reg","all")), 
#       aes(yr,Mean,group=interaction(cugrp,cat1),color=cat1,linetype=cugrp) )+ geom_line()
