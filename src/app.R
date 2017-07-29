
#set path depending on user
user<-system2("whoami", stdout=TRUE)
wd<-switch(user,
           nathan=file.path("~/Dropbox/njames/school/PhD/misc/gss_2017"),
		      'gosset\\nathan'=file.path("C:","Users","Nathan","Dropbox","njames","school","PhD","misc","gss_2017"),
           jacquelynneal=file.path("~jacquelynneal/Desktop/gss_2017"))
setwd(wd)

#source on surface
#source(file.path(wd,"src","app.R"))

#load libraries
library(tidyverse)
library(lubridate)
library(shiny)
library(plotly)
library(DT)
#library(shinyjs)

#load data
load(file.path(wd,"cache","maj.RData")) # major datasets for tab1
load(file.path(wd,"cache","dict.RData")) # data dictionary & codes
load(file.path(wd,"cache","com_plt.RData")) # scraped combined tables plots

cats_t3<-filter(com_plt,level<=2) %>% select(cat1) %>% distinct() %>% flatten() %>% unlist()

#### Define UI ####
ui <- fluidPage(
   
   # Application title
   #titlePanel("Consumer Expenditure Data"),
  # useShinyjs(),
navbarPage("Consumer Expenditure Data", # selected="Descriptives", #make Descriptives selected
  
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
        selectInput("data_t1", "Choose a dataset:",
                     choices=list(
                            Interview=c("FMLI"="fmli","MEMI"="memi","MTBI"="mtbi", "ITBI"="itbi",
                               "ITII"="itii","NTAXI"="ntaxi","FPAR"="fpar","MCHI"="mchi"),
                            Diary=c("FMLD"="fmld","MEMD"="memd","DTBD"="dtbd","EXPD"="expd",
                                    "DTID"="dtid") 
                              )),
        uiOutput("ui_year_t1"),
        uiOutput("ui_qtr_t1"),
        uiOutput("ui_var_t1"),       
        actionButton("dispButton_t1", "Display")
      ,width=3),
      
      mainPanel(
        uiOutput("h1"),
        tableOutput("var_t1"),
        uiOutput("h2"),
        dataTableOutput("summ_t1"),
        uiOutput("h3"),
        plotOutput("plot_t1"),
        verbatimTextOutput("event")
      ,width=9)
      
    ) #close sidebarLayout 
  ), #close tab 1 input
  
  ### Tab 2 input ### 
  #  removed
  
  ### Tab 3 input ###
  tabPanel("Annual",
   tags$head(tags$style(HTML(".multicol{font-size:16px;
                          height:auto;
                          -webkit-column-count: 2;
                          -moz-column-count: 2;
                          column-count: 2;
                          }"))),
    sidebarLayout(
      sidebarPanel( 
      dateRangeInput("year_t3","Year", start = "2011-01-01", end = "2015-12-31",
                     min ="2011-01-01", max="2015-12-31", 
                     startview="decade", format = "yyyy"),
      selectInput("cat_t3","Category", 
                  choices = cats_t3,
                  multiple=TRUE, selected="Average annual expenditures"),
      uiOutput("ui_t3"),
      checkboxInput("showse_t3","Show SE"),
      
      tags$div(align = "left", 
       class = "multicol",
        checkboxGroupInput("cuchar_t3", "CU category",
                         c("All" = "all",
                           "Age" = "age",
                           "Race"="rac",
                           "Education"="edu",
                           "Size of CU"="cus",
                           "Income (Quintiles)"="qui",
                           "Region" = "reg",
                           "Housing Tenure"="hou",
                           "Area Type"="toa"),
                         selected="all")
       ),
      actionButton("plotButton_t3", "Plot")
      
     ,width=2),
     
     mainPanel(
       plotlyOutput("plot_t3")
     ,width=10)
    
   ) # close sidebarLayout
) # close tab 3 input
  
) # close navbarPage
) # close ui fluidPage

#### Define server ####
server <- function(input, output) {
  
### tab 1 outputs ###

# dynamic UI  
  output$ui_year_t1<-renderUI({
    if (is.null(input$data_t1))
      return()
    
    yrvec0<-select(get(input$data_t1),fileyear) %>% distinct() %>% pull() 
    yrvec<-paste0("20",yrvec0)
    yrls<-as.list(yrvec0)
    names(yrls)<-yrvec
    
    selectInput("year_t1", "Choose a year:", yrls)
  })

  output$ui_qtr_t1<-renderUI({
    if (is.null(input$data_t1)|is.null(input$year_t1))
      return()
    
    qtrvec<-filter(get(input$data_t1),fileyear==input$year_t1) %>%
      select(fileqtr) %>% distinct() %>% pull() 
    
    selectInput("qtr_t1", "Choose a quarter:", qtrvec)
  })

  output$ui_var_t1<-renderUI({
    if (is.null(input$data_t1)|is.null(input$year_t1)|is.null(input$qtr_t1))
      return()
    
    # drop flag variables
    flgvars<- select(as.tibble(data_dic_vars),contains("Flag")) %>% distinct() %>% pull() %>% tolower()
      
    varvec<-filter(get(input$data_t1),fileyear==input$year_t1,fileqtr==input$qtr_t1) %>% 
      select(-one_of(flgvars)) %>% names()
    
    ## append names
    vv2 <-as.tibble(data_dic_vars) %>%
          filter(File==toupper(input$data_t1),
                 `Variable Name` %in% toupper(varvec),
                  as.integer(paste0("20",input$year_t1))>=`First  Year`,
                  as.integer(paste0("20",input$year_t1))<=`Last  Year`) %>% 
          select('Variable Name','Variable Description') %>%
          mutate(vname=paste(`Variable Name`,"(",`Variable Description`,")"))
    
    varlist<-select(vv2,'Variable Name') %>% pull()  %>% tolower() %>% as.list()
     
    varlab<- select(vv2,vname) %>% pull()
    names(varlist)<-varlab
    
    selectInput("var_t1", "Choose a variable:", varlist)
  })  
  
# display button reactive  
t1<-eventReactive(input$dispButton_t1,{
    
  data_t1_upper <- toupper(input$data_t1) 
  var_t1_upper <- toupper(input$var_t1) 
  
  # filter & select data
  # file, year (& qtr) determines available vars to select
  fdat_t1<-filter(get(input$data_t1),fileyear==input$year_t1,fileqtr==input$qtr_t1) %>%
   select(input$var_t1)
  
  #determine var type
  vartype_t1<-fdat_t1[[1]] %>% typeof()
  catvar_t1<-switch(vartype_t1,
                character=TRUE,
                integer=FALSE,
                double=FALSE,
                logical=TRUE)

  # filter info from data dictionary for current year
  fdv_t1<-filter(data_dic_vars,File==data_t1_upper ,`Variable Name`==var_t1_upper,
                as.integer(paste0("20",input$year_t1))>=`First  Year`,
                as.integer(paste0("20",input$year_t1))<=`Last  Year`) 
 
  out<-list(fdat_t1=fdat_t1,catvar_t1=catvar_t1,fdv_t1=fdv_t1)
  
  #filter info from codes (if cat) 
  #!! newid is char, but has no codes & is super long
  #!! some vars in data_dic_codes aren't exactly same as variables
  #  (see e.g. BUILDING where dict needs to be padded to length 2 to match) 
  if (catvar_t1){
    fdc_t1<-filter(data_dic_codes,File==data_t1_upper,
                   `Variable Name`==var_t1_upper,
                   as.integer(paste0("20",input$year_t1))>=`First Year`,
                   as.integer(paste0("20",input$year_t1))<=`Last Year`) %>%
            select(`Variable Name`,`Code Value`,`Code Description`)
  
    if (nrow(fdc_t1)>0) {
      fdat_fc_t1<-factor(fdat_t1[[1]],
                    levels=unlist(flatten(select(fdc_t1,`Code Value`))),
                    labels=unlist(flatten(select(fdc_t1,`Code Description`))))
      } else { # character var w/o code (e.g. newid)
        fdat_fc_t1<-factor(fdat_t1[[1]])
      }
    out <- append(out, list(fdc_t1=fdc_t1,fdat_fc_t1=fdat_fc_t1))    
  }

out 
})
  
# display data dictionary variable info
output$h1<-renderUI({
  t<-t1() 
  h3("Data Dictionary")
  })

output$var_t1<-renderTable({
  t1<-t1()
  t1$fdv_t1
},digits=0)  
  
#display summary
output$h2<-renderUI({
  t<-t1() 
  h3("Summary")
})

output$summ_t1 <- DT::renderDataTable({
  t1<-t1()
  if (t1$catvar_t1){
    group_by(t1$fdat_t1[1],t1$fdat_fc_t1) %>% dplyr::summarize(Freq=n()) %>%
      dplyr::rename(Category=`t1$fdat_fc_t1`) %>%
      full_join(.,t1$fdc_t1,by=c("Category"="Code Description")) %>%
      select("Code Value",Category,Freq)
  } else { #!don't reverse order of t1 & t2 na.omit has weird residual behavior
    t2<-t1$fdat_t1[1] %>% dplyr::summarize(n=n(),`NA`= sum(is.na( eval(parse(text=isolate(input$var_t1))) )))
    t1<-summarize_all(na.exclude(t1$fdat_t1[1]),funs(min,mean=round(mean(.),2),
                                                     median,max,sd=round(sd(.),3),IQR))   
    as.tibble(c(t1,t2))
  }
},rownames= FALSE)
   
#display plot
output$h3<-renderUI({
  t<-t1() 
  h3("Plot")
})

output$plot_t1 <- renderPlot({
  t1<-t1()
  if (t1$catvar_t1){
    ggplot(as.tibble(t1$fdat_fc_t1),aes(value))+geom_bar()+
      theme(axis.text.x = element_text(angle = 30, hjust = 1),
            axis.text=element_text(size=14),
            axis.title=element_text(size=16,face="bold")) + labs(x="Category")
  } else {
    ggplot(t1$fdat_t1,aes(eval(parse(text=isolate(input$var_t1))) )) + geom_density() +
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=16,face="bold")) + labs(x="")
  }
  
})
  
#use this to check input vals
# output$event <- renderPrint({
#    c(input$data_t1,
#    input$year_t1,
#    input$qtr_t1,
#    input$var_t1)
#  })

### tab 2 outputs ###

  
### tab 3 outputs ###
    

# dynamic UI  
  output$ui_t3<-renderUI({
   
    #make and clean-up subcategory list
    din<-data.frame(select(com_plt,lev2,lev3))
    ulevs<-na.omit(unique(com_plt$lev2))
    sublist<-lapply(ulevs, function(x) unique(subset(din,lev2==x,select=lev3,drop=T)))
    names(sublist)<-ulevs
    sublist<-lapply(sublist, function(x) x[!is.na(x)]) #rm NAs within lists
    naonly<-sapply(sublist,function(x) all(is.na(x)) ) #rm items with only NA
    sublist[naonly]<-NULL
   
   #keep items of sublist that match categories in input$cat ??
 #  sublist2<-sublist[input$cat_t3]
    sublist2<-sublist
   if ( all( sapply(sublist2,function(x) all(is.null(x))) ) )
     return()
   
   naonly2<-sapply(sublist2,function(x) all(is.null(x)) ) #rm items with only NULL
   sublist2[naonly2]<-NULL
   
   selectInput("subcat_t3", "Expenditure subcategory:", sublist2 , multiple=TRUE)
 })
 
 # using ggplotly & eventReactive to only plot when all options set
 plt<-eventReactive(input$plotButton_t3,{
   
   yrs<-year(input$year_t3)
   filtered_plt<-filter(com_plt, cat1 %in% c(input$cat_t3,input$subcat_t3), 
                        cuchar %in% input$cuchar_t3, 
                        between(yr,yrs[1],yrs[2]))
   
   p<-ggplot(filtered_plt, aes(yr,Mean,group=interaction(cugrp,cat1),color=cugrp,linetype=cat1))+
     geom_line()+ labs(x="Year",y="Mean ($)") + 
     theme(legend.title=element_blank(),
           legend.text=element_text(size=12),
           axis.text=element_text(size=14),
           axis.title=element_text(size=16,face="bold"),
           axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))
           ) 
   
   if (input$showse_t3){
     p<-p+geom_linerange(data=filtered_plt,aes(yr,ymin=Mean-SE,ymax=Mean+SE))
   }
   
   ggplotly(p,tooltip=c("x","y","colour","linetype"),width=1400,height=700)
   
 })
 
 output$plot_t3 <- renderPlotly({
   plt()
 })
 

} #close server

# Run the application 
shinyApp(ui = ui, server = server)

