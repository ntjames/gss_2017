
#load libraries
library(tidyverse)
library(lubridate)
library(shiny)
library(plotly)
library(DT)
library(shinythemes)

#load data
load("dict.RData") # data dictionary & codes
load("com_plt.RData") # scraped combined tables plots

#load individually
load("fmli.RData") 
load("memi.RData")
load("mtbi.RData")
load("itbi.RData")
load("itii.RData")
load("ntaxi.RData")
load("fpar.RData")
load("mchi.RData")
load("fmld.RData")
load("memd.RData")
load("dtbd.RData")
load("expd.RData")
load("dtid.RData")

cats_t3<-filter(com_plt,level<=2) %>% select(cat1) %>% distinct() %>% flatten() %>% unlist()

#### Define UI ####
ui <- fluidPage( theme = shinytheme("readable"),
                 #shinythemes::themeSelector(),
                 
                 # Application title
                 #titlePanel("Consumer Expenditure Data"),
                 # useShinyjs(),
                 navbarPage("Consumer Expenditure Data",
                            
                            ### Tab 0 input ###
                            tabPanel("About",
                                     h1( 'JSM 2017 Data Challenge'), h3('Government Statistics Section'),
                                     a(strong('Consumer Expenditure Data'),href='a(https://www.bls.gov/cex/'),br(), 
                                     HTML(paste('The Consumer Expenditure Surveys (CE) program provides data on expenditures, 
                                                income, and demographic characteristics of consumers in the United States. 
                                                Data are provided in tables, databases, news releases, reports, and public-use microdata files. Data are 
                                                collected by the Census Bureau for BLS in two surveys, the Interview Survey for 
                                                major items and the Diary Survey for more
                                                frequently purchased items. CE data are primarily used to revise the 
                                                Consumer Price Index. The CE is the only Federal household survey to 
                                                provide information on the complete range of consumer expenditures 
                                                and incomes, as well as the characteristics of those consumers. 
                                                Researchers and government agencies also use CE data to study consumers 
                                                spending habits and trends.',tags$sup(1))),
                                     
                                     br(),
                                     br(),
                                     strong('Data Challenge'),
                                     p('For the GSS Data Challenge, we were interested in creating tools to allow quick, simple, interactive access to basic dataset
                                       and variable information as well as comparisons of annual trends for intitial exploratory data analysis. 
                                       Two modules were developed. '),
                                     strong('Interactive Data Dictionary'), 
                                     p('The Interactive Data Dictionary module allows the user to load any of the "major" CE Public Use Microdata
                                       datasets (8 Interview, 5 Diary) and explore summary statistics, codebook values, frequencies, and basic
                                       visualizations.' ),
                                     strong('Annual Trends'), 
                                     p('The Annual Trends module allows the user to view average annual Income and Expenditure 
                                       data from 2011 to 2015 for major categories overall and stratified by 8 Consumer Unit (CU) characteristics. The 
                                       interactive plot allows users to hover over a data point to view more information, 
                                       zoom/pan to focus on a specific area of interest,
                                       and isolate or hide individual traces.' ),
                                     br(),
                                     p(paste('1: '),a('https://www.bls.gov/cex/',href='https://www.bls.gov/cex/'))
                                     ), #close tab 0
                            
                            ### Tab 1 input ###
                            tabPanel("Interactive Data Dictionary",
                                     # Sidebar with a slider input for number of bins 
                                     sidebarLayout(
                                       
                                       sidebarPanel(
                                         selectInput("data_t1", "Choose a dataset:",
                                                     choices=list(
                                                       Interview=c("FMLI (characteristics, income, weights, and summary level expenditures for the CU)"="fmli",
                                                                   "MEMI (characteristics and income for each member in the CU)"="memi",
                                                                   "MTBI (detailed monthly expenditure file categorized by Universal Classification Code (UCC))"="mtbi",
                                                                   "ITBI (Consumer Unit monthly income file categorized by UCC)"="itbi",
                                                                   "ITII (Consumer Unit monthly imputed income file categorized by UCC)"="itii",
                                                                   "NTAXI (federal and state tax information for each tax unit in the CU)"="ntaxi",
                                                                   "FPAR (paradata about the survey)"="fpar",
                                                                   "MCHI (data about the contact history)"="mchi"),
                                                       Diary=c("FMLD (characteristics, income, weights, and summary level expenditures for the CU)"="fmld",
                                                               "MEMD (characteristics and income for each member in the CU)"="memd",
                                                               "DTBD (detailed weekly expenditure file categorized by UCC)"="dtbd",
                                                               "EXPD (detailed annual income file categorized by UCC)"="expd",
                                                               "DTID (Consumer Unit imputed income file categorized by UCC)"="dtid") 
                                                     )),
                                         uiOutput("ui_year_t1"),
                                         uiOutput("ui_qtr_t1"),
                                         uiOutput("ui_qtr_ex_t1"),
                                         uiOutput("ui_var_t1"), 
                                         uiOutput("ui_contopt_t1"), 
                                         actionButton("dispButton_t1", "Display")
                                         ,width=3),
                                       
                                       mainPanel(
                                         uiOutput("h1"),
                                         tableOutput("var_t1"),
                                         uiOutput("h2"),
                                         #    dataTableOutput("summ_t1"),
                                         uiOutput("summtab_t1"),
                                         uiOutput("h3"),
                                         plotlyOutput("plot_t1"), 
                                         verbatimTextOutput("event")
                                         ,width=9)
                                       
                                     ) #close sidebarLayout 
                            ), #close tab 1 input
                            
                            ### Tab 2 input ### 
                            #  removed
                            
                            ### Tab 3 input ###
                            tabPanel("Annual Trends",
                                     tags$head(tags$style(HTML(".multicol{font-size:15px;
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
  
  output$ui_qtr_ex_t1 <-renderUI({
    if (is.null(input$data_t1)|is.null(input$year_t1)|is.null(input$qtr_t1)|is.null(input$var_t1))
      return()
    
    if (input$qtr_t1=="1x"){ 
      text<-renderText({ "*“x” signifies that the 1st quarter file of the selected calendar year is not identical to the
        5th quarter file of the previous calendar year release." }) 
      text()
      }
    
    #  h1(textOutput("expl") )
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
  
  output$ui_contopt_t1<-renderUI({
    if (is.null(input$data_t1)|is.null(input$year_t1)|is.null(input$qtr_t1)|is.null(input$var_t1))
      return()
    
    fdat<-filter(get(input$data_t1),fileyear==input$year_t1,fileqtr==input$qtr_t1) %>%
      select(input$var_t1)
    
    #determine var type
    vartype<-fdat[[1]] %>% typeof()
    catvar<-switch(vartype,
                   character=TRUE,
                   integer=FALSE,
                   double=FALSE,
                   logical=TRUE)
    
    if(!catvar){
      radioButtons("contdisp","Plot options",choices=c("density"="dens","histogram"="hist"))
    }
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
  
  output$summtab_t1<-renderUI({
    # if (is.null(input$data_t1)|is.null(input$year_t1))
    #  return()
    t1<-t1()
    if (t1$catvar_t1){ 
      dataTableOutput("summ_t1a")
    } else {
      dataTableOutput("summ_t1b")
    }
    
  })
  
  output$summ_t1a <- DT::renderDataTable({
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
  },rownames= FALSE, options = list(lengthMenu = c(5, 10, 20), pageLength = 5, 
                                    orderClasses = TRUE))
  
  
  output$summ_t1b <- DT::renderDataTable({
    t1<-t1()
    if (t1$catvar_t1){
      group_by(t1$fdat_t1[1],t1$fdat_fc_t1) %>% dplyr::summarize(Freq=n()) %>%
        dplyr::rename(Category=`t1$fdat_fc_t1`) %>%
        full_join(.,t1$fdc_t1,by=c("Category"="Code Description")) %>%
        select("Code Value",Category,Freq)
    } else { #!don't reverse order of t1 & t2 na.omit has weird residual behavior
      t2<-t1$fdat_t1[1] %>% dplyr::summarize(n=n(),`NA`= sum(is.na( eval(parse(text=isolate(input$var_t1))) )))
      t1<-summarize_all(na.exclude(t1$fdat_t1[1]),funs(min,mean=round(mean(.),2),
                                                       median=round(median(.),2),
                                                       max,sd=round(sd(.),3),IQR))
      as.tibble(c(t1,t2))
    }
  },rownames= FALSE,options=list(paging = FALSE,searching=FALSE,ordering=FALSE))
  
  
  #display plot
  output$h3<-renderUI({
    t<-t1() 
    h3("Plot")
  })
  
  #output$plot_t1 <- renderPlot({
  output$plot_t1 <- renderPlotly({
    t1<-t1()
    if (t1$catvar_t1){
      p<- ggplot(as.tibble(t1$fdat_fc_t1),aes(value))+geom_bar()+
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              axis.text=element_text(size=14),
              axis.title=element_text(size=16,face="bold")) + labs(x="Category")
    } else {
      p<- ggplot(t1$fdat_t1,aes(x=eval(parse(text=isolate(input$var_t1) )) ) ) + labs(x="") +
        theme(axis.text=element_text(size=14),
              axis.title=element_text(size=16,face="bold")) 
      if (input$contdisp=="dens"){
        p<-p+geom_density()
      } else {
        p<-p+geom_histogram(stat="bin")
      }
      
    }
    
    ggplotly(p,tooltip=c("y"),width=1150,height=500)
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
            legend.text=element_text(size=10),
            axis.text=element_text(size=14),
            axis.title=element_text(size=16,face="bold")
            # ,axis.title.y = element_text(margin = margin(t = 0, r = 50, b = 0, l = 50))
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

