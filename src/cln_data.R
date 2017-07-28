
#set path depending on user
user<-system2("whoami", stdout=TRUE)
wd<-switch(user,
       nathan=file.path("~/Dropbox/njames/school/PhD/misc/gss_2017"),
       jacquelynneal=file.path("~jacquelynneal/Desktop/gss_2017"))
setwd(wd)

#load ProjectTemplate ( http://projecttemplate.net )
#see gss_2017/config/global.dcf for config. options (e.g., libraries, preprocessing)
library(ProjectTemplate)
library(readxl)

#load.project()

# don't perform munging  i.e. keep all data (e.g. diary and individual EXPN)
 load.project(list('cache_loading'=FALSE,'munging'=FALSE)) 

# only load cache
# load.project(list('dat_loading'=FALSE,'munging'=FALSE)) 

#currently use only CESD PUMD Interview files
#! add major Diary files
if (0) {
  # FMLI - CU characteristics, income, and summary level expenditures (1 record per CU)
  # MEMI - member characteristics, income data 
  # MTBI - monthly expenditures at UCC level (multiple records per CU - 
  #                                         1 record per expenditure per month/year)
  # ITBI - income converted to monthly time frame
  # ITII - imputation variants of income converted to monthly time frame
  # NTAXI - fed. and state tax info
  # FPAR - CU level paradata about interview survey
  # MCHI - para data about each interview contact attempt
   
  # ISTUB - aggregation scheme used in published CES interview tables, 
  #         contain UCCs & abbreviated titles
   
  # EXPN - 43 detailed expenditure files
}


#read in data dictionary 
stubpath<-file.path(getwd(),"stubdata")

data_dic_vars<-read_excel(file.path(stubpath,"ce_pumd_interview_diary_dictionary.xlsx"), 
                     sheet=2, col_names = TRUE, skip=2)

data_dic_codes<-read_excel(file.path(stubpath,"ce_pumd_interview_diary_dictionary.xlsx"), 
                          sheet=3, col_names = TRUE, skip=2)

save(data_dic_vars,data_dic_codes,file=file.path(wd,"cache","dict.RData"))

## read-in major datasets, append

# fmli_files<-ls()[grep("fmli[0-9]",ls())]

#add file name as variable for each FMLI, then use separate to split into year and quarter
#fmli<-bind_rows(lapply(fmli_files, function(x) mutate(get(x),fileyrqtr=str_sub(x,5,8)  ))) %>%
#  separate(fileyrqtr,into=c("fileyear","fileqtr"),sep=2)

#save(fmli,file=file.path(wd,"cache","fmli.RData"))

read_app<-function(pre){
  pat<-paste0(pre,"[0-9]")
  files<-ls(envir=.GlobalEnv)[grep(pat,ls(envir=.GlobalEnv))]
    
  #add file name as variable, then use separate to split into year and quarter
  appfiles<-bind_rows(lapply(files, function(x) mutate(get(x),fileyrqtr=str_sub(x,5,8) ))) %>%
    separate(fileyrqtr,into=c("fileyear","fileqtr"),sep=2)
  assign(pre,appfiles, envir=.GlobalEnv)
}

read_app("fmli")

majds<-list("fmli","memi","itbi")

tmp<-lapply(majds,read_app)

# delete all of the independent data frames from memory
# rm(list=fmli_files)

# clear up RAM
gc()


save(list=unlist(majds),file=file.path(wd,"cache","maj.RData"))



## STEP 1: READ IN THE STUB PARAMETER FILE AND CREATE FORMATS  
#?  need to modify for multiple files?
stubfn<-dir(stubpath,"*.txt")

#get wrapped rows, append X3 in wrapped row to X3 in previous row 
stubfile0<-read_table(file.path(stubpath,stubfn), skip=1,
               col_names=c("type","level","title","var_ucc","source","factor","group"))
stub_rw<-1:nrow(stubfile0)
w2<-stub_rw[stubfile0$type!=1] #wrap rows
w1<-w2-1 # row above wrap row

stubfile0[w1,"title"]<-paste(unlist(stubfile0[w1,"title"]),unlist(stubfile0[w2,"title"]))

## subset data (drop wrap rows, title rows, and ASSET and ADDENDA group),
# create new line variable
stubfile<-filter(stubfile0, type==1, source != "T",
               group %in% c("CUCHARS", "FOOD", "EXPEND", "INCOME")) %>%
          mutate(count=9999+1:nrow(.),line=paste0(count,level))

# start with a character vector with ten blank strings..
curlines <- rep( "" , 10 )

# initiate a matrix containing the line numbers of each expenditure category
aggfmt0 <- matrix( nrow = nrow( stubfile ) , ncol = 10 )

# loop through each record in the stubfile..
for ( i in seq( nrow( stubfile ) ) ){
  
  # if the 'UCC' variable is numeric 
    if (  identical(as.character(stubfile[i,"source"]),"I") ){
    
    # save the line number as the last element in the char vector
    curlines[ 10 ] <- unlist(stubfile[ i , "line" ])
    
    # otherwise blank it out
  } else curlines[ 10 ] <- ""
  
  # store the current line and level in separate atomic variables
  curlevel <- unlist(stubfile[ i , "level" ])
  
  # write the current line inside the length-ten character vector
  curlines[ curlevel ] <- unlist(stubfile[ i , "line" ])
  
  # if the current level is 1-8, blank out everything above it up to nine
  if ( curlevel < 9 ) curlines[ (curlevel+1):9 ] <- ""
  
  # remove actual value
  savelines <- curlines
  savelines[ curlevel ] <- ""
  
  # overwrite the entire row with the character vector of length ten
  aggfmt0[ i , ] <- savelines
}

# convert the matrix to a data frame and name columns line1 - line10
# tack on the UCC and line columns from the stubfile (which has the same number of records)
# remove records where the UCC is not numeric, order by UCC, rename line to compare
aggfmt1 <- as_tibble(aggfmt0) %>% setNames(paste0( "line" , 1:10 )) %>%
      bind_cols(select(stubfile,var_ucc,line,source)) %>%
      filter(source=="I") %>% arrange(var_ucc) %>% rename(compare = line) %>%
      mutate(source=NULL)

# reset the row names/numbers
#rownames( aggfmt1 ) <- NULL

# transpose the data, holding UCC and compare
aggfmt2 <- melt(data.frame(aggfmt1), id=c("var_ucc","compare"))
names( aggfmt2 )[ 4 ] <- "line"

# retain the UCC-to-line crosswalk wherever the 'line' variable is not blank
aggfmt <- subset( aggfmt2 , line != "" , select = c( "var_ucc" , "line" ) )

# re-order the data frame by UCC
aggfmt <- aggfmt[ order( aggfmt$var_ucc ) , ]



#scratch below

# 
# #filter(data_dic_codes,File=='FMLI',`Variable Name`=='RWATERPC')
# 
# datafile<-"fmli"
# year<-"15"
# qtr<-"1x"
# #var<-"childage"
# #var<-"rwaterpc"
# var<-"division"
# 
# datafilenm<-toupper(datafile)
# varnm<-toupper(var)
# 
# # filter & select data
# 
# # file, year (& qtr) determines available vars to select
# ## filter(fmli,fileyear=="16",fileqtr=="1") %>% select(childage,fileyear) %>% as.tibble()
# fdat<-filter(eval(parse(text=datafile)),fileyear==year,fileqtr==qtr) %>% select(var) %>% as.tibble()
# 
# #determine var type
# vartype<-typeof(fdat[[1]])
# catvar<-switch(vartype,
#        character=TRUE,
#        integer=FALSE)
# 
# # filter info from data dictionary
# ## filter(data_dic_vars,File=='FMLI',`Variable Name`=='CHILDAGE')
# fdv<-filter(data_dic_vars,File==datafilenm,`Variable Name`==varnm)
# 
# #filter info from codes (if applicable?)
# ## filter(data_dic_codes,File=='FMLI',`Variable Name`=='CHILDAGE')
# if (catvar){
# fdc<-filter(data_dic_codes,File==datafilenm,`Variable Name`==varnm)
# 
# fdat_fc<-factor(fdat[[1]],
#        levels=unlist(flatten(select(fdc,`Code Value`))),
#        labels=unlist(flatten(select(fdc,`Code Description`))))
# }
# 
# #check if min number of chars in fdat[[1]] matches min number in dictionary
# # if not pad dictionary
# 
# #check if dictionary has more than 10 categories if yes, then pad to max length
# str_pad( unlist(flatten(select(fdc,`Code Value`))),2,"left",pad="0") 
# 
# ## output
# 
# #display data dict info
# data.frame(fdv)
# if(catvar){
#   data.frame(fdc)
# }
# 
# #display summary
# if (catvar){
#   table(fdat_fc)
# } else {
#   summary(fdat[[1]])
# }
# 
# ggplot(as.tibble(fdat_fc),aes(value))+geom_bar()
# ggplot(fdat,aes(eval(parse(text=var)))) + geom_density()
# 
# select(data_dic_vars,contains("Flag")) %>% distinct() %>% p
