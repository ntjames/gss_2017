
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

# don't perform munging i.e. keep all data (e.g. diary and individual EXPN)
 load.project(list('cache_loading'=FALSE,'munging'=FALSE)) 

if (0) {
#Interview
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
  #         contain UCCs & abbreviated titles (not included app)
  # EXPN - 43 detailed expenditure files

# Diary
  # FMLD - Contains CU chars, income, and chars and earnings of the ref person and spouse.    
  # MEMD - Member Characteristics and Income" 
  # DTBD - contains CU characteristic and income data.
  # EXPD - expenditure recorded by a CU in a weekly diary is identified by UCC
  # DTID -  similar to the DTBD file, w/ imputation
  # DSTUB - aggregation scheme (not included app)
}

## function to append major datasets
read_app<-function(pre){
  pat<-paste0(pre,"[0-9]")
  files<-ls(envir=.GlobalEnv)[grep(pat,ls(envir=.GlobalEnv))]
  if (pre=="ntaxi"){ pos<-c(6,9)  } else { pos<-c(5,8) }
  
  #add file name as variable, then use separate to split into year and quarter
  appfiles<-bind_rows(lapply(files, function(x) mutate(get(x),fileyrqtr=str_sub(x,pos[1],pos[2]) ))) %>%
    separate(fileyrqtr,into=c("fileyear","fileqtr"),sep=2)
  assign(pre,appfiles, envir=.GlobalEnv)
}

## Interview ## 
majds_i<-list("fmli","memi","mtbi","itbi","itii","ntaxi")
tmp<-lapply(majds_i,read_app)

# delete all of the independent data frames from memory
pat_i<-paste0(unlist(majds_i),"[0-9]",collapse="|")
ind_files<-ls()[grep(pat_i,ls())]
rm(list=ind_files)

# clear up RAM
gc()

#process paradata
pards_i<-c("fpar","mchi")

mchi<-as.tibble(mchi1415) %>% separate(qyear,into=c("fileyear","fileqtr"),sep=4) %>% 
  mutate(fileyear=str_sub(fileyear,3,4))
fpar<-as.tibble(fpar1415) %>% separate(qyear,into=c("fileyear","fileqtr"),sep=4) %>% 
  mutate(fileyear=str_sub(fileyear,3,4))

int_files<-c(unlist(majds_i),pards_i)

## Diary ##
majds_d<-list("fmld","memd","dtbd","expd","dtid")
tmp<-lapply(majds_d,read_app)
dry_files<-unlist(majds_d)

#save major datasets
major_ds<-c(int_files,dry_files)
save(list=major_ds,file=file.path(wd,"cache","maj.RData"))

#smaller version of data for deploy app
for (j in major_ds){
  ds<-get(j)
  dsc<-filter(ds,fileqtr %in% c("3","4","1x"))
  assign(j,dsc)
  save(list=j, file=file.path(wd,"deploy",paste0(j,".RData")))
}

#read in data dictionary 
stubpath<-file.path(getwd(),"stubdata")

data_dic_vars<-read_excel(file.path(stubpath,"ce_pumd_interview_diary_dictionary.xlsx"), 
                          sheet=2, col_names = TRUE, skip=2)

data_dic_codes<-read_excel(file.path(stubpath,"ce_pumd_interview_diary_dictionary.xlsx"), 
                           sheet=3, col_names = TRUE, skip=2)

save(data_dic_vars,data_dic_codes,file=file.path(wd,"cache","dict.RData"))
