#set path depending on user
user<-system2("whoami", stdout=TRUE)
wd<-switch(user,
           nathan=file.path("~/Dropbox/njames/school/PhD/misc/gss_2017"),
           jacquelynneal=file.path("~jacquelynneal/Desktop/gss_2017"))
setwd(wd)

#load libraries
library(tidyverse)
library(readxl)
library(stringr)

## Process data for 2011 - 2015 ##
yrs<-c("2015","2014","2013")
yrs2<-c("2012")
yrs3<-c("2011")
grp<-c("age","race","educat","cusize","quintile","region","tenure")

# CESD links and local paths
dl_links<-as.vector(outer(yrs,grp, 
                FUN=function(x,y) paste0("https://www.bls.gov/cex/",x,"/combined/",y,".xlsx")))
dl_paths<-as.vector(outer(yrs,grp, FUN=function(x,y) file.path(wd,"scraped",paste0(x,y,".xlsx") ) ))

dl_links2<-as.vector(outer(yrs2,grp, 
                          FUN=function(x,y) paste0("https://www.bls.gov/cex/",x,"/combined/",y,".xls")))
dl_paths2<-as.vector(outer(yrs2,grp, FUN=function(x,y) file.path(wd,"scraped",paste0(x,y,".xls") ) ))

dl_links3<-as.vector(outer(yrs3,grp, 
                           FUN=function(x,y) paste0("https://www.bls.gov/cex/",x,"/stnderror/",y,".xls")))
dl_paths3<-as.vector(outer(yrs3,grp, FUN=function(x,y) file.path(wd,"scraped",paste0(x,y,".xls") ) ))

#download data (only need to dl once)
#mapply(function(x,y,...) download.file(url=x, destfile=y,...), dl_links, dl_paths ,MoreArgs = list(mode="wb") )
#mapply(function(x,y,...) download.file(url=x, destfile=y,...), dl_links2, dl_paths2 ,MoreArgs = list(mode="wb") )
#mapply(function(x,y,...) download.file(url=x, destfile=y,...), dl_links3, dl_paths3 ,MoreArgs = list(mode="wb") )

#read scraped data
readin<-cbind(rbind(expand.grid(yrs,grp),expand.grid(yrs2,grp),expand.grid(yrs3,grp)),
              c(dl_paths,dl_paths2,dl_paths3),stringsAsFactors=FALSE)

for (j in 1:nrow(readin)){
  yr<-substr(readin[j,1],3,4)
  var<-substr(readin[j,2],1,3)
  nm<-paste0(var,yr)
  assign(nm,read_excel(readin[j,3], col_names = TRUE, skip=2))
}

# source in stubfile & use for filtering, etc. below
stubpath<-file.path(getwd(),"stubdata")
stubfn<-dir(stubpath,"*.txt")

stubfiles0<-lapply(stubfn,function(x) read_table(file.path(stubpath,x), skip=1,
                                     col_names=c("type","level","title",
                                                 "var_ucc","source","factor",
                                                 "group")))

# stubfiles0[[1]] # some stubfiles not formatted correctly & reading in improperly
#stubfile0<-read_table(file.path(stubpath,stubfn), skip=1,
#                      col_names=c("type","level","title","var_ucc","source","factor","group"))

#get wrapped rows, append X3 in wrapped row to X3 in previous row 
stubfile0<-stubfiles0[[4]]
stub_rw<-1:nrow(stubfile0)
w2<-stub_rw[stubfile0$type!=1] #wrap rows
w1<-w2-1 # row above wrap row

# correct wrapped title
stubfile0[w1,"title"]<-paste(unlist(stubfile0[w1,"title"]),unlist(stubfile0[w2,"title"]))

## subset data (drop wrap rows, title rows, and ASSET and ADDENDA group),
# create new line variable
stubfile<-filter(stubfile0, type==1, source != "T",
          group %in% c("CUCHARS", "FOOD", "EXPEND", "INCOME")) %>% 
          select(-c(type,factor)) %>% 
          mutate(level=if_else(group=='INCOME',as.integer(level-1),level),
                                lev1=if_else(level==1,title,NULL),
                                lev2=if_else(level==2,title,NULL),
                                lev3=if_else(level==3,title,NULL)) %>%
          fill(lev1:lev3) %>% 
          mutate(lev2=if_else(level!=1,lev2,NULL),lev3=if_else(level>=3,lev3,NULL)) 

stubfilekp<-filter(stubfile,group!="CUCHARS",group!="INCOME"&level<=3|group=="INCOME"&level<=1)
kp <-stubfilekp %>% select(title) %>% flatten() %>% unlist()

# fix column names due to nesting in excel file for (cus)ize , (edu)cat, (rac)e, 
#  housing (ten)ure and area

fixnames<-function(ds){
  nmfx<-grep("X",names(ds))
  chkseq<-seq(min(nmfx)-1,max(nmfx))
  suprow<-chkseq[!chkseq %in% nmfx]
  
  names(ds)[nmfx]<-""
  nm<-names(ds)
  
  for (i in 4:length(nm)){
    if(nm[i]=="") {
      nm[i]<-nm[i-1]
    }
  }
  
newnm<-gsub(": NA","",paste(nm,ds[1,],sep=": "))
newnm
}

# fix educat, cusize, and race colnames
for (i in c("edu","cus","rac")){
  for (j in 11:15){
    dsc<-paste0(i,j)
    ds<-get(dsc) #temp dataset
    names(ds)<-fixnames(ds)
    assign(dsc,ds) #assign back to dataset
  }
}  

#make housing tenure and type of are datasets from each ten dataset
for (j in 11:15){
  assign(paste0("hou",j), select( get(paste0("ten",j)) ,1:6 ))
  assign(paste0("toa",j), select( get(paste0("ten",j)) ,1:2,7:10 ))
}

#function to help w/ name fix
prefixname<-function(ds){
  names(ds)[-c(1,2)] <-ds[1,-c(1,2)]
  names(ds)[is.na(names(ds))]<-"X"
names(ds)
}

# fix type of area and housing tenure file colnames
for (i in c("toa","hou")){
  for (j in 11:15){
    dsc<-paste0(i,j)
    ds<-get(dsc) #temp dataset
    names(ds)<-prefixname(ds) 
    ds<-ds[-1,]
    names(ds)<-fixnames(ds)
    assign(dsc,ds) #assign back to dataset
  }
}  

#drop tenure datasets and temp ds dataset
rm(list=ls()[grep("ten[0-9]{2}",ls())])
rm(ds)

#hotfix 2015 filenames to correct all filenames

#race
names(rac15)<-gsub("All\nconsumer\nunits","All CUs",names(rac15)) %>% gsub("\n","",.) %>%
gsub("White and all other races, and Asian","White, Asian &\n all other",.) %>%
gsub("orAfr","or Afr",.) %>% gsub("andall","& other",.) %>%
gsub("otherraces ","",.) %>% gsub(" a/","",.)

#education
names(edu15)<-gsub("All\nconsumer\nunits","All CUs",names(edu15)) %>% gsub("\n"," ",.) %>%
  gsub("graduate","grad",.) %>% gsub("[hH]igh school","HS",.) %>% gsub("with","w/",.) %>%
  gsub("[cC]ollege grad","College",.) %>% 
    gsub("Master's, professional, doctoral degree","Graduate degree",.)

#CU size
names(cus15)<-gsub("All\nconsumer\nunits","All CUs",names(cus15)) %>% gsub("\n"," ",.) %>%
  gsub("people$","ppl",.)

#age
names(age15)<-gsub("All\nconsumer\nunits","All CUs",names(age15)) %>% gsub("\n"," ",.) %>%
  gsub("Under 25 years","24 years and younger",.)

#housing tenure
names(hou15)<- gsub("All\nconsumer\nunits","All CUs",names(hou15)) %>% gsub("\n"," ",.) %>%
   gsub(" Home- owner "," ",.) %>% gsub("without","w/o",.) %>% gsub("with","w/",.)

#type of area
names(toa15)<-gsub("All\nconsumer\nunits","All CUs",names(toa15)) %>% gsub("\n"," ",.)

#income
names(qui15)<-gsub("All\nconsumer\nunits","All CUs",names(qui15)) %>% gsub("\n"," ",.) %>%
  gsub(" percent","%",.)

#region
names(reg15)<-gsub("All\nconsumer\nunits","All CUs",names(reg15))

#ensure datasets have same names across years
cuchars<-c("age","cus","edu","hou","qui","rac","reg","toa")
for (i in cuchars) {
    lastdsc<-paste0(i,"15")
    lastds<-get(lastdsc)
  for (j in 11:14){
    dsc<-paste0(i,j)
    ds<-get(dsc) #temp dataset
    names(ds)<-names(lastds)
    assign(dsc,ds) #assign back to dataset
  }
}

# print names to check
# lapply(ls()[grep("[a-z]{3}[0-9]{2}",ls())], function(x) names(get(x)))

# function to clean datasets
# remove lines above 50 (CU chars) and below 573 (addt'l tax, income)
# clean unneeded rows and add labels
# gather cleaned data, separate item name from stat (using space before last word)
# keep selected values, tidy, merge on stubfile
clndat<-function(cesdat){
  nm<-cesdat
  firstrow<-which(get(cesdat)$Item == "Average annual expenditures")
  lastrow<-which(get(cesdat)$Item == "Addenda:")-1
  outdat<-slice(get(cesdat),firstrow:lastrow) %>%  
    mutate(t1=is.na(.[,1]),ldt1=lead(t1),lagt1=lag(t1),
         t2=is.na(.[,2]),ldt2=lead(t2),lagt2=lag(t2),
         drop=ldt1 & lagt1 & t2 & ldt2 & lagt2,
         lonerow=!t2 & ldt2 & lagt2 %in% c(NA,T),
         lab=!t1 & t2, needlab=!t2 & !lonerow,
         label=if_else(lab,Item,NULL)) %>%
  fill(label) %>%
  filter(!drop,!t2) %>%
  mutate(Item=if_else(needlab,paste(label,Item),Item)) %>%
  select(names(get(cesdat))) %>%
  gather("cugrp","val",-1) %>%
  separate(Item,c("cat1","stat"), sep="[ ](?=[^ ]+$)") %>% 
  filter(cat1 %in% kp) %>%
  spread(stat,val) %>%  mutate(in_nm = nm) %>% 
  separate(in_nm,c("cuchar","yr"),sep=3) %>%  
      mutate(cuchar=if_else(cugrp=="All CUs","all",cuchar)) %>%
      left_join(y=stubfilekp,by=c("cat1"="title")) %>%
  mutate(yr=as.integer(paste0("20",yr)), Mean=as.numeric(Mean),SE=as.numeric(SE))  
}


#for 2011-2012 change "Health care" to "Healthcare" to match other files
for (i in cuchars) {
  for (j in 11:12){
    dsc<-paste0(i,j)
    ds<-get(dsc) #temp dataset
    ds<-mutate(ds,Item=if_else(Item=="Health care","Healthcare",Item))
    assign(dsc,ds) #assign back to dataset
  }
}

#change "Personal taxes (missing values not imputed)" (2011-2012)
# and "Personal taxes (contains some imputed values)" (2013-2015) to  "Personal taxes"
for (i in cuchars) {
  for (j in 11:15){
    dsc<-paste0(i,j)
    ds<-get(dsc) #temp dataset
    ds<-mutate(ds,Item=if_else(str_sub(Item,1,14)=="Personal taxes","Personal taxes",Item))
    assign(dsc,ds) #assign back to dataset
  }
}

# also change "Personal taxes (contains some imputed values)" to "Personal taxes"
# in stubfile
stubfilekp<-mutate(stubfilekp,
                   title=if_else(str_sub(title,1,14)=="Personal taxes","Personal taxes",title))

# loop to clean all data
for (i in cuchars) {
  for (j in 11:15){
    dsn<-paste0(str_sub(i,1,2),j)
    ds<-clndat( paste0(i,j) )
    assign(dsn,ds) #assign back to dataset
  }
}

#list of datasets to bind
bd<-as.list(ls()[grep("^[a-z]{2}[0-9]{2}$",ls())])
bdl<-lapply(bd,get)

#bind and cache
com_plt<-bind_rows(bdl) %>% arrange(yr,cuchar) %>% distinct()

#check cuchars  
# select(com_plt, cuchar) %>% table()

save(com_plt,file=file.path(wd,"cache","com_plt.RData"))
