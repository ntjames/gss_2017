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

## Process data for 2012 - 2015 ##
yrs<-c("2015","2014","2013")
yrs2<-c("2012")
grp<-c("age","race","educat","cusize","quintile","region","tenure")

# CESD links and local paths
dl_links<-as.vector(outer(yrs,grp, 
                FUN=function(x,y) paste0("https://www.bls.gov/cex/",x,"/combined/",y,".xlsx")))
dl_paths<-as.vector(outer(yrs,grp, FUN=function(x,y) file.path(wd,"scraped",paste0(x,y,".xlsx") ) ))

dl_links2<-as.vector(outer(yrs2,grp, 
                          FUN=function(x,y) paste0("https://www.bls.gov/cex/",x,"/combined/",y,".xls")))
dl_paths2<-as.vector(outer(yrs2,grp, FUN=function(x,y) file.path(wd,"scraped",paste0(x,y,".xls") ) ))

#download data (only need to dl once)
#mapply(function(x,y,...) download.file(url=x, destfile=y,...), dl_links, dl_paths ,MoreArgs = list(mode="wb") )
#mapply(function(x,y,...) download.file(url=x, destfile=y,...), dl_links2, dl_paths2 ,MoreArgs = list(mode="wb") )

#read scraped data
readin<-cbind(rbind(expand.grid(yrs,grp),expand.grid(yrs2,grp)),
              c(dl_paths,dl_paths2),stringsAsFactors=FALSE)

for (j in 1:nrow(readin)){
  yr<-substr(readin[j,1],3,4)
  var<-substr(readin[j,2],1,3)
  nm<-paste0(var,yr)
  assign(nm,read_excel(readin[j,3], col_names = TRUE, skip=2))
}

# source in stubfile & use for filtering, etc. below
#! pull stub files from multiple years to get spellings, etc.
#! e.g. Health care in 2012 v Healthcare 2013-2015
stubpath<-file.path(getwd(),"stubdata")
stubfn<-dir(stubpath,"*.txt")

stubfiles0<-lapply(stubfn,function(x) read_table(file.path(stubpath,x), skip=1,
                                     col_names=c("type","level","title",
                                                 "var_ucc","source","factor",
                                                 "group")))

# stubfiles0[[1]] # first file, 2012 stub not formatted correctly, so reading in improperly
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

#filter(stubfile,group!="CUCHARS",level<=2)
#filter(stubfile,group!="CUCHARS",level<=3)

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
  for (j in 12:15){
    dsc<-paste0(i,j)
    ds<-get(dsc) #temp dataset
    names(ds)<-fixnames(ds)
    assign(dsc,ds) #assign back to dataset
  }
}  

#make housing tenure and type of are datasets from each ten dataset
for (j in 12:15){
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
  for (j in 12:15){
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
names(rac15)<-gsub("and all","&",names(rac15))
names(rac15)<-gsub(" a/","",names(rac15))

#ensure datasets have same names across years
cuchars<-c("age","cus","edu","hou","qui","rac","reg","toa")
for (i in cuchars) {
    lastdsc<-paste0(i,"15")
    lastds<-get(lastdsc)
  for (j in 12:14){
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
  # nm<-deparse(substitute(cesdat))
  nm<-cesdat
  outdat<-slice(get(cesdat),51:643) %>%  ##!! determine stopping point w/o hardcode? (Addenda?) 
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
  spread(stat,val) %>%  mutate(in_nm = nm) %>% #!
  separate(in_nm,c("cuchar","yr"),sep=3) %>%  #!
  mutate(cuchar=if_else(cugrp=="All\nconsumer\nunits","all",cuchar)) %>%
  left_join(y=stubfilekp,by=c("cat1"="title")) %>%
  mutate(yr=as.integer(paste0("20",yr)), Mean=as.numeric(Mean),SE=as.numeric(SE))  
}

#a12<-clndat("age12")

#for 2012 change "Health care" to "Healthcare" to match other files
for (i in cuchars) {
    dsc<-paste0(i,"12")
    ds<-get(dsc) #temp dataset
    ds<-mutate(ds,Item=if_else(Item=="Health care","Healthcare",Item))
    assign(dsc,ds) #assign back to dataset
}

#change "Personal taxes (missing values not imputed)" (2012)
# and "Personal taxes (contains some imputed values)" (2013-2015) to  "Personal taxes"
for (i in cuchars) {
  for (j in 12:15){
    dsc<-paste0(i,j)
    ds<-get(dsc) #temp dataset
    ds<-mutate(ds,Item=if_else(str_sub(Item,1,14)=="Personal taxes","Personal taxes",Item))
    assign(dsc,ds) #assign back to dataset
  }
}

#filter(age12,Item=="Personal taxes")
#filter(hou14,Item=="Personal taxes")

# also change "Personal taxes (contains some imputed values)" to "Personal taxes"
# in stubfile
stubfilekp<-mutate(stubfilekp,
                   title=if_else(str_sub(title,1,14)=="Personal taxes","Personal taxes",title))

# clean data
for (i in cuchars) {
  for (j in 12:15){
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

#check cuchars are correct 
# select(com_plt, cuchar) %>% table()

save(com_plt,file=file.path(wd,"cache","com_plt.RData"))
  
#ggplot(filter(com_plt,cat1 %in% c("Food","Housing")), aes(cugrp,yr,cat1,Mean))+
#  geom_point(mapping=aes(x=yr,y=Mean))





if (0) {
## Process data before 2012 ##

# yrs3<-c("2011","2010")

#dl_links3m<-as.vector(outer(yrs3,grp, 
#                           FUN=function(x,y) paste0("https://www.bls.gov/cex/",x,"/Standard/",y,".xls")))
#dl_paths3m<-as.vector(outer(yrs3,grp, FUN=function(x,y) file.path(wd,"scraped",paste0(x,y,"_mn.xls") ) ))
#mapply(function(x,y,...) download.file(url=x, destfile=y,...), dl_links3m, dl_paths3m ,MoreArgs = list(mode="wb") )

#clean pre- 2012

outdat<-slice(age11,43:185) %>%  ##!! need way to determine stopping point w/o hardcode (Addenda?) 
  mutate(t1=is.na(.[,1]),ldt1=lead(t1),lagt1=lag(t1),
         t2=is.na(.[,2]),ldt2=lead(t2),lagt2=lag(t2),
         drop=ldt1 & lagt1 & t2 & ldt2 & lagt2,
         lonerow=!t2 & ldt2 & lagt2 %in% c(NA,T))  %>%
  filter(!drop,!t2) %>%
  mutate(Item=paste(Item,"Mean")) %>%
  select(names(age11)) %>% #!
  gather("cugrp","val",-1) %>%
  separate(Item,c("cat1","stat"), sep="[ ](?=[^ ]+$)") %>% 
  filter(cat1 %in% kp) %>%
  spread(stat,val) %>%  mutate(in_nm = "age11") %>% #!
  separate(in_nm,c("cuchar","yr"),sep=3) %>%
  mutate(cuchar=if_else(cugrp=="All\nconsumer\nunits","all",cuchar)) %>%
  left_join(y=stubfilekp,by=c("cat1"="title")) 

# ! need to read and add on SE
#  %>% mutate(yr=as.integer(paste0("20",yr)), Mean=as.numeric(Mean),SE=as.numeric(SE))  
}








## scratch below

# nmfx<-grep("X",names(edu15))
# chkseq<-seq(min(nmfx)-1,max(nmfx))
# suprow<-chkseq[!chkseq %in% nmfx]
# 
# names(edu15)[nmfx]<-""
# 
# nm<-names(edu15)
# 
# for (i in 4:length(nm)){
#   if(nm[i]=="") {
#     nm[i]<-nm[i-1]
#   }
# }
# 
# newnm<-gsub(": NA","",paste(nm,edu15[1,],sep=": "))

# din<-data.frame(select(a15,lev2,lev3))
# ulevs<-unique(a15$lev2)
# sublist<-lapply(ulevs, function(x) unique(subset(din,lev2==x,select=lev3,drop=T)))
# names(sublist)<-ulevs

# add category before name i.e. Age: Under 25 years ?? dropped income because classification changed

#names(inc12)<-names(inc13)<-names(inc14)<-names(inc15) #<-gsub("\n"," ",names(inc15))

#hou12<-select(ten12,1:6)
#add_ds<-ls()[grep("hou|ten",ls())]


#names(toa12)<-prefixname(toa12)
#toa12<-toa12[-1,]
#fixnames(toa12)

#names(hou12)<-prefixname(hou12)
#hou12<-hou12[-1,]
#fixnames(hou12)

# for (i in add_ds){
#   names(i)[-c(1,2)] <-i[1,-c(1,2)]
#   names(i)[is.na(names(i))]<-"X"
#   i<-i[-1,]
# }
# 
# prefixname(toa12)

#toa12<-select(ten12,1:2,7:10)

# names(toa12)[-c(1,2)] <-toa12[1,-c(1,2)]
# toa12<-toa12[-1,]
# names(toa12)[is.na(names(toa12))]<-"X"
# 
# fixnames(toa12)

# # function/loop ?
# a12<-clndat(age12)
# 
# i12<-clndat(inc12)
# r12<-clndat(reg12)
# 
# a13<-clndat(age13)
# i13<-clndat(inc13)
# r13<-clndat(reg13)
# 
# a14<-clndat(age14)
# i14<-clndat(inc14)
# r14<-clndat(reg14)
# 
# a15<-clndat(age15)
# i15<-clndat(inc15)
# r15<-clndat(reg15)
# 
# #check diff
# a12cats<-select(a12,cat1) %>% distinct() 
# a13cats<-select(a13,cat1) %>% distinct() 
# a14cats<-select(a14,cat1) %>% distinct() 
# 
# intersect(a12cats,a13cats)
# setdiff(a12cats,a13cats)
# setdiff(a13cats,a12cats)
# 
# intersect(a12cats,a14cats)
# setdiff(a12cats,a14cats)
# setdiff(a14cats,a12cats)
# 
# intersect(a13cats,a14cats)
# setdiff(a13cats,a14cats)
# setdiff(a14cats,a13cats)

