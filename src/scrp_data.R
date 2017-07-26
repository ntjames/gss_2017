#set path depending on user
user<-system2("whoami", stdout=TRUE)
wd<-switch(user,
           nathan=file.path("~/Dropbox/njames/school/PhD/misc/gss_2017"),
           jacquelynneal=file.path("~jacquelynneal/Desktop/gss_2017"))
setwd(wd)

library(tidyverse)
library(readxl)

yrs<-c("2015","2014","2013")
grp<-c("age","income","region")

#download data (only need to dl once)

#url <- "https://www.bls.gov/cex/2015/combined/age.xlsx"
#download.file(url, file.path(wd,"scraped","2015age.xlsx"), mode="wb")

dl_links<-as.vector(outer(yrs,grp, 
                FUN=function(x,y) paste0("https://www.bls.gov/cex/",x,"/combined/",y,".xlsx")))

dl_paths<-as.vector(outer(yrs,grp, FUN=function(x,y) file.path(wd,"scraped",paste0(x,y,".xlsx") ) ))
                
#mapply(function(x,y,...) download.file(url=x, destfile=y,...), dl_links, dl_paths ,MoreArgs = list(mode="wb") )

#read scraped data
readin<-cbind(expand.grid(yrs,grp),dl_paths,stringsAsFactors=FALSE)

for (j in 1:nrow(readin)){
  yr<-substr(readin[j,1],3,4)
  var<-substr(readin[j,2],1,3)
  nm<-paste0(var,yr)
  assign(nm,read_excel(readin[j,3], col_names = TRUE, skip=2))
}

# source in stubfile & use for filtering, etc. below
stubpath<-file.path(getwd(),"stubdata")
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
          select(-c(type,factor)) %>% 
          mutate(level=if_else(group=='INCOME',as.integer(level-1),level),
                                lev1=if_else(level==1,title,NULL),
                                lev2=if_else(level==2,title,NULL),
                                lev3=if_else(level==3,title,NULL)) %>%
          fill(lev1:lev3) %>% 
          mutate(lev2=if_else(level!=1,lev2,NULL),lev3=if_else(level>=3,lev3,NULL)) 

filter(stubfile,group!="CUCHARS",level<=2)
filter(stubfile,group!="CUCHARS",level<=3)

stubfilekp<-filter(stubfile,group!="CUCHARS",group!="INCOME"&level<=3|group=="INCOME"&level<=1)
kp <-stubfilekp %>% select(title) %>% flatten() %>% unlist()

din<-data.frame(select(a15,lev2,lev3))
ulevs<-unique(a15$lev2)
sublist<-lapply(ulevs, function(x) unique(subset(din,lev2==x,select=lev3,drop=T)))
names(sublist)<-ulevs

#add category before name i.e. Age: Under 25 years

#ensure datasets have same names across years
names(age13)<-names(age14)<-names(age15) #<-gsub("\n"," ",names(age15))
names(inc13)<-names(inc14)<-names(inc15) #<-gsub("\n"," ",names(inc15))
names(reg13)<-names(reg14)<-names(reg15) #<-gsub("\n"," ",names(reg15))

# function to clean datasets
# remove lines above 50 (CU chars) and below 573 (addt'l tax, income)
# clean unneeded rows and add labels
# gather cleaned data, separate item name from stat (using space before last word)
# keep selected values, tidy, merge on stubfile
clndat<-function(cesdat){
  nm<-deparse(substitute(cesdat))
  outdat<-slice(cesdat,51:643) %>% 
    mutate(t1=is.na(.[,1]),ldt1=lead(t1),lagt1=lag(t1),
         t2=is.na(.[,2]),ldt2=lead(t2),lagt2=lag(t2),
         drop=ldt1 & lagt1 & t2 & ldt2 & lagt2,
         lonerow=!t2 & ldt2 & lagt2 %in% c(NA,T),
         lab=!t1 & t2, needlab=!t2 & !lonerow,
         label=if_else(lab,Item,NULL)) %>%
  fill(label) %>%
  filter(!drop,!t2) %>%
  mutate(Item=if_else(needlab,paste(label,Item),Item)) %>%
  select(names(cesdat)) %>%
  gather("cugrp","val",-1) %>%
  separate(Item,c("cat1","stat"), sep="[ ](?=[^ ]+$)") %>% 
  filter(cat1 %in% kp) %>%
  spread(stat,val) %>%  mutate(in_nm = nm) %>%
  separate(in_nm,c("cuchar","yr"),sep=3) %>%
  mutate(cuchar=if_else(cugrp=="All\nconsumer\nunits","all",cuchar)) %>%
  left_join(y=stubfilekp,by=c("cat1"="title")) %>%
  mutate(yr=as.integer(paste0("20",yr)), Mean=as.numeric(Mean),SE=as.numeric(SE))  
}

# clean data
a13<-clndat(age13)
i13<-clndat(inc13)
r13<-clndat(reg13)

a14<-clndat(age14)
i14<-clndat(inc14)
r14<-clndat(reg14)

a15<-clndat(age15)
i15<-clndat(inc15)
r15<-clndat(reg15)

#list of datasets to bind
bd<-as.list(ls()[grep("^[a-z]{1}[0-9]{2}$",ls())])
bdl<-lapply(bd,get)

#bind and cache
com_plt<-bind_rows(bdl) %>% arrange(yr,cuchar) %>% distinct()

save(com_plt,file=file.path(wd,"cache","com_plt.RData"))
  
#ggplot(filter(com_plt,cat1 %in% c("Food","Housing")), aes(cugrp,yr,cat1,Mean))+
#  geom_point(mapping=aes(x=yr,y=Mean))

