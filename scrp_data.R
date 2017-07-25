#set path depending on user
user<-system2("whoami", stdout=TRUE)
wd<-switch(user,
           nathan=file.path("~/Dropbox/njames/school/PhD/misc/gss_2017"),
           jacquelynneal=file.path("~jacquelynneal/Desktop/gss_2017"))
setwd(wd)

library(tidyverse)
library(readxl)

#download data

url <- "https://www.bls.gov/cex/2015/combined/age.xlsx"
download.file(url, file.path(wd,"scraped","2015age.xlsx"), mode="wb")

url2 <- "https://www.bls.gov/cex/2014/combined/age.xlsx"
download.file(url2, file.path(wd,"scraped","2014age.xlsx"), mode="wb")

url3 <- "https://www.bls.gov/cex/2015/combined/income.xlsx"
download.file(url3, file.path(wd,"scraped","2015income.xlsx"), mode="wb")

yrs<-c("2015","2014","2013")
grp<-c("age","income","region")

links<-as.vector(outer(yrs,grp, 
                FUN=function(x,y) paste0("https://www.bls.gov/cex/",x,"/combined/",y,".xlsx")))

paste0("https://www.bls.gov/cex/",yrs,"/combined/",grp,".xlsx")

#read scraped data

age14<-read_excel(file.path(wd,"scraped","2014age.xlsx"), col_names = TRUE, skip=2)
age15<-read_excel(file.path(wd,"scraped","2015age.xlsx"), col_names = TRUE, skip=2)
inc15<-read_excel(file.path(wd,"scraped","2015income.xlsx"), col_names = TRUE, skip=2)

#! source in stubfile & use for filtering, etc. below
filter(stubfile,group!="CUCHARS",level<=2)
filter(stubfile,group!="CUCHARS",level<=3)

kp<-c("Average annual expenditures","Food","Alcoholic beverages",
      "Housing", "Apparel and services", 
      "Transportation", "Healthcare","Entertainment","Personal care products and services",
      "Reading","Education","Tobacco products and smoking supplies",
      "Miscellaneous","Cash contributions","Personal insurance and pensions",
      "Money income before taxes","Personal taxes (contains some imputed values)",
      "Income after taxes")

#clean
# remove lines above 50 (CU chars) and below 573 (addt'l tax, income)
# clean unneeded rows and add labels
# gather cleaned data, separate item name from stat (using space before last word)
# keep selected values, tidy
clndat<-function(cesdat){
  nm<-deparse(substitute(cesdat))
outdat<-slice(cesdat,51:573) %>% 
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
  mutate(cuchar=if_else(cugrp=="All\nconsumer\nunits","all",cuchar))
 # mutate(cuchar=if_else(cugrp=="All consumer units","all",cuchar))
}

# clean data
a15<-clndat(age15)
i15<-clndat(inc15)

  
a14<-clndat(age14)

ls()[grep("^[a-z]{1}[0-9]{2}$",ls())]

#bind 
com_plt<-bind_rows(a15,i15,a14) %>% arrange(yr,cuchar) %>% distinct()

save(com_plt,file=file.path(wd,"cache","com_plt.RData"))
  
ggplot(filter(com_plt,cat1 %in% c("Food","Housing")), aes(cugrp,yr,cat1,Mean))+
  geom_point(mapping=aes(x=yr,y=Mean))

