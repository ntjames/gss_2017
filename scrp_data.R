#set path depending on user
user<-system2("whoami", stdout=TRUE)
wd<-switch(user,
           nathan=file.path("~/Dropbox/njames/school/PhD/misc/gss_2017"),
           jacquelynneal=file.path("~jacquelynneal/Desktop/gss_2017"))
setwd(wd)

library(tidyverse)
library(readxl)


url <- "https://www.bls.gov/cex/2015/combined/age.xlsx"
download.file(url, file.path(wd,"scraped","2015age.xlsx"), mode="wb")

url2 <- "https://www.bls.gov/cex/2014/combined/age.xlsx"
download.file(url2, file.path(wd,"scraped","2014age.xlsx"), mode="wb")

age14<-read_excel(file.path(wd,"scraped","2014age.xlsx"),col_names = TRUE, skip=2)
age15<-read_excel(file.path(wd,"scraped","2015age.xlsx"),col_names = TRUE, skip=2)

age_sml<-slice(age15,1:15)

#removelines below 573 (addt'l tax, income)
age_2<-slice(age15,1:573) %>% 
  mutate(t1=is.na(.[,1]),ldt1=lead(t1),lagt1=lag(t1),
         t2=is.na(.[,2]),ldt2=lead(t2),lagt2=lag(t2),
         drop=ldt1 & lagt1 & t2 & ldt2 & lagt2,
         lonerow=!t2 & ldt2 & lagt2 %in% c(NA,T),
         lab=!t1 & t2,
         needlab=!t2 & !lonerow,
         label=if_else(lab,Item,NULL)) %>%
  fill(label) %>%
  filter(!drop,!t2) %>%
  mutate(Item=if_else(needlab,paste(label,Item),Item)) %>%
  select(names(age15))
  
kp0<-slice(age_2, 1:31) %>% select(Item) %>%  flatten() %>% unlist()

kp1<-paste(c("Food","Food at home","Food away from home","Alcoholic beverages",
      "Housing",
      "Apparel and services",
      "Transportation",
      "Healthcare"),"Mean")

#add se?

kp<-c(kp0,kp1)

pltdat<-gather(age_2,"grp","val",-1)  %>% filter(Item %in% kp) %>% arrange(Item) %>% 
  spread(Item,val)

ggplot(data.frame(pltdat), aes(grp,Food.Mean))+geom_point()

