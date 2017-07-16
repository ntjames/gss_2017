wd<-file.path("~/Dropbox/njames/school/PhD/misc/GSS_data_challenge/gss_2017")
setwd(wd)
library(ProjectTemplate)
load.project()

library(tidyverse)
library(plotly)
# https://plot.ly/r/

#explore EXPN (annual expenditure) files
kp_id<-quote(c(qyear,newid,seqno,alcno))

# cla - clothing

#check NA
#cla15na<-subset(cla15,is.na(clothxa),
#                select=c(qyear,newid,seqno,alcno,clothya,clothmoa,clothxa))

#sum monthly cost by newid
# cla15mn0<-aggregate(cla15sub$clothxa, by=list(cla15sub$newid,cla15sub$clothmoa),
          function(x) sum(x,na.rm=TRUE))

#median per month
# cla15med<-aggregate(cla15mn0$x,list(cla15mn0$Group.2),median)
# cla15avg<-aggregate(cla15mn0$x,list(cla15mn0$Group.2),mean)

#sample records
cla15$rn<-1:nrow(cla15)
smp<-sample(nrow(cla15),10000)

## tidyverse
kp_id<-c("qyear","newid","seqno","alcno")

ca15<-as_tibble(cla15)

##!! need to sum over newid, then average by quarter??
ca15_s<-filter(ca15,rn %in% smp) %>% select("clothya","clothxa","clothmoa",kp_id) %>% 
  group_by(qyear) %>% summarize_at("clothxa",funs(mean(.,na.rm=TRUE))) %>% 
  gather(subcat,val,-qyear)

#filter(ca15,rn %in% smp) %>% select(.,contains("clothx"),-contains("_"),"qyear") %>% 
#  group_by(qyear) %>% summarize_all(funs(mean(.,na.rm=TRUE),median(.,na.rm=TRUE) )) 


# rnt - rented living quarters
#head(rnt15)
names(rnt15)
rnt15$rn<-1:nrow(rnt15)

#sample 1000 records
smp<-sample(nrow(rnt15),1000)

#sum monthly cost by newid ??

#median per month?? only have per ref period of 3 mon
#rnt15med<-aggregate(rnt15sub$qrt3mcmx,list(rnt15sub$qyear),
#                    function(x) median(x,na.rm=TRUE))

rt15<-as_tibble(rnt15)

##!! need to sum over newid, then average by quarter??
rt15_s<-filter(rt15,rn %in% smp) %>% select("qrt3mcmx",kp_id) %>% 
  group_by(qyear) %>% summarize_at("qrt3mcmx",funs(mean(.,na.rm=TRUE))) %>% 
  gather(subcat,val,-qyear)

# rtv - rented vehicles

# lsd - leased vehicles

# ovb - owned vehicles

# veq - vehicle maintenance
veq15$rn<-1:nrow(veq15)
smp<-sample(nrow(veq15),10000)

## tidyverse
vq15<-as_tibble(veq15)

vq15_s<-filter(vq15,rn %in% smp) %>% select(.,ends_with("x"),-contains("_"),"qyear") %>% 
  group_by(qyear) %>% summarize_all(funs(mean(.,na.rm=TRUE))) %>% gather(subcat,val,-qyear)


# filter(vq15,rn %in% smp) %>% select(.,ends_with("x"),-contains("_"),"qyear") %>% 
#   group_by(qyear) %>% summarize_all(funs(mean(.,na.rm=TRUE),median(.,na.rm=TRUE) )) %>% 
#   gather(subcat,val,-qyear)

plot_ly(vq15_s,x=~qyear, y=~val, split=~subcat,type="scatter",mode="lines") %>%
  layout(xaxis = list(title="quarter"), 
         yaxis = list(title="expenditure ($)")) 

(vq15_s2<-mutate(vq15_s,subcat_f=factor(subcat,levels=c("vopexpx","qvopeqpx"),
                              labels=c("Total Cost","Amount less reimb"))))

# paste(letters[1:2],toupper(letters[1:2]),sep="=")
# unique(vq15_s2$subcat)
# levels(vq15_s2$subcat_f)


# vlr - vehicle license, reg., inspection

# vot - vehicle operating expenses

#sample records
vot15$rn<-1:nrow(vot15)
smp<-sample(nrow(vot15),10000)

## tidyverse
vo15<-as_tibble(vot15)

# v15m<-filter(v15,rn %in% smp) %>% select(.,c("jgasoxqv","jdiesxqv","qyear")) %>% 
#   group_by(qyear) %>% summarize_all(funs(mean(.,na.rm=TRUE)))
# 
# v15m
# v15_s<-gather(v15m,subcat,val,-qyear)
# 
# plot_ly(v15_s,x=~qyear, y=~val, split=~subcat,type="scatter",mode="lines") %>%
#   layout(xaxis = list(title="quarter"), 
#          yaxis = list(title="expenditure ($)")) 

## need flags? (i.e. jgas_xqv)
vo15_s<-filter(vo15,rn %in% smp) %>% select(.,ends_with("qv"),ends_with("x"),-contains("_"),"qyear") %>% 
  group_by(qyear) %>% summarize_all(funs(mean(.,na.rm=TRUE))) %>% gather(subcat,val,-qyear)

plot_ly(vo15_s,x=~qyear, y=~val, split=~subcat,type="scatter",mode="lines") %>%
  layout(xaxis = list(title="quarter"), 
         yaxis = list(title="expenditure ($)")) 

#### Merge datasets
expn<-bind_rows(mutate(ca15_s,cat="cla"),mutate(rt15_s,cat="rnt"),
  mutate(vq15_s,cat="veq"),mutate(vo15_s,cat="vot"))


# xpb - taxis, limousines, and mass transportation

# xpa - food & beverages
head(xpa15)
names(xpa15)
xpa15$rn<-1:nrow(xpa15)

#sample records
smp<-sample(nrow(xpa15),2500)

xpa15sub<-subset(xpa15,rn %in% smp,
                 select=c(eval(kp_id),jmkpurqv,jnonfdqv,jothstqv,jdineoqv,
                          jaloutqv,jalhomqv,jmkgrcqv))

#add fake classifier
xpa15sub$grp<-xpa15sub$jmkpurqv>median(xpa15sub$jmkpurqv)

xpa15sub$grp<-factor(xpa15sub$grp,labels=c("1-2 CU members","3+ CU members"))

#only for q1? add others?
# fmlisub<-subset(fmli151x,newid %in% xpa15sub$newid, select=c(newid, fam_size)) 

#sum by newid ??

#mean per quarter
xpa15mn<-aggregate(xpa15sub[,c("jdineoqv","jothstqv","jmkgrcqv")],
                    list(xpa15sub$qyear),
                    function(x) mean(x,na.rm=TRUE))

xpa15mn2<-aggregate(xpa15sub[,c("jdineoqv","jothstqv","jmkgrcqv")],
                    list(xpa15sub$qyear,xpa15sub$grp),
                    function(x) mean(x,na.rm=TRUE))

#save cleaned data to cache
xpa15mn

# plot food categories
plot_ly(xpa15mn,x=~Group.1, y=~jdineoqv,type="scatter",mode="lines",
        name="food - dining") %>% 
  add_trace(y=~jothstqv,type="scatter",name="food - oth") %>%
  add_trace(y=~jmkgrcqv,type="scatter",name="food - grocery")


plot_ly(xpa15mn,x=~Group.1, y=~jdineoqv,type="scatter",mode="lines",
        color=I("#66C2A5"), name="food - dining") %>% 
  layout(xaxis = list(title="quarter"), 
         yaxis = list(title="expenditure ($)"))  %>%
  add_trace(y=~jmkgrcqv,type="scatter",name="food - grocery",
            color=I("#FC8D62"))

plot_ly(xpa15mn2,x=~Group.1, y=~jdineoqv,type="scatter",mode="lines",
        color=I("#66C2A5"), name="food - dining",split=~Group.2) %>% 
  layout(xaxis = list(title="quarter"), 
         yaxis = list(title="expenditure ($)"))  %>%
  add_trace(y=~jmkgrcqv,type="scatter",name="food - grocery",
            color=I("#FC8D62"))

#frame?
plot_ly(xpa15mn2,x=~Group.1, y=~jdineoqv,type="scatter",mode="lines",
        color=I("#66C2A5"),
        name="food - dining",frame=~Group.2) %>% 
  add_trace(y=~jmkgrcqv,type="scatter",name="food - grocery",
            color=I("#FC8D62"))
