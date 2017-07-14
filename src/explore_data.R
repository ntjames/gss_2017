wd<-file.path("~/Dropbox/njames/school/PhD/misc/GSS_data_challenge/gss_2017")
setwd(wd)
library(ProjectTemplate)
load.project()
library(ggplot2)
library(plotly)
# https://plot.ly/r/

if(0){
#diary - removed in 01-A.R
fmld - chars, income, weight, summ exp for CU
memd - chars and income for each memb of CU
expd - detailed weekly expenditure by UCC
dtbd - detailed annual income file by UCC
dtid - CU imputed income file by UCC

#interview quarterly - report expenditures for prev 3 mo.
fmli - chars, income, weight, summ exp for CU
memi - chars and income for each memb of CU
mtbi - detailed monthly expenditure by UCC
itbi - CU monthly income by UCC
ntaxi - fed and state tax info
itii - CU monthly imputed income

#interview annual
expn - expenditure/non-expenditure from sections of 
quarterly survey - each release ~ 50 files
examples
cla - clothing and sewing
ihb - hospitalization and health ins
veq - vehicle maintenance and repair

#paradata 
fpar - data about survey, timing, record use
mchi - contact history between field rep and respondent, etc

nms<-ls()
#diary for 1st q (removed in 01-A.R)
nms[grep("d[0-9]{3}",nms)]
#nms[grep("d[0-9]",nms)]

#interview for 1st q
nms[grep("i[0-9]{3}",nms)]

#expn datasets
nms[grep("^[a-z]{3}[0-9]{2}",nms)]
}

if(0){
#monthly expenditure
#is there a codebook for UCC?
head(mtbi151x)
length(unique(mtbi151x$ucc))
hist(mtbi151x$cost)
ucc_sum<-by(mtbi151x$cost, mtbi151x$ucc, sum)

#ucc cats with highest expenditure
head(ucc_sum[order(-ucc_sum)])

#income
head(dtbd151)
subset(dtbd151,newid=="03111041")
with(subset(dtid151,impnum=="1"),hist(amount))

#weekly expenditure
#head(expd151)
#table(expd151$expnsqdy)
}

#explore EXPN (annual expenditure) files
kp_id<-quote(c(qyear,newid,seqno,alcno))

# cla - clothing
head(cla15)
hist(cla15$clothxa)
cla15$rn<-1:nrow(cla15)

#sample 10000 records
smp<-sample(nrow(cla15),10000)

cla15sub<-subset(cla15,rn %in% smp,
                 select=c(eval(kp_id),clothya,clothmoa,clothxa))

#check NA
#cla15na<-subset(cla15,is.na(clothxa),
#                select=c(qyear,newid,seqno,alcno,clothya,clothmoa,clothxa))

#sum monthly cost by newid
cla15mn0<-aggregate(cla15sub$clothxa, by=list(cla15sub$newid,cla15sub$clothmoa),
          function(x) sum(x,na.rm=TRUE))

#median per month
cla15med<-aggregate(cla15mn0$x,list(cla15mn0$Group.2),median)

cla15avg<-aggregate(cla15mn0$x,list(cla15mn0$Group.2),mean)

cla15dat<-merge(cla15med,cla15avg,by="Group.1")

#plot median cost over months
plot_ly(cla15med,x=~Group.1, y=~x,type="scatter",mode="lines")

#median and mean (example of multiple traces)
plot_ly(cla15dat,x=~Group.1, y=~x.x,type="scatter",mode="lines") %>% 
  add_trace(y=~x.y,type="scatter")

#boxplots
#ggplot(data=cla15mn0,mapping=aes(x=Group.2,y=x))+geom_boxplot()+scale_y_log10()

#plot_ly(cla15mn0,x=~Group.2, y=~x,type="box") 


# rnt - rented living quarters
head(rnt15)
names(rnt15)
rnt15$rn<-1:nrow(rnt15)

#sample 1000 records
smp<-sample(nrow(rnt15),1000)

rnt15sub<-subset(rnt15,rn %in% smp,
                 select=c(eval(kp_id),qrt3mcmx))

#sum monthly cost by newid ??

#median per month?? only have per ref period of 3 mon
rnt15med<-aggregate(rnt15sub$qrt3mcmx,list(rnt15sub$qyear),
                    function(x) median(x,na.rm=TRUE))

# rtv - rented vehicles

# lsd - leased vehicles

# ovb - owned vehicles

# veq - vehicle maintenance

# vlr - vehicle license, reg., inspection

# vot - vehicle operating expenses


vot15$rn<-1:nrow(vot15)

#sample records
smp<-sample(nrow(vot15),10000)

vot15sub<-subset(vot15,rn %in% smp,
                 select=c(eval(kp_id),jgasoxqv,jdiesxqv))

#mean per quarter
vot15mn<-aggregate(vot15sub[,c("jgasoxqv","jdiesxqv")],
                   list(vot15sub$qyear),
                   function(x) mean(x,na.rm=TRUE))

plot_ly(vot15mn,x=~Group.1, y=~jgasoxqv,type="scatter",mode="lines",
        name="fuel - gasoline") %>%
        layout(xaxis = list(title="quarter"), 
         yaxis = list(title="expenditure ($)")) 

vot15mn_g<-gather(vot15mn,Group.1)


## tidyverse
v15<-as_tibble(vot15)
v15m<-filter(v15,rn %in% smp) %>% select(.,c("jgasoxqv","jdiesxqv","qyear")) %>% 
  group_by(qyear) %>% summarize_all(funs(mean(.,na.rm=TRUE)))

v15m
v15_s<-gather(v15m,subcat,val,-qyear)

plot_ly(v15_s,x=~qyear, y=~val, split=~subcat,type="scatter",mode="lines") %>%
  layout(xaxis = list(title="quarter"), 
         yaxis = list(title="expenditure ($)")) 


## need flags? (i.e. jgas_xqv)
v15_s<-filter(v15,rn %in% smp) %>% select(.,ends_with("qv"),ends_with("x"),-contains("_"),"qyear") %>% 
  group_by(qyear) %>% summarize_all(funs(mean(.,na.rm=TRUE))) %>% gather(v15m,subcat,val,-qyear)

plot_ly(v15_s,x=~qyear, y=~val, split=~subcat,type="scatter",mode="lines") %>%
  layout(xaxis = list(title="quarter"), 
         yaxis = list(title="expenditure ($)")) 

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
