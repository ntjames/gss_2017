
#set path depending on user
user<-system2("whoami", stdout=TRUE)
wd<-switch(user,
       nathan=file.path("~/Dropbox/njames/school/PhD/misc/gss_2017"),
       jacquelynneal=file.path("~jacquelynneal/Desktop/gss_2017"))
setwd(wd)

#load ProjectTemplate ( http://projecttemplate.net )
#see gss_2017/config/global.dcf for config. options (e.g., libraries, preprocessing)
library(ProjectTemplate)

load.project()

# don't perform munging  i.e. keep all data (e.g. diary and individual EXPN)
# load.project(list('munging'=FALSE)) 

#currently use only CESD PUMD Interview files
if (0) {
  # FMLI - CU characteristics, income, and summary level expenditures
  # MEMI - member characteristics, income data
  # MTBI - monthly expenditures at UCC level
  # ITBI - income converted to monthly time frame
  # ITII - imputation variants of income converted to monthly time frame
  # NTAXI - fed. and state tax info
  # FPAR - CU level paradata about interview survey
  # MCHI - para data about each interview contact attempt
   
  # ISTUB - aggregation scheme used in published CES interview tables, 
  #         contain UCCs & abbreviated titles
   
  # EXPN - 43 detailed expenditure files
}


## STEP 1: READ IN THE STUB PARAMETER FILE AND CREATE FORMATS  
#?  need to modify for multiple files?
stubpath<-file.path(getwd(),"stubdata")
stubfn<-dir(stubpath)

#get wrapped rows, append X3 in wrapped row to X3 in previous row 
stubfile0<-read_table(file.path(stubpath,stubfn), skip=1,
               col_names=c("type","level","title","var_ucc","source","factor","group"))
stub_rw<-1:nrow(stubfile0)
w2<-stub_rw[stubfile0$type!=1] #wrap rows
w1<-w2-1 # row above wrap row

#head(stubfile0[sort(c(w1,w2)),])
stubfile0[w1,"title"]<-paste(unlist(stubfile0[w1,"title"]),unlist(stubfile0[w2,"title"]))
#head(stubfile0[sort(c(w1,w2)),])

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


## see around line 341 in interview mean and se.R

fmli_files<-ls()[grep("fmli",ls())]

#add file name as variable for each FMLI, then use separate to split into year and quarter
fmli<-bind_rows(lapply(fmli_files, function(x) mutate(get(x),fileyrqtr=str_sub(x,5,8)  ))) %>%
      separate(fileyrqtr,into=c("fileyear","fileqtr"),sep=2)

#x<-str_sub(fmli_files,5,8) 
#str_extract(x,"^[0-9]{2}")
#str_split(x,"^[0-9]{2}")

# delete all of the independent data frames from memory
rm(list=fmli_files)

# clear up RAM
gc()


# create a character vector containing 45 variable names (wtrep01, wtrep02, ... wtrep44 and finlwt21)
wtrep <- c( paste0( "wtrep" , str_pad( 1:44 , 2 , pad = "0" ) ) , "finlwt21")

# create a second character vector containing 45 variable names (repwt1, repwt2, .. repwt44, repwt45)
repwt <- paste0( "repwt" , 1:45 )

# create a third character vector that will be used to define which columns to keep
f.d.vars <- c( wtrep , "mo_scope" , "inclass" , "newid" , "src" )

# create a mo_scope variable in this large new family data frame
fmli <-  transform(fmli,
    mo_scope =
      # first quarter should be interview month minus 1
      ifelse( fileqtr %in% '1x' , as.numeric( qintrvmo ) - 1 ,
      # final quarter (i.e. 5th quarter or 1st of next yr) should be 4 minus the interview month
      ifelse( fileqtr %in% '1' , ( 4 - as.numeric( qintrvmo )  ) ,
      # all other quarters should have a 3
      3 ) ) ) %>%
    mutate(src="I")  %>% # the source column for family records should be "I" (interview) throughout
    select(f.d.vars) # keeping only the 45 wtrep columns, plus the additional four written above


# loop through the 45 wtrep variables in the newly-stacked fmly data frame..
for ( i in 1:45 ){
  
  # convert all columns to numeric
  fmli[ , wtrep[ i ] ] <- as.numeric( as.character( fmli[ , wtrep[ i ] ] ) )
  
  # replace all missings with zeroes
  fmli[ is.na( fmli[ , wtrep[ i ] ] ) , wtrep[ i ] ] <- 0
  
  # multiply by months in scope, then divide by 12 (months)
  fmli[ , repwt[ i ] ] <- ( fmli[ , wtrep[ i ] ] * fmli[ , "mo_scope" ] / 12 )
}


mtbi_files<-ls()[grep("mtbi",ls())]
itbi_files<-ls()[grep("itbi",ls())]

mtbi<-bind_rows(lapply(mtbi_files,get))  %>% select(newid, expname, ref_mo, ref_yr, ucc, cost)
itbi<-bind_rows(lapply(itbi_files,get))  %>% select(newid, refmo, refyr, ucc, value) %>%
  rename(cost=value, ref_mo=refmo,ref_yr=refyr)

#! limit the itbi and mtbi (interview) data frames to records from the current year
#!  with pubflags of two

#! keep only newid, ucc, cost

# stack the itbi and mtbi files
# create a new 'source' column, with "I" (interview) throughout
# multiply the 'COST' column by 4 whenever the UCC code is 710110
# add leading zeroes to UCC's to ensure proper format
expend<-bind_rows(mtbi,itbi) %>% 
  mutate(src="I",cost=if_else(ucc=="710110",cost*4,cost),ucc=str_pad(ucc, 6, pad="0")) %>%
  arrange(ucc)

# delete all of the independent data frames from memory
rm(list=c(mtbi_files,itbi_files))
rm(mtbi,itbi)

gc()


library(DBI)
library(RSQLite)

# designate a temporary file to store a temporary database
temp.db <- tempfile()

# create a new connection to the temporary database file (defined above)
db <- dbConnect( SQLite() , temp.db )

# store the family data frame in that database
dbWriteTable( db , 'fmli' , fmli , row.names = FALSE )

# create an index on the fmly table to drastically speed up future queries
dbSendQuery( db , "CREATE INDEX nsf ON fmli ( newid , src )" )

# store the expenditure data frame in that database as well
dbWriteTable( db , 'expend' , expend , row.names = FALSE )

# create an index on the expend table to drastically speed up future queries
dbSendQuery( db , "CREATE INDEX nse ON expend ( newid , src )" )

# create a character vector rcost1 - rcost45
rcost <- paste0( "rcost" , 1:45 )

# partially build the sql string, multiply each 'wtrep##' variable by 'COST' and rename it 'rcost##'
wtrep.cost <- paste0( "( b.COST * a." , wtrep , " ) as " , rcost , collapse = ", " )

# build the entire sql string..
sql.line <-
  paste(
    # creating a new 'pubfile' table, saving a few columns from each table
    "create table pubfile as select a.newid , a.inclass , b.src , b.ucc ," ,
    wtrep.cost ,
    # joining the family and expenditure tables on two fields
    "from fmli as a inner join expend as b on a.newid = b.newid AND a.src = b.src"
  )

# execute that sql query
dbSendQuery(
  db ,
  sql.line
)


# create an index on the pubfile table to drastically speed up future queries
dbSendQuery( db , "CREATE INDEX isu ON pubfile ( inclass , src , ucc )" )


# /***************************************************************************/
# /* STEP3: CALCULATE POPULATIONS                                            */
# /* ----------------------------------------------------------------------- */
# /*  SUM ALL 45 WEIGHT VARIABLES TO DERIVE REPLICATE POPULATIONS            */
# /*  FORMATS FOR CORRECT COLUMN CLASSIFICATIONS                             */
# /***************************************************************************/

# create a character vector containing 45 variable names (rpop1, rpop2, ... rpop44, rpop45)
rpop <- paste0( "rpop" , 1:45 )

# partially build the sql string, sum each 'repwt##' variable into 'rpop##'
rpop.sums <- paste( "sum( " , repwt , ") as " , rpop , collapse = ", " )

# partially build the sql string, sum each 'rcost##' variable into the same column name, 'rcost##'
rcost.sums <- paste( "sum( " , rcost , ") as " , rcost , collapse = ", " )

# create a total population sum (not grouping by 'INCLASS' -- instead assigning everyone to '10')
pop.all <- dbGetQuery( db , paste( "select 10 as inclass, src, " , rpop.sums , "from fmli group by src" ) )

# create a population sum, grouped by INCLASS (the income class variable)
pop.by <- dbGetQuery( db , paste( "select inclass, src," , rpop.sums , "from fmli group by inclass, src" ) )

# stack the overall and grouped-by population tables
pop <- rbind( pop.all , pop.by )

# /***************************************************************************/
# /* STEP4: CALCULATE WEIGHTED AGGREGATE EXPENDITURES                        */
# /* ----------------------------------------------------------------------- */
# /*  SUM THE 45 REPLICATE WEIGHTED EXPENDITURES TO DERIVE AGGREGATES/UCC    */
# /*  FORMATS FOR CORRECT COLUMN CLASSIFICATIONS                             */
# /***************************************************************************/


# create the right hand side of the aggregate expenditures table
aggright <-
  # use a sql query from the temporary database (.db) file
  dbGetQuery(
    db ,
    paste(
      # group by INCLASS (income class) and a few other variables
      "select inclass, src, ucc," ,
      rcost.sums ,
      "from pubfile group by src , inclass , ucc" ,
      # the 'union' command stacks the grouped data (above) with the overall data (below)
      "union" ,
      # do not group by INCLASS, instead assign everyone as an INCLASS of ten
      "select '10' as inclass, src , ucc," ,
      rcost.sums ,
      "from pubfile group by src , ucc"
    )
  )

# disconnect from the temporary database (.db) file
dbDisconnect( db )

# delete that temporary database file from the local disk
file.remove( temp.db )

gc()


# create three character vectors containing every combination of..

# the expenditure table's source variable
so <- names( table( expend$src ) )
# the expenditure table's UCC variable
uc <- names( table( expend$ucc ) )
# the family table's INCLASS (income class) variable
cl <- names( table( fmli[ , 'inclass' ] ) )
# add a '10' - overall category to the INCLASS variable
cl <- c( cl , "10" )

# now create a data frame containing every combination of every variable in the above three vectors
# (this matches the 'COMPLETETYPES' option in a sas proc summary call
aggleft <- expand.grid( so , uc , cl )

# name the columns in this new data frame appropriately
names( aggleft ) <- c( 'src' , 'ucc' , 'inclass' )

# perform a left-join, keeping all records in the left hand side, even ones without a match
agg <- merge( aggleft , aggright , all.x = TRUE )

# /***************************************************************************/
# /* STEP5: CALCULTATE MEAN EXPENDITURES                                     */
# /* ----------------------------------------------------------------------- */
# /* 1 READ IN POPULATIONS AND LOAD INTO MEMORY USING A 3 DIMENSIONAL ARRAY  */
# /*   POPULATIONS ARE ASSOCIATED BY INCLASS, SOURCE(t), AND REPLICATE(j)    */
# /* 2 READ IN AGGREGATE EXPENDITURES FROM AGG DATASET                       */
# /* 3 CALCULATE MEANS BY DIVIDING AGGREGATES BY CORRECT SOURCE POPULATIONS  */
# /*   EXPENDITURES SOURCED FROM DIARY ARE CALULATED USING DIARY POPULATIONS */
# /*   WHILE INTRVIEW EXPENDITURES USE INTERVIEW POPULATIONS                 */
# /* 4 SUM EXPENDITURE MEANS PER UCC INTO CORRECT LINE ITEM AGGREGATIONS     */
# /***************************************************************************/

library(sqldf)	
# create a character vector containing mean1, mean2, ... , mean45
means <- paste0( "mean" , 1:45 )

# merge the population and weighted aggregate data tables together
avgs1 <- merge( pop , agg )

# loop through all 45 weights..
for ( i in 1:45 ){
  # calculate the new 'mean##' variable by dividing the expenditure (rcost##) by the population (rpop##) variables
  avgs1[ , means[ i ] ] <- ( avgs1[ , rcost[ i ] ] / avgs1[ , rpop[ i ] ] )
  
  # convert all missing (NA) mean values to zeroes
  avgs1[ is.na( avgs1[ , means[ i ] ] ) , means[ i ] ] <- 0
}

# keep only a few columns, plus the 45 'mean##' columns
avgs1 <- avgs1[ , c( "src" , "inclass" , "ucc" , means ) ]

# partially build the sql string, sum each 'mean##' variable into the same column name, 'mean##'
avgs.sums <- paste( "sum( " , means , ") as " , means , collapse = ", " )

# merge on the 'line' column from the 'aggfmt' data frame
avgs3 <- merge( avgs1 , aggfmt )

# remove duplicate records from the data frame
avgs3 <- sqldf( 'select distinct * from avgs3' )

# construct the full sql string, grouping each sum by INCLASS (income class) and line (expenditure category)
sql.avgs <- paste( "select INCLASS, line," , avgs.sums , "from avgs3 group by INCLASS, line" )

# execute the sql string
avgs2 <- sqldf( sql.avgs )

## around line 666 in interview mean and se.R






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
  mutate(vq15_s,cat="veq"),mutate(vo15_s,cat="vot")) %>% group_by(subcat)

##cache expn dataset
cache('expn')

# ???
expn$subcat<-as.factor(expn$subcat)

ggplot(data=filter(expn,cat %in% 'veq'), aes(qyear,val))+geom_line(aes(group=subcat,color=subcat))

ggplot(data=filter(expn,cat %in% 'veq'), aes(qyear,val,group=subcat)) + geom_line(aes(color=subcat))

ggplot(economics_long, aes(date, value01, colour = variable)) + geom_line()


plot_ly(filter(expn,cat %in% 'veq'), 
        x=~qyear, y=~val, split=~subcat, type="scatter", mode="lines")

# xpb - taxis, limousines, and mass transportation

# xpa - food & beverages
save(xpa15,file=file.path(wd,"cache","xpa15_cache.RData"))

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
