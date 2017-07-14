# preprocessing script.

#remove unneeded files 
nms<-ls()

#diary for 1st q
rm(list=nms[grep("d[0-9]{3}",nms)])
