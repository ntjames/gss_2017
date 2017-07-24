# preprocessing script to drop unneeded files

#remove unneeded files
nms<-ls()

#remove diary files
rm(list=nms[grep("d[0-9]{3}",nms)])

#remove detailed expenditure (EXPN) files
rm(list=nms[grep("[a-z]{3}[0-9]{2}$",nms)])
