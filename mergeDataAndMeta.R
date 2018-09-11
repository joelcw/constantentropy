library(plyr)
library(dplyr)

####

foo <- read.csv("~/constantentropy/")

cleanmeta <- read.csv("~/CurrentLx/OldNorse/gentdigs/pyccleMeta.csv")

####Give appropriate column names to the columns

colnames(foo) <- c("OV","Clause","ObjType","SbjType","Date",ID")

#fulldata <- join(foo,cleanmeta,by="file", type="left")

fulldata <- merge(foo, cleanmeta, by=c("file"), all.y=FALSE)

#fulldata <- left_join(foo,cleanmeta,by="file")

write.csv(fulldata, file="~/CurrentLx/OldNorse/gentdigs/fulldataset.csv", row.names = FALSE)

