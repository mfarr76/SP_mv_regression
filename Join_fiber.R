rm(list = ls())

load("C:/Users/mfarr/Documents/R_files/Spotfire.data/mv_tables.RData")


##input parameters
prop_tbl
geo
data

suppressWarnings(library(dplyr,warn.conflicts=FALSE))


names(geo) <- gsub("-","", names(geo))
names(geo) <- gsub(" ", "_", names(geo))
names(geo) <- gsub("[()]","", names(geo))
names(geo) <- gsub("_","", names(geo))
names(data) <- gsub(" ","",  names(data))
names(data) <- gsub("/","_",  names(data))
names(data) <- gsub("[()]","", names(data))
names(data) <- gsub("\\%", "", names(data))
names(geo) <- gsub("\\%", "", names(geo))

colnames(data) <- make.names( colnames(data) )

FIBER <- left_join(data, prop_tbl %>%
                     select(WELLCOMPID, PROPNUM), by = "PROPNUM")




##create a Rdata file to load in R========================================================
TimeStamp=paste(date(),Sys.timezone())
tdir = 'C:/Users/MFARR/Documents/R_files/Spotfire.data' # place to store diagnostics if it exists (otherwise do nothing)
if(file.exists(tdir) && file.info(tdir)$isdir) suppressWarnings(try(save(list=ls(), file=paste(tdir,'/join_daily.RData',sep=''), RFormat=T )))
