rm(list = ls())

#well <- read.csv("feature.csv")
load("C:/Users/mfarr/Documents/R_files/Spotfire.data/feature.RData")


##load packages=====================================================================================
isNamespaceLoaded <- function(name) is.element(name, loadedNamespaces())

suppressWarnings(library(dplyr, warn.conflicts = FALSE))
suppressPackageStartupMessages(library(party))
library(caret)

options(StringsAsFactors=TRUE)

##input parameters
input
response
join

##remove Ack variables
if(response == "AcK_mbt_EarlyTime"){
  
join <- join[-which(colnames(join)=="AcK_mbt_LateTime")]
}

##remove Ack variables
if(response == "AcK_mbt_LateTime"){
  
join <- join[-which(colnames(join)=="AcK_mbt_EarlyTime")]
}


##build formula
form <- as.formula(paste(response,'~.')) 

##remover na's based on response variable and change from char to factor
well <- join %>% filter(!is.na(.[response])) %>%
  mutate_if(is.character, as.factor) %>% 
  filter(TypeCurve == "B")

##remove rows with na's
well <- well[complete.cases(well), ]



mtry <- ceiling(sqrt(ncol(well)))

set.seed(1234)
#fCtrl <- train(form, data=well, method='ranger', na.action = na.omit, importance=TRUE)
fCtrl <-  cforest(form, data= well, control=cforest_unbiased(mtry=mtry,ntree=50))
wello <-data.frame("mean decrease accuracy"=varimp(fCtrl, conditional = TRUE))
wello<-data.frame(Features= rownames(wello),wello)

colnames<-rownames(wello)
colnames<-paste(rownames(wello),collapse="],[")
colnames=paste("[",colnames,"]",sep="")

obj_caret <- train(form, data=well, method='ranger', 
                   importance = "permutation", metric = 'RMSE',
                   na.action = na.omit, num.trees = 500)

wello <- data.frame(varImp(obj_caret, useModel = TRUE)[1])
wello<-data.frame(Features= rownames(wello),wello)

plot(varImp(obj_caret), top = 20)

obj.pred <- predict(obj_caret, RF.Test[-1])
RMSE(obj.pred, RF.Test$AcK_mbt_EarlyTime)
R2(obj.pred, RF.Test$AcK_mbt_EarlyTime)


TimeStamp=paste(date(),Sys.timezone())
tdir = 'C:/Users/MFARR/Documents/R_files/Spotfire.data' # place to store diagnostics if it exists (otherwise do nothing)
if(file.exists(tdir) && file.info(tdir)$isdir) suppressWarnings(try(save(list=ls(), file=paste(tdir,'/feature.RData',sep=''), RFormat=T )))

