rm(list = ls())


load("C:/Users/mfarr/Documents/R_files/Spotfire.data/mv_tables.RData")
load("C:/Users/mfarr/Documents/R_files/Spotfire.data/feature_fiber.RData")
load("C:/Users/mfarr/Documents/R_files/Spotfire.data/join_mv.RData")

response <- "CUMGAS_3MO"
var.input <- input

##load packages=====================================================================================
isNamespaceLoaded <- function(name) is.element(name, loadedNamespaces())

suppressWarnings(library(dplyr, warn.conflicts = FALSE))
#suppressPackageStartupMessages(library(party))
suppressPackageStartupMessages(library(ranger))
suppressPackageStartupMessages(library(caret))
#suppressPackageStartupMessages(library(e1071))
suppressWarnings(library(e1071, warn.conflicts = FALSE))

options(StringsAsFactors=TRUE)

##input parameters
var.input
response
Join


#change the name of the Response variable to be R friendly
colnames(Join) <- make.names( colnames(Join) )
cols <- colnames(Join)
response <- make.names(response)

##remover na's based on response variable and change from char to factor
Join <- Join %>% filter(!is.na(.[response])) %>%
  mutate_if(is.character, as.factor)

##make data table from user input====================================================================
output <- data.frame(strsplit(var.input, ","))
names(output) <- "MAIN"

##create data table with response variable first then loop in the explainatory variables
df <- Join[response]
i<-1
for(i in 1:nrow(output))##for loop
{
  idx <- Join[which(colnames(Join)==as.character(output[i,1]))]
  df <- cbind(df, idx)
}

well <- df %>% filter(!is.na(.[response]))##remove na's


##build formula
form <- as.formula(paste(response,'~.')) 

##remove rows with na's
well <- well[complete.cases(well), ]

sapply(df, function(x) sum(is.na(x)))

set.seed(1234)
obj_caret <- train(form, data=well, method='ranger', 
                   importance = "permutation", metric = 'RMSE',
                   na.action = na.omit, num.trees = 500)

wello <- data.frame("mean decrease accuracy" = varImp(obj_caret, useModel = TRUE)[1])
wello <- data.frame(Features= rownames(wello),wello)

colnames<-rownames(wello)
colnames<-paste(rownames(wello),collapse="],[")
colnames=paste("[",colnames,"]",sep="")






TimeStamp=paste(date(),Sys.timezone())
tdir = 'C:/Users/MFARR/Documents/R_files/Spotfire.data' # place to store diagnostics if it exists (otherwise do nothing)
if(file.exists(tdir) && file.info(tdir)$isdir) suppressWarnings(try(save(list=ls(), file=paste(tdir,'/feature.RData',sep=''), RFormat=T )))


library(caret)


dummies <- dummyVars(GasRate5_9MSCFD ~ ., data)


head(predict(dummies, data))

data$RockGroup <- as.factor(data$RockGroup)
rg <- C(data$RockGroup, treatment)

df <- data

lm(df$GasRate5_9MSCFD ~ rg)

C(data$RockGroup)







