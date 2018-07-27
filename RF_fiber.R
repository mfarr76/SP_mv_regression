rm(list = ls())

#load("C:/Users/mfarr/Documents/R_files/Spotfire.data/Join_mv.RData")
load("C:/Users/mfarr/Documents/R_files/Spotfire.data/fiber_rf.RData")
#load("C:/Users/mfarr/Documents/R_files/Spotfire.data/out.RData")

#response <- "AcK_mbt_LateTime"
multiONOFF <- "OFF"
split <- 60

##install package if it is not already installed========================================
list.of.packages <- c("dplyr", "tibble")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos =  "https://mran.revolutionanalytics.com/snapshot/2017-05-01/")

##load spotfire below===================================================================


suppressPackageStartupMessages

isNamespaceLoaded <- function(name) is.element(name, loadedNamespaces())

library(dplyr, warn.conflicts = FALSE)
#library(randomForestSRC, warn.conflicts = FALSE)
#library('corrplot') # visualisation
library(ranger, warn.conflicts = FALSE)
library(caret, warn.conflicts = FALSE)

##input parameters
Join
input
response
ntree
split

treeType <- "Regression"

##functions===============================================================================
findCorrelation_fast <- function(x, cutoff = .90, verbose = FALSE){
  if(any(!complete.cases(x)))
    stop("The correlation matrix has some missing values.")
  averageCorr <- colMeans(abs(x))
  averageCorr <- as.numeric(as.factor(averageCorr))
  x[lower.tri(x, diag = TRUE)] <- NA
  combsAboveCutoff <- which(abs(x) > cutoff)
  
  colsToCheck <- ceiling(combsAboveCutoff / nrow(x))
  rowsToCheck <- combsAboveCutoff %% nrow(x)
  
  colsToDiscard <- averageCorr[colsToCheck] > averageCorr[rowsToCheck]
  rowsToDiscard <- !colsToDiscard
  
  if(verbose){
    colsFlagged <- pmin(ifelse(colsToDiscard, colsToCheck, NA),
                        ifelse(rowsToDiscard, rowsToCheck, NA), na.rm = TRUE)
    values <- round(x[combsAboveCutoff], 3)
    cat('\n',paste('Combination row', rowsToCheck, 'and column', colsToCheck,
                   'is above the cut-off, value =', values,
                   '\n \t Flagging column', colsFlagged, '\n'
    ))
  }
  
  deletecol <- c(colsToCheck[colsToDiscard], rowsToCheck[rowsToDiscard])
  deletecol <- unique(deletecol)
  deletecol
}


level_count <- function(table){
  sum(ifelse(sapply(table, function(x) length(levels(x)) != 1) == FALSE, 1, 0))
}


##data type cleanup=======================================================================

#change the name of the Response variable to be R friendly
colnames(Join) <- make.names( colnames(Join) )
cols <- colnames(Join)
response <- make.names(response)

##remover na's based on response variable and change from char to factor
Join <- Join %>% filter(!is.na(.[response])) %>%
  mutate_if(is.character, as.factor)

##make data table from user input====================================================================
output <- data.frame(strsplit(input, ","))
names(output) <- "MAIN"

##create data table with response variable first then loop in the explainatory variables
df <- Join[response]
i<-1
for(i in 1:nrow(output))##for loop
{
  idx <- Join[which(colnames(Join)==as.character(output[i,1]))]
  df <- cbind(df, idx)
}

df <- df %>% filter(!is.na(.[response]))##remove na's

##combine with Join table to bring in LEASE
df <- bind_cols(Join %>% 
                  filter(!is.na(Join[response])) %>% 
                  select(LEASE), 
                df)
##end of data table build=============================================================================

form <- as.formula(paste(response,'~.'))##build formula

highCor <- df %>% #find high cor columns
  na.omit() %>%
  select(-one_of(response)) %>%
  select_if(is.numeric) %>%
  select(one_of(names(.[findCorrelation_fast(abs(cor(.)), 0.85)]))) %>%
  colnames()

#highCor <- names(df[-1][findCorrelation_fast(abs(cor(df[-1])), 0.85)])


if(multiONOFF == "ON"){
  
  ##remove multicollinearity columns
  df <- bind_cols(Join %>% 
                    filter(!is.na(Join[response])) %>% 
                    select(LEASE), 
                  df %>%
                    #select_if(is.numeric) %>%
                    select(-one_of(highCor))) %>%
    na.omit() %>% droplevels()
  
}else{
  ##remove multicollinearity columns
  df <- bind_cols(Join %>% 
                    filter(!is.na(Join[response])) %>% 
                    select(LEASE), 
                  df) %>%
    na.omit() %>% droplevels()
}

attributes(df)$na.action <- NULL #remove attribute from data.table
df <- df[sapply(df, function(x) length(levels(x)) != 1)] #remove factors that only have 1 level


##split data into train/test===============================================================
set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
#trainRow <- sample.int(n = nrow(df), size = floor(split/100*nrow(df)), replace = F)
trainRow <- createDataPartition(df[[response]],  p = split/100, list = FALSE)


RF.Train <- droplevels(df[trainRow, ]) #create train set
RF.Test <- droplevels(df[-trainRow, ]) #create test set


##remove factor levels====================================================================

if(level_count(RF.Train)!=0) {#check train table and remove if 1 level
  
  RF.Test <- RF.Test[-which(colnames(RF.Train)==colnames(RF.Train[sapply(RF.Test, function(x) length(levels(x)) == 1)]))]
  RF.Train <- RF.Train[sapply(RF.Train, function(x) length(levels(x)) != 1)] #drop explainatory columns with only 1 factor 
}


if(level_count(RF.Test)!=0) {#check test table and remove if 1 level
  
  
  RF.Train <- RF.Train[-which(colnames(RF.Test)==colnames(RF.Test[sapply(RF.Test, function(x) length(levels(x)) == 1)]))]
  RF.Test <- RF.Test[sapply(RF.Test, function(x) length(levels(x)) != 1)] #drop explainatory columns with only 1 factor 

}


##build rf model=============================================================================
form <- as.formula(paste(response,'~.')) #build formula


#obj <- rfsrc(form, data = train[-1],ntree=ntree, importance=TRUE, tree.err=TRUE)

##ranger package
mtry <- ceiling(sqrt(ncol(RF.Train)))


set.seed(1234)
obj <- ranger(form, RF.Train, mtry = mtry, 
              importance = "permutation", num.trees = 1000)

RF.Train$predict <- obj$predictions


#create Variable Importance table
VarImportance<-NULL
VarImportance<-obj$variable.importance

if(treeType=="Regression"){
  VarImportance<-cbind(VarImportance,names(obj$variable.importance))
  colnames(VarImportance)<-c("Importance","Variable")
  VarImportance <- as.data.frame(VarImportance,stringsAsFactors=FALSE)
  VarImportance$Importance <- as.numeric(VarImportance$Importance)
}else{
  VarImportance<-cbind(VarImportance,rownames(obj$vaiable.importance))
  colnames(VarImportance)<-c(colnames(obj$importance),"Variable")
  VarImportance <- as.data.frame(VarImportance,stringsAsFactors=FALSE)
  
  for(c in colnames(obj$importance)){
    VarImportance[,c] <- as.numeric(VarImportance[,c])
  }
}


pred_rngr <- predictions(predict(obj, RF.Test[-1], num.trees = 1000))
RF.Test$predict <- predictions(predict(obj, RF.Test[-1], num.trees = 1000))


RMSE <- sqrt(mean((RF.Test[[response]] - pred_rngr)^2, na.rm = TRUE))
MAE <- mean(abs(RF.Test[[response]] - pred_rngr), na.rm = TRUE)

error <- data.frame(RMSE, MAE)


#create table with stats for the training data
Stats <- capture.output(print(obj))





TimeStamp=paste(date(),Sys.timezone())
tdir = 'C:/Users/MFARR/Documents/R_files/Spotfire.data' # place to store diagnostics if it exists (otherwise do nothing)
if(file.exists(tdir) && file.info(tdir)$isdir) suppressWarnings(try(save(list=ls(), file=paste(tdir,'/string.RData',sep=''), RFormat=T )))

##bootstrap==============================================================================
#i <- 20
split <- 60
##create random samples with replacement
set.seed(1)
(sam <- sample(c(1:1000), size = 1000, replace=TRUE))

#test
#sam == 2

##create output placeholder for each loop
train_mean <- vector("double", 1000) #mean of response variable in train set
test_mean <- vector("double", 1000) #mean of response variable in test set
rmse <- vector("double", 1000)  #rmse in units of response variable
r2_train <- vector("double", 1000) #r2 of the model using the training set
r2_test <- vector("double", 1000) #r2 fo the model using the test set
#mod_coef <- matrix(0,1000,16) #matrix to hold coefficients from each loop

for(i in 1:1000){
  
  set.seed(sam[i])
  trainRow <- createDataPartition(df[[response]],  p = split/100, list = FALSE)
  #trainRow <- sample.int(n = nrow(df), size = floor(split/100*nrow(df)), replace = F)
  train <- df[trainRow, ] #create train set
  test <- df[-trainRow, ] #create test set
  train_mean[i] <- mean(train[[response]]) #mean of train
  test_mean[i] <- mean(test[[response]]) #mean of test
  
  set.seed(sam[i]) 
  mod_rf <- ranger(form, train[-1], mtry = mtry, importance = "permutation", num.trees = 1000)
  pred_rf <- predictions(predict(mod_rf, test[-1], num.trees = 1000))
  rmse[i] <- sqrt(mean((test[[response]] - pred_rf)^2, na.rm = TRUE)) #calc rmse
  r2_train[i] <- mod_rf$r.squared #retreive r2 from model
  r2_test[i] <- cor(pred_rf, test[[response]])^2 #retreive r2 from model
  
}


##create a histrogram of rmse values with red line as your mean
hist(rmse, density=35, main = "Test RMSE over 1000 Samples", xlab = "Value of Obtained RMSE", col="blue", border="black")
abline(v=mean(rmse), lwd=3, col="red")

##create a histrogram of adj r2 values with red line as your mean
hist(r2_train, density=35, main = "Test R2_Train over 1000 Samples", xlab = "Value of Obtained R2_Train", col="blue", border="black")
abline(v=mean(r2_train), lwd=3, col="red")

##create a histrogram of adj r2 values with red line as your mean
hist(r2_test, density=35, main = "Test R2_Test over 1000 Samples", xlab = "Value of Obtained R2_Test", col="blue", border="black")
abline(v=mean(r2_test), lwd=3, col="red")

##create a histrogram of train_mean values with red line as your mean
hist(train_mean, density=35, main = "Train_Mean over 1000 Samples", xlab = "Value of Obtained Train_Mean", col="blue", border="black")
abline(v=mean(train_mean), lwd=3, col="red")


##create a histrogram of test_mean values with red line as your mean
hist(test_mean, density=35, main = "Test Mean over 1000 Samples", xlab = "Value of Obtained Test_Mean", col="blue", border="black")
abline(v=mean(test_mean), lwd=3, col="red")


##testing================================================================

R2(pred_rngr, RF.Test$AcK_mbt_EarlyTime)

objJoin1 <- Join %>%
  filter(!is.na(Join[response]))

sapply(Join1, function(x) sum(is.na(x)))

na_count <- sapply(Join1, function(y) sum(is.na(y)))
(na_percent <- data.frame(na_count)/nrow(Join1))
#names(public[,na_percent<0.95])
#training_remove_sparse_records<-public[,na_percent<0.95]
Join1 <- Join1[,na_percent==0]

numericVars <- which(sapply(Join1, is.numeric)) #index vector numeric variables
Join1_num <- Join1[, numericVars]
Join1_num$model_H <- NULL

data.frame(row = cumsum(rep(1, ncol(Join1))), class = sapply(Join1, class))


(highCor <- names(Join1_num[,findCorrelation(abs(cor(Join1_num)), 0.8)]))
correlate <- cor(Join1_num, use = "everything", method = "pearson")
corrplot(correlate, type = "lower", sig.level = 0.01, insig = "blank")

##create a for loop to check the distribution of train/test set
dflist <- list(df, RF.Train, RF.Test)                                 
dfname <- c("df", "RF.Train", "RF.Test")

##for more information on for loops go to the website below
#http://r4ds.had.co.nz/iteration.html#for-loops

metric <- data.frame()  #create an empty data.frame to hold each loop
for(i in 1:length(dflist)){
  df <- data.frame(dflist[i])
  df_tbl <- data.frame(wellcount = nrow(df))   
  df_tbl$mean <- mean(df[[response]], na.rm = TRUE)   
  df_tbl$median <- median(df[[response]], na.rm = TRUE)
  df_tbl$variance <- var(df[[response]], na.rm = TRUE)
  df_tbl$sd <- sd(df[[response]], na.rm = TRUE)
  df_tbl$P10 <- quantile(df[[response]], probs = (0.90), na.rm = TRUE)
  df_tbl$P90 <- quantile(df[[response]], probs = (0.10), na.rm = TRUE)
  #df_tbl$SoPhiH <- mean(df$SoPhiH_LEF, na.rm = TRUE)
  #df_tbl$tvd <- mean(df$TotalDepthTVD, na.rm = TRUE)
  metric <- rbind(metric, df_tbl)#store the results of each loop
  #rownames(metric) <- dfname[i]
  #print(metric)
}
rownames(metric) <- dfname
metric




##feature selection====================================================
library(party)
options(StringsAsFactors=TRUE)

data.frame(colnames(Join))

#change the name of the Response variable to be R friendly
colnames(Join) <- make.names( colnames(Join) )
well <- df
#well<-Join[,complete.cases(Join)]
wellid<-well[,1]
well<-well[,-1] ##nice way to create table without your response variable
well[,1]<-as.factor(well[,1])
well<-select_if(well, is.numeric)

sapply(well, function(x) sum(is.na(x)))

##ranger package
mtry <- ceiling(sqrt(ncol(well)))

fCtrl <-  cforest(form, data= well, control=cforest_unbiased(mtry=mtry,ntree=50))
wello <-data.frame("mean decrease accuracy"=varimp(fCtrl))
wello<-data.frame(Features= rownames(wello),wello)

colnames<-rownames(wello)
colnames<-paste(rownames(wello),collapse="],[")
colnames=paste("[",colnames,"]",sep="")













