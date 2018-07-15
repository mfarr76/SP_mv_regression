rm(list = ls())

#load("C:/Users/mfarr/Documents/R_files/Spotfire.data/daily_tables.RData")
load("C:/Users/mfarr/Documents/R_files/Spotfire.data/rta_rf.RData")

##install package if it is not already installed========================================
list.of.packages <- c("dplyr", "tibble")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos =  "https://mran.revolutionanalytics.com/snapshot/2017-05-01/")




##join adapt rta table======================================================
names(geo) <- gsub("-","", names(geo))
names(geo) <- gsub(" ", "_", names(geo))
names(geo) <- gsub("[()]","", names(geo))
names(geo) <- gsub("_","", names(geo))


join <- left_join(geo, rta, by = c("AMAPI" = "API")) %>%
  rename(LEF_PER = `%LEF`,
         UEF_PER = `%UEF`)

##load spotfire below=======================================================


suppressPackageStartupMessages

library(dplyr, warn.conflicts = FALSE)
library(randomForestSRC, warn.conflicts = FALSE)
library('corrplot') # visualisation


##input parameters
join
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



##data type cleanup=======================================================================

#change the name of the Response variable to be R friendly
colnames(join) <- make.names( colnames(join) )
cols <- colnames(join)
response <- make.names(response)

#get the columns which are character type and date or date-time type
types <- sapply(join[,cols],class)
char.cols <- unlist(names(types[types=='character']))
datetime.cols <- unlist(names(types[types=='c("POSIXct", "POSIXt")']))
date.cols <- unlist(names(types[types=='Date']))
date.cols <- c(datetime.cols,date.cols)

join[,char.cols] <- lapply(join[,char.cols] , factor)
join[,date.cols] <- lapply(join[,date.cols] , factor)

if("ProdYear" %in% cols) {
  join$ProdYear <- as.factor(join$ProdYear)
}


##make data table from user input========================================================

output <- data.frame(strsplit(input, ","))
names(output) <- "MAIN"

df <- join[response]
for(i in 1:nrow(output))
{
  idx <- join[which(colnames(join)==as.character(output[i,1]))]
  df <- cbind(df, idx)
}

df <- df %>% filter(!is.na(.[response]))

highCor <- df %>% #find high cor columns
  na.omit() %>%
  select(-one_of(response)) %>%
  select_if(is.numeric) %>%
  select(one_of(names(.[findCorrelation_fast(abs(cor(.)), 0.85)]))) %>%
  colnames()

#highCor <- names(df[-1][findCorrelation_fast(abs(cor(df[-1])), 0.85)])

##remove multicollinearity columns
df <- bind_cols(join %>% 
                  filter(!is.na(join[response])) %>% 
                  select(LEASE), 
                df %>%
                  select_if(is.numeric) %>%
                  select(-one_of(highCor))) %>%
  na.omit() %>% droplevels()

attributes(df)$na.action <- NULL #remove attribute from data.table
df <- df[sapply(df, function(x) length(levels(x)) != 1)] #remove factors that only have 1 level


##split data into train/test===============================================================
set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
trainRow <- sample.int(n = nrow(df), size = floor(split/100*nrow(df)), replace = F)

train <- droplevels(df[trainRow, ]) #create train set
test <- droplevels(df[-trainRow, ]) #create test set

train <- train[sapply(train, function(x) length(levels(x)) != 1)] #drop explainatory columns with only 1 factor
test <- test[sapply(test, function(x) length(levels(x)) != 1)] #drop explainatory columns with only 1 factor


##build lm model=============================================================================
form <- as.formula(paste(response,'~.')) #build formula


obj <- rfsrc(form, data = train[-1],ntree=ntree, importance=TRUE, tree.err=TRUE)

#create Variable Importance table
VarImportance<-NULL
VarImportance<-obj$importance

if(treeType=="Regression"){
  VarImportance<-cbind(VarImportance,names(obj$importance))
  colnames(VarImportance)<-c("Importance","Variable")
  VarImportance <- as.data.frame(VarImportance,stringsAsFactors=FALSE)
  VarImportance$Importance <- as.numeric(VarImportance$Importance)
}else{
  VarImportance<-cbind(VarImportance,rownames(obj$importance))
  colnames(VarImportance)<-c(colnames(obj$importance),"Variable")
  VarImportance <- as.data.frame(VarImportance,stringsAsFactors=FALSE)
  
  for(c in colnames(obj$importance)){
    VarImportance[,c] <- as.numeric(VarImportance[,c])
  }
}


#PredictedValues <- data.frame(test[response], obj$predicted)
#colnames(PredictedValues) <- c("actual", "predicted")

#sapply(test, function(x) sum(is.na(x)))

pd <- predict.rfsrc(obj, test[-1])

PredictedValues <- data.frame(test$LEASE,test[response], pd$predicted)
colnames(PredictedValues) <- c("LEASE","actual", "predicted")


rmse <- sqrt(mean((pd$yvar - pd$predicted)^2, na.rm = TRUE))
mae <- mean(abs(pd$yvar - pd$predicted), na.rm = TRUE)

error <- data.frame(RMSE, MAE)

#create table with stats for the training data
Stats<-capture.output(print(obj))


TimeStamp=paste(date(),Sys.timezone())
tdir = 'C:/Users/MFARR/Documents/R_files/Spotfire.data' # place to store diagnostics if it exists (otherwise do nothing)
if(file.exists(tdir) && file.info(tdir)$isdir) suppressWarnings(try(save(list=ls(), file=paste(tdir,'/string.RData',sep=''), RFormat=T )))

##testing================================================================

join1 <- join %>%
  filter(!is.na(join[response]))

sapply(join1, function(x) sum(is.na(x)))

na_count <- sapply(join1, function(y) sum(is.na(y)))
(na_percent <- data.frame(na_count)/nrow(join1))
#names(public[,na_percent<0.95])
#training_remove_sparse_records<-public[,na_percent<0.95]
join1 <- join1[,na_percent==0]

numericVars <- which(sapply(join1, is.numeric)) #index vector numeric variables
join1_num <- join1[, numericVars]
join1_num$model_H <- NULL

data.frame(row = cumsum(rep(1, ncol(join1))), class = sapply(join1, class))


(highCor <- names(join1_num[,findCorrelation(abs(cor(join1_num)), 0.8)]))
correlate <- cor(join1_num, use = "everything", method = "pearson")
corrplot(correlate, type = "lower", sig.level = 0.01, insig = "blank")



















