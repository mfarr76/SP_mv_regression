rm(list = ls())
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/rf.RData")

count_zero(Orig.Data)
count_NA(Orig.Data)


install_load <- function (package1, ...)  {   
  
  # convert arguments to vector
  packages <- c(package1, ...)
  
  # start loop to determine if each package is installed
  for(package in packages){
    
    # if package is installed locally, load
    if(package %in% rownames(installed.packages()))
      do.call('library', list(package))
    
    # if package is not installed locally, download, then load
    else {
      install.packages(package, repos="https://cloud.r-project.org/")
      do.call("library", list(package))
    }
  } 
}

install_load('randomForestSRC')

############## RAI UTILITY FUNCTION ##############
LogOut = ""
LogErr = ""
log.out <- function(msg) {
  fullmsg = paste(paste("[",Sys.time(),"]: "), toString(msg), sep=" ");
  LogOut <<- paste(LogOut, fullmsg, sep="\n");
}

##################################################


results <- NULL
ErrRate<-NULL
Stats <- NULL
OutputTable <-NULL
ImputedData <- NULL
Stats.predict <- NULL
VarImportance <- NULL
conf.matx <- NULL
NumOfTrees <- NumTrees

#change the name of the Response variable to be R friendly
colnames(Data) <- make.names( colnames(Data) )
cols <- colnames(Data)

Orig.Data <- Data

Predictors <- cols[1:(length(cols)-1)]
Response <- cols[length(cols)]


attributes(data)$na.action <- NULL

#get the columns which are character type and date or date-time type
types <- sapply(Data[,cols],class)
char.cols<- unlist(names(types[types=='character']))
datetime.cols<- unlist(names(types[types=='c("POSIXct", "POSIXt")']))
date.cols <- unlist(names(types[types=='Date']))
date.cols <- c(datetime.cols,date.cols)


if(Response %in% char.cols){
  treeType <- "Classification"
} else{
  treeType <- "Regression"}

#convert char variables to factors
Data[,char.cols] <- lapply(Data[,char.cols] , factor)
Data[,date.cols] <- lapply(Data[,date.cols] , factor)

#training data
data.train <- which(!is.na(Data[,Response]))

#build formula
form <- as.formula(paste(Response,'~.'))

complete.cases.idx = which(complete.cases(Data[,Predictors]))

#run Random Forest
if(Imputation !='No'){
  #train
  obj <- rfsrc(form,data=Data[data.train,cols],ntree=NumOfTrees , na.action=Imputation, importance=TRUE, tree.err=TRUE)
  #predict on the whole data set
  obj.predict <- rfsrc(form,data=Data[,cols],ntree=NumOfTrees , importance=FALSE, na.action=Imputation)
  
}else{
  
  #predict on the whole data set
  obj <- rfsrc(form,data=Data[data.train,cols], na.action="na.omit", ntree=NumOfTrees,importance=TRUE, tree.err=TRUE)
  #predict on the whole data set (exclude NA predictors)
  #responses to predict are NA, hence, imputation method has to be used here; otherwise the method skips those records as incomplete
  obj.predict <- rfsrc(form,data=Data[complete.cases.idx,cols],ntree=NumOfTrees , importance=FALSE, na.action="na.impute")
  
}



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






#create table with Error Rates

if(treeType=="Regression"){
  ErrRate <- as.data.frame(cbind(1:length(obj$err.rate),obj$err.rate))
  colnames(ErrRate) <- c("Number of Trees","Error Rate")
} else{
  ErrRate <- as.data.frame(cbind(1:nrow(obj$err.rate),obj$err.rate))
  colnames(ErrRate) <- c("Number of Trees",colnames(obj$err.rate))
}


#create table with stats for the training data
Stats<-capture.output(print(obj))

if(treeType=='Classification'){
  Stats <- c(Stats[1:12],Stats[length(Stats)])
}

#create table with stats for the training data
Stats.predict<-capture.output(print(obj.predict))
#keep olnly the first row
Stats.predict <- Stats.predict[1]


#create confusion matrix
if(treeType=="Classification"){
  conf.matx <- table(obj$yvar, if(!is.null(obj$class.oob) && !all(is.na(obj$class.oob))) obj$class.oob else obj$class)
  colnames(conf.matx) <- paste(colnames(conf.matx),"-predicted",sep="")
  conf.matx <- cbind(conf.matx,  class.error = round(1 - diag(conf.matx)/rowSums(conf.matx, na.rm = TRUE), 4))
  
  conf.matx <- cbind(paste(rownames(conf.matx),"-observed",sep=""),conf.matx)
  conf.matx <- as.data.frame(conf.matx, stringsAsFactors=FALSE)
  for(c in colnames(conf.matx)){
    if(c!="V1"){
      conf.matx[,c] <- as.numeric(conf.matx[,c])
    }
  }
  
}	  

# Predict values
result <- predict(obj.predict, newdata=Data[,cols], na.action="na.impute")
if(treeType=="Regression"){
  predicted <- as.data.frame(result$predicted)
} else{
  predicted <- as.data.frame(result$class)
}
colnames(predicted)<-paste(ModelName, '.Predicted', sep="");

PredictedValues <- predicted

if(Imputation == 'No') {
  # Remove incomplete cases
  PredictedValues$Predicted[!complete.cases(Data[,Predictors])] = NA
}


TimeStamp=paste(date(),Sys.timezone())
tdir = 'C:/Users/MFARR/Documents/R_files/Spotfire.data' # place to store diagnostics if it exists (otherwise do nothing)
if(file.exists(tdir) && file.info(tdir)$isdir) suppressWarnings(try(save(list=ls(), file=paste(tdir,'/rf.RData',sep=''), RFormat=T )))
