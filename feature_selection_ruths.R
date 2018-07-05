rm(list = ls())

well <- read.csv("feature.csv")

#making suredplyr is installed
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

isNamespaceLoaded <- function(name) is.element(name, loadedNamespaces())


sapply(well, function(x) sum(is.na(x)))

install_load('party')
#library(caret)
options(StringsAsFactors=TRUE)
well<-well[complete.cases(well),]
wellid<-well[,1]
well<-well[,-1] ##nice way to create table without your response variable
well[,1]<-as.factor(well[,1])
fCtrl <-  cforest(EURCategory ~ . , data= well, control=cforest_unbiased(mtry=2,ntree=50))
wello <-data.frame("mean decrease accuracy"=varimp(fCtrl))
wello<-data.frame(Features= rownames(wello),wello)

colnames<-rownames(wello)
colnames<-paste(rownames(wello),collapse="],[")
colnames=paste("[",colnames,"]",sep="")


