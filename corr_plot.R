rm(list = ls())

load("C:/Users/mfarr/Documents/R_files/Spotfire.data/corr_plot.RData")
load("C:/Users/mfarr/Documents/R_files/Spotfire.data/daily_tables.RData")
response <- "GasEURBCF"

suppressWarnings(library(dplyr, warn.conflicts = FALSE))
suppressWarnings(library(corrplot, warn.conflicts = FALSE))


##data prep=============================================================
df <- join %>%
  #filter(!is.na(join[response]))
  filter(!is.na(SwCALC), 
         !is.na(initialwhpsiavg), 
         !is.na(distancebtwnstagesftavg), 
         !is.na(perfclusterspacing))

na_count <- sapply(df, function(y) sum(is.na(y)))
(na_percent <- data.frame(na_count/nrow(df)))
#names(public[,na_percent<0.95])
#training_remove_sparse_records<-public[,na_percent<0.95]
df <- df[,na_percent==0]

numericVars <- which(sapply(df, is.numeric)) #index vector numeric variables
df <- df[, numericVars]
df$model_H <- NULL

highCor <- names(df[,findCorrelation_fast(abs(cor(df)), 0.85)])

library(corrplot)

##create correlation matrix
correlate <- cor( df, use = "everything" )
corrName <- data.frame(colName = rownames(correlate))

#correlate <- cbind(ColName = rownames(correlate), correlate)
correlate <- cbind(corrName, correlate)

corr_plot<- RGraph(print(corrplot(correlate, method="circle", type="lower",  
                                  sig.level = 0.01, insig = "blank")), 
                   display = FALSE,
                   data = c("correlate"),
                   packages = c("corrplot"), 
                   height = 400, width = 600)


corrplot(correlate, method="circle", type="lower",  
         sig.level = 0.01, insig = "blank")

cor_matrix<-abs(cor(df))
diag(cor_matrix)<-0
library(corrplot)
corrplot(cor_matrix, method="square", type = "lower")


