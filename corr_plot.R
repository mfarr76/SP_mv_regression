rm(list = ls())

load("C:/Users/mfarr/Documents/R_files/Spotfire.data/corr_plot.RData")




suppressWarnings(library(dplyr, warn.conflicts = FALSE))
suppressWarnings(library(corrplot, warn.conflicts = FALSE))
suppressWarnings(library(RinR, warn.conflicts = FALSE))
pushPATH("C:/Program Files/R/R-3.4.4/bin")


##not currently using
plot <- RGraph(print(ggplot(test, aes_string(x = test$actual), y = names(test["pred.lm"]))) + 
                 geom_point() +
                 stat_smooth(method = "lm", col = "blue") +
                 labs(title = paste("Adj R2 = ",signif(summary(mod)$adj.r.squared, 5),
                                    "MAE =",signif(mean(abs(test$pred.lm - test$actual), na.rm = TRUE), 4),
                                    "RMSE =",signif(sqrt(mean((test$pred.lm - test$actual)^2, na.rm = TRUE)), 4),
                                    "P =",signif(summary(mod)$coef[2,4], 4))) +
                 theme(plot.title = element_text(lineheight=.8, hjust = 0.5))), 
display = FALSE,
data = c("mod", "test"),
packages = c("ggplot2"),
height = 300, width = 480)

##data prep=============================================================
df <- join %>%
  filter(!is.na(join[response]))

na_count <- sapply(df, function(y) sum(is.na(y)))
(na_percent <- data.frame(na_count/nrow(df)))
#names(public[,na_percent<0.95])
#training_remove_sparse_records<-public[,na_percent<0.95]
df <- df[,na_percent==0]

numericVars <- which(sapply(df, is.numeric)) #index vector numeric variables
df <- df[, numericVars]
df$model_H <- NULL

highCor <- names(df[,findCorrelation(abs(cor(df)), 0.85)])

library(corrplot)

##create correlation matrix
correlate <- cor( df, use = "everything" )

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


