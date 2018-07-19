rm(list = ls())

#load("C:/Users/mfarr/Documents/R_files/Spotfire.data/daily_tables.RData")
load("C:/Users/mfarr/Documents/R_files/Spotfire.data/caret_rf.RData")

isNamespaceLoaded <- function(name) is.element(name, loadedNamespaces())


library(dplyr, warn.conflicts = FALSE)
library(caret, warn.conflicts = FALSE)
library(randomForest)
library(ranger)


features_na <- colnames(df)[unique(which(is.na(join), arr.ind = TRUE)[,2])]
features <- colnames(join[,!(colnames(join)%in% c("Uncertainity.Index"))])##exclude a colnames
features <- features[!(features %in% features_na)]
response <- "AcK_mbt_LateTime"

col <- data.frame(colnames(df))

df <- join %>% 
  filter(!is.na(join[response])) %>%
  select_if(is.numeric)

sapply(df, function(x) sum(is.na(x)))

features <- colnames(df[c(16:53, 69)])
df <- na.omit(df[features])

df <- na.omit(df)

sapply(df, function(x) sum(is.na(x)))

split <- 70
set.seed(1234)
idx <- createDataPartition(df[,response],  p = split/100, list = FALSE)

train <- df[idx,]
test <- df[-idx,]


form <- as.formula(paste(response,'~.')) #build formula

set.seed(1234)
rm.mod <- train(AcK_mbt_LateTime ~ spacing_prodyear_bin + effectivelateral + TotalClusters,
                RF.Train, method = 'rf', na.action = na.omit, importance =  TRUE)

set.seed(1234)
rngr <- ranger(AcK_mbt_LateTime ~ spacing_prodyear_bin + effectivelateral + TotalClusters, RF.Train, 
               importance = "permutation", num.trees = 1000)


RF.Train %>% select(AcK_mbt_LateTime, spacing_prodyear_bin, effectivelateral, TotalClusters)

set.seed(1234)
rm.mod <- train(form, RF.Train[-1], method = 'rf', na.action = na.omit, importance =  TRUE)
lm.mod <- train(form, RF.Train[-1], method = 'lm', na.action = na.omit, importance =  TRUE)
rf.imp <- varImp(rm.mod, useModel = TRUE)
lm.imp <- varImp(lm.mod, useModel = TRUE)
plot(rf.imp)
rm.pred <- predict(rm.mod, RF.Test)
RMSE(rm.pred, RF.Test$AcK_mbt_LateTime)
R2(rm.pred, RF.Test$AcK_mbt_LateTime)

PredictedValues <- data.frame(test$LEASE,test[response], rm.pred)
colnames(PredictedValues) <- c("LEASE","actual", "predicted")


RF.imp <- data.frame(Object = row.names(rf.imp$importance), rf.imp$importance)

##ranger
set.seed(1234)
rngr <- ranger(form, RF.Train[-1], importance = "permutation", num.trees = 1000)
pred_rngr <- predictions(predict(rngr, RF.Test[-1], num.trees = 1000))
#barchart(sort(rngr$variable.importance))
#var <- data.frame(rngr$variable.importance)
var <- data.frame(Names = names(rngr$variable.importance) , Value = rngr$variable.importance)
R2()
R2(pred_rngr, RF.Test$AcK_mbt_LateTime)


library(ggplot2)
ggplot(var, aes(Names, Value)) + geom_col()
















