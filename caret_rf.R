rm(list = ls())

#load("C:/Users/mfarr/Documents/R_files/Spotfire.data/daily_tables.RData")
load("C:/Users/mfarr/Documents/R_files/Spotfire.data/caret_rf.RData")

isNamespaceLoaded <- function(name) is.element(name, loadedNamespaces())


library(dplyr, warn.conflicts = FALSE)
library(caret, warn.conflicts = FALSE)
library(randomForest)


features_na <- colnames(df)[unique(which(is.na(join), arr.ind = TRUE)[,2])]
features <- colnames(join[,!(colnames(join)%in% c("Uncertainity.Index"))])##exclude a colnames
features <- features[!(features %in% features_na)]
response <- "AcK_mbt_LateTime"

col <- data.frame(colnames(df))

df <- join %>% 
  filter(!is.na(join[response])) %>%
  select_if(is.numeric)

features <- colnames(df[c(16:53, 69)])
df <- na.omit(df[features])

sapply(df, function(x) sum(is.na(x)))

split <- 0.7
set.seed(1234)
idx <- createDataPartition(df[,response],  p = split/100, list = FALSE)

train <- df[idx,]
test <- df[-idx,]

form <- as.formula(paste(response,'~.')) #build formula

#mod_lime <- as_regressor(mod)
set.seed(1234)
rm.mod <- train(form, train[-1], method = 'rf', na.action = na.omit, importance =  TRUE)
lm.mod <- train(form, train, method = 'lm', na.action = na.omit, importance =  TRUE)
rf.imp <- varImp(rm.mod, useModel = TRUE)
lm.imp <- varImp(lm.mod, useModel = TRUE)
plot(rf.imp)
rm.pred <- predict(rm.mod, test)
RMSE(rm.pred, test$AcK_mbt_LateTime)
R2(rm.pred, test$AcK_mbt_LateTime)

RF.imp <- data.frame(Object = row.names(rf.imp$importance), rf.imp$importance)


obj <- rfsrc(form, data = train[-1],ntree=100, importance=TRUE, tree.err=TRUE)
VarImportance<-obj$importance
pd <- predict.rfsrc(obj, test)
rmse <- sqrt(mean((pd$yvar - pd$predicted)^2, na.rm = TRUE))






















