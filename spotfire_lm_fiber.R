rm(list = ls())

load("C:/Users/mfarr/Documents/R_files/Spotfire.data/lm.RData")
load("C:/Users/mfarr/Documents/R_files/Spotfire.data/out.RData")



isNamespaceLoaded <- function(name) is.element(name, loadedNamespaces())

##modeling
#library(caret, warn.conflicts = FALSE)
#library(MASS, warn.conflicts = FALSE)
suppressWarnings(library(dplyr, warn.conflicts = FALSE))
library(broom, warn.conflicts = FALSE)
library(olsrr, warn.conflicts = FALSE)
library(RinR, warn.conflicts = FALSE)
library(leaps, warn.conflicts = FALSE)

#library(ggplot2, warn.conflicts = FALSE)
#pushPATH("C:/Program Files/R/R-3.5.0/bin")


#input parameters=========================================================================
join  #data table
input 
split ##train / test split

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

features_na <- colnames(join)[unique(which(is.na(join), arr.ind = TRUE)[,2])]
features <- colnames(join[,!(colnames(join)%in% c("Uncertainity.Index"))])##exclude a colnames
features <- features[!(features %in% features_na)]
target <- response




#get the columns which are character type and date or date-time type
types <- sapply(join[,cols],class)
char.cols <- unlist(names(types[types=='character']))
datetime.cols <- unlist(names(types[types=='c("POSIXct", "POSIXt")']))
date.cols <- unlist(names(types[types=='Date']))
date.cols <- c(datetime.cols,date.cols)

join[,char.cols] <- lapply(join[,char.cols] , character)
join[,date.cols] <- lapply(join[,date.cols] , factor)

if("ProdYear" %in% cols) {
  join$ProdYear <- as.factor(join$ProdYear)
}


##make data table from user input========================================================

output <- data.frame(strsplit(input, ","))
names(output) <- "MAIN"

df <- join[response]
make.names(output[9,1])

for(i in 1:nrow(output))
{
  idx <- join[which(colnames(join)== make.names(as.character(output[i,1])))]
  df <- cbind(df, idx)
}

#sapply(df, function(x) sum(is.na(x)))

df <- df %>% filter(!is.na(.[response]))

highCor <- df %>% #find high cor columns
  na.omit() %>%
  select(-one_of(response)) %>%
  select_if(is.numeric) %>%
  select(one_of(names(.[findCorrelation_fast(abs(cor(.)), 0.8)]))) %>%
  colnames()
#df2<- df %>%
#  select_if(is.numeric) %>%
#  na.omit(.)

#highCor2 <- names(df2[,findCorrelation_fast(abs(cor(df2)), 0.85)])
#highCor <- names(df[findCorrelation_fast(abs(cor(df)), 0.85)])

multi.ONOFF <- "ON"

if(multi.ONOFF == "ON"){
  
  ##remove multicollinearity columns
  df <- bind_cols(join %>% 
                    filter(!is.na(join[response])) %>% 
                    select(LEASE), 
                  df %>%
                    #select_if(is.numeric) %>%
                    select(-one_of(highCor))) %>%
    na.omit() %>% droplevels()
  
}else{
  ##remove multicollinearity columns
  df <- bind_cols(join %>% 
                    filter(!is.na(join[response])) %>% 
                    select(LEASE), 
                  df) %>%
    na.omit() %>% droplevels()
}



attributes(df)$na.action <- NULL #remove attribute from data.table
df <- df[sapply(df, function(x) length(levels(x)) != 1)] #remove factors that only have 1 level

#highCor2 <- names(df[,findCorrelation_fast(abs(cor(df)), 0.8)])
#df <- df %>% select(-one_of(highCor2))


##split data into train/test=================================================================
set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
trainRow <- sample.int(n = nrow(df), size = floor(split/100*nrow(df)), replace = F)

train <- droplevels(df[trainRow, ]) #create train set
test <- droplevels(df[-trainRow, ]) #create test set



#write.csv(train, file = "train.csv")



##build lm model=============================================================================

form <- as.formula(paste(response,'~.')) #build formula
mod <- lm(form, train[-1]) #lm model
lm.stats <- tidy(mod) #stats from lm model
aug.stats <- augment(mod) #stats from lm model
pred.lm <- predict(mod, test[-1])

#length(mod$coefficients) > mod$rank

lm.predict <- test$LEASE
lm.predict <- cbind(lm.predict, data.frame(actual = test[response], predicted = pred.lm))
colnames(lm.predict) <- c("LEASE","actual", "predicted")



RMSE <- sqrt(mean((lm.predict$predicted - lm.predict$actual)^2, na.rm = TRUE))
MAE <- mean(abs(lm.predict$predicted - lm.predict$actual), na.rm = TRUE)

error_lm <- data.frame(RMSE, MAE)


#library(MASS)
aic <- stepAIC(mod, direction = "forward")

bwards <- sort(colnames(aic$model[-1]))

arrange(bwards)


par(mfrow=c(2,2))


graph <- RGraph(
  {par(mfrow=c(2,2)) 
    plot(
      mod)
    ask=F},
  display = FALSE,
  data = c("mod"),
  packages = c("dplyr"), 
  height = 400, width = 600)

lm.mod.plot <- as.raw(graph)




##end of fxn===================================================================================
TimeStamp=paste(date(),Sys.timezone())
tdir = 'C:/Users/MFARR/Documents/R_files/Spotfire.data' # place to store diagnostics if it exists (otherwise do nothing)
if(file.exists(tdir) && file.info(tdir)$isdir) suppressWarnings(try(save(list=ls(), file=paste(tdir,'/lm.RData',sep=''), RFormat=T )))

##end of code==================================================================================

data.frame(row = cumsum(rep(1, ncol(df))), class = sapply(df, class))

train %>% select_if(is.factor) %>%
  sapply(levels)

#sapply(df, function(x) length(levels(x)))

dt <- join %>%
  filter(!is.na(join[response])) %>%
  mutate(cluster = as.character(cluster)) %>%
  select(AcK_mbt_EarlyTime, cluster, Develop, Zone)

sapply(dt, function(x) sum(is.na(x)))

mod_lm <- lm(form, dt)

na_count <- sapply(df, function(y) sum(is.na(y)))
(na_percent <- data.frame(na_count)/nrow(df))
#names(public[,na_percent<0.95])
#training_remove_sparse_records<-public[,na_percent<0.95]
join1 <- join1[,na_percent==0]

numericVars <- which(sapply(join1, is.numeric)) #index vector numeric variables
join1_num <- join1[, numericVars]
names(join1_num)
join1_num$model_H <- NULL

mod <- lm(form, join1_num)

step_back_Mod <- step(mod , direction = "backward")

df <- df %>% select_if(is.numeric)

summary(mod <- lm(form, df))

step_back_Mod <- step(lm.mod , direction = "backward")
step_forward_Mod <- step(lm.mod, direction = "forward")

summary(step_back_Mod)
summary(step_forward_Mod)

par(mfrow=c(2,2))
plot(mod)

autoplot(lm(form, train[-2]))

library(olsrr, warn.conflicts = FALSE)

cl <- makeCluster(2, type = "SOCK")
registerDoSNOW(cl)

t.mod <- lm(AcK_mbt_LateTime ~ StageSpacing + Clusters_1000, df[-1])
lm.mod <- lm(form, train[-1])

k <- ols_step_all_possible(mod)
plot(k)
p <- ols_step_best_subset(mod)
plot(p)
cl <- ols_coll_diag(mod)
data.frame(cl[[1]])
ols_plot_hadi(mod)
ols_plot_cooksd_bar(mod)
ols_plot_dfbetas(mod)
ols_plot_resid_fit(t.mod)
ols_plot_resid_stand(mod)

ols_step_both_p(t.mod)



k <- ols_step_backward_p(lm(AcK_mbt_LateTime ~ StageSpacing, Clusters_1000, train[-1]))
k <- ols_step_best_subset(lm(AcK_mbt_EarlyTime ~ VClayCALC + PermCALC + RHOBCALC, train[-2]))           

summary(k)


dt <- data %>%
  filter(!is.na(join[response]))

mod <- lm(form, df)

library(leaps)

regsub <- regsubsets(form, data = train[-1],
             nbest = 1,       # 1 best model for each number of predictors
             nvmax = NULL,    # NULL for no limit on number of variables
             force.in = NULL, force.out = NULL,
             method = "exhaustive")

reg.summary <- summary(regsub)

library(ggvis)
rsq <- as.data.frame(reg.summary$adjr2)
names(rsq) <- "R2"
rsq %>% 
  ggvis(x=~ c(1:nrow(rsq)), y=~R2 ) %>%
  layer_points(fill = ~ R2 ) %>%
  add_axis("y", title = "R2") %>% 
  add_axis("x", title = "Number of variables")

plot(regsub, scale = "adjr2", main = "Adjusted R2")


plot <- RinR::RGraph(print(plot(regsub, scale = "adjr2", main = "Adjusted R2")), 
                     display = FALSE,
                     data = c("dplry","regsub"),
                     packages = c("leaps"),
                     height = 1500, width = 2000)



aic_b <- stepAIC(mod, direction = "both")
aic <- stepAIC(mod, direction = "backward")
aic <- stepAIC(mod, direction = "forward")
sep <- step(mod, direction = "backward")
sep <- step(mod, direction = "forward")
summary(aic)
aug.stats <- augment(aic) #stats from lm model
lm.stats <- tidy(aic) #stats from lm model
aug.stats <- augment(mod) #stats from lm model
glance(aic)

library(ggplot2)
td <- tidy(aic, conf.int = TRUE)
ggplot(td[-1,], aes(estimate, term, color = term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0)

ggplot(td[-1,], aes(term, estimate))+
  geom_point()+
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high))+
  labs(title = "Coefficients of a linear regression model")

set.seed(1234)
mod_lm <- train(form, train[-2], method = 'lm')

body(predict.lm)

length(mod$coefficients) > mod$rank


library(lime)
library(caret)

#mod_lime <- as_regressor(mod)
rm.mod <- train(form, train[-1], method = 'rf')
explainer <- lime(train[-1], rm.mod)

explanatory <- explain(test[-1], explainer, n_features = 4)

plot_features(explanatory)







