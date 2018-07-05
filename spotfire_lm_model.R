rm(list = ls())

load("C:/Users/mfarr/Documents/R_files/Spotfire.data/lm.RData")


##modeling
#library(caret, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(broom, warn.conflicts = FALSE)
library(olsrr, warn.conflicts = FALSE)
#library(RinR, warn.conflicts = FALSE)
#library(ggplot2, warn.conflicts = FALSE)
#pushPATH("C:/Program Files/R/R-3.4.4/bin")


#input parameters=========================================================================
join  #data table
input 
split ##train / test split

##data type cleanup=======================================================================

#sapply(df, function(x) sum(is.na(x)))

attributes(join)$na.action <- NULL

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
#join$cluster_proppant <- as.factor(gsub("\\*","", join$cluster_proppant))

##make data table from user input========================================================

output <- data.frame(strsplit(input, ","))
names(output) <- "MAIN"

df <- join[response]
df$LEASE <- join["LEASE"]

for(i in 1:nrow(output))
{
  idx <- join[which(colnames(join)==as.character(output[i,1]))]
  df <- cbind(df, idx)
}

#sapply(df, function(x) sum(is.na(x)))

df <- na.omit(df)

##split data into train/test===============================================================
set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
trainRow <- sample.int(n = nrow(df), size = floor(split/100*nrow(df)), replace = F)

train <- df[trainRow, ] #create train set
test <- df[-trainRow, ] #create test set



##remove from sf===========================================================================

##create a for loop to check the distribution of train/test set
dtlist <- list(df, train, test)                                 
dtname <- c("df", "train", "test")

#i <- 3
metric <- data.frame()  #create an empty data.frame to hold each loop
for(i in 1:length(dtlist)){
  dt <- data.frame(dtlist[i])
  dt_tbl <- data.frame(tableName = dtname[i]);
  dt_tbl$wellcount <- nrow(dt);   
  dt_tbl$mean <- mean(dt[[response]], na.rm = TRUE);   
  dt_tbl$median <- median(dt[[response]], na.rm = TRUE);
  dt_tbl$variance <- var(dt[[response]], na.rm = TRUE);
  dt_tbl$sd <- sd(dt[[response]], na.rm = TRUE);
  dt_tbl$P10 <- quantile(dt[[response]], probs = (0.90), na.rm = TRUE);
  dt_tbl$P90 <- quantile(dt[[response]], probs = (0.10), na.rm = TRUE);
  #df_tbl$SoPhiH <- mean(df$SoPhiH_LEF, na.rm = TRUE)
  #df_tbl$tvd <- mean(df$TotalDepthTVD, na.rm = TRUE)
  metric <- rbind(metric, dt_tbl)#store the results of each loop
  #rownames(metric) <- dfname[i]
  #print(metric)
}

#sapply(mod_tbl, function(x) sum(is.na(x)))

##build lm model=============================================================================

form <- as.formula(paste(response,'~.')) #build formula
mod <- lm(form, train[-2]) #lm model
lm.stats <- tidy(mod) #stats from lm model
aug.stats <- augment(mod) #stats from lm model
pred.lm <- predict(mod, test[-2])

length(mod$coefficients) > mod$rank

lm.predict <- test$LEASE
lm.predict <- cbind(lm.predict, data.frame(actual = test[response], predicted = pred.lm))
colnames(lm.predict) <- c("LEASE","actual", "predicted")



RMSE <- sqrt(mean((lm.predict$predicted - lm.predict$actual)^2, na.rm = TRUE))
MAE <- mean(abs(lm.predict$predicted - lm.predict$actual), na.rm = TRUE)

error_lm <- data.frame(RMSE, MAE)


##end of fxn===================================================================================
TimeStamp=paste(date(),Sys.timezone())
tdir = 'C:/Users/MFARR/Documents/R_files/Spotfire.data' # place to store diagnostics if it exists (otherwise do nothing)
if(file.exists(tdir) && file.info(tdir)$isdir) suppressWarnings(try(save(list=ls(), file=paste(tdir,'/lm.RData',sep=''), RFormat=T )))

##end of code==================================================================================

data.frame(row = cumsum(rep(1, ncol(df))), class = sapply(df, class))

sapply(df, function(x) length(levels(x)))

join1 <- join %>%
  filter(!is.na(join[response]))


#sapply(join1, function(x) sum(is.na(x)))



na_count <- sapply(df, function(y) sum(is.na(y)))
(na_percent <- data.frame(na_count)/nrow(df))
#names(public[,na_percent<0.95])
#training_remove_sparse_records<-public[,na_percent<0.95]
join1 <- join1[,na_percent==0]

numericVars <- which(sapply(join1, is.numeric)) #index vector numeric variables
join1_num <- join1[, numericVars]
join1_num <- join1_num[, -c(1:7, 9:10)]
names(join1_num)
join1_num$model_H <- NULL

mod <- lm(form, join1_num)

step_back_Mod <- step(mod , direction = "backward")

df <- df %>% select_if(is.numeric)

summary(mod <- lm(form, df))

step_back_Mod <- step(mod , direction = "backward")
step_forward_Mod <- step(mod, direction = "forward")

summary(step_back_Mod)
summary(step_forward_Mod)

par(mfrow=c(2,2))
plot(mod)

autoplot(lm(form, train[-2]))


k <- ols_step_all_possible(mod)
plot(k)
p <- ols_step_best_subset(mod)
cl <- ols_coll_diag(mod)
data.frame(cl[[1]])

k <- ols_step_backward_p(lm(AcK_mbt_EarlyTime ~ VClayCALC + PermCALC + RHOBCALC, train[-2]))
k <- ols_step_best_subset(lm(AcK_mbt_EarlyTime ~ VClayCALC + PermCALC + RHOBCALC, train[-2]))           

summary(k)


dt <- data %>%
  filter(!is.na(join[response]))

mod <- lm(form, df)

library(leaps)

regsub <- regsubsets(form, data = train[-2],
             nbest = 1,       # 1 best model for each number of predictors
             nvmax = NULL,    # NULL for no limit on number of variables
             force.in = NULL, force.out = NULL,
             method = "exhaustive")

plot(regsub, scale = "adjr2", main = "Adjusted R2")








