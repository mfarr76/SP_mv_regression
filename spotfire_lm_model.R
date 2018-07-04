rm(list = ls())

load("C:/Users/mfarr/Documents/R_files/Spotfire.data/lm.RData")


##modeling
#library(caret, warn.conflicts = FALSE)                                                
library(broom, warn.conflicts = FALSE)

library(RinR, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
pushPATH("C:/Program Files/R/R-3.4.4/bin")


#input parameters
join

response <- "GasEURBCF"

##make friendly column name
response <- make.names(response)

sapply(df, function(x) sum(is.na(x)))

attributes(join)$na.action <- NULL

#change the name of the Response variable to be R friendly
colnames(join) <- make.names( colnames(join) )
cols <- colnames(join)

#get the columns which are character type and date or date-time type
types <- sapply(join[,cols],class)
char.cols<- unlist(names(types[types=='character']))
datetime.cols<- unlist(names(types[types=='c("POSIXct", "POSIXt")']))
date.cols <- unlist(names(types[types=='Date']))
date.cols <- c(datetime.cols,date.cols)

join[,char.cols] <- lapply(join[,char.cols] , factor)
join[,date.cols] <- lapply(join[,date.cols] , factor)

output <- data.frame(strsplit(input, ","))
names(output) <- "MAIN"

join$cluster_proppant <- as.factor(gsub("\\*","", join$cluster_proppant))

##make friendly column name
response <- make.names(response)


df <- join[response]
df$LEASE <- join["LEASE"]

for(i in 1:nrow(output))
{
  idx <- join[which(colnames(join)==as.character(output[i,1]))]
  df <- cbind(df, idx)
}

#sapply(df, function(x) sum(is.na(x)))

df <- na.omit(df)

nrow(df)


##split data into train/test===============================================================
set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
trainRow <- sample.int(n = nrow(join), size = floor(.75*nrow(join)), replace = F)

train <- df[trainRow, ] #create train set
test <- df[-trainRow, ] #create test set


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



sapply(mod_tbl, function(x) sum(is.na(x)))

form <- as.formula(paste(response,'~.')) #build formula
mod <- lm(form, train[-2]) #lm model
lm.stats <- tidy(mod) #stats from lm model
aug.stats <- augment(mod) #stats from lm model
pred.lm <- predict(mod, test)


lm.predict <- data.frame(test$LEASE,test[response], pred.lm)
colnames(lm.predict) <- c("LEASE","actual", "predicted")


RMSE <- sqrt(mean((lm.predict$predicted - lm.predict$actual)^2, na.rm = TRUE))
MAE <- mean(abs(lm.predict$predicted - lm.predict$actual), na.rm = TRUE)

error_lm <- data.frame(RMSE, MAE)


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









