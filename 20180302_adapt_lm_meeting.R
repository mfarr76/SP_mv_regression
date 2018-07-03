##lm meeting 3/2/18
##concepts used in this workflow can be found in part at the link below
##highly reading it and using it as a refernce
##http://hadley.nz/ Hadley Wickham website...hadley is the man!!
##http://r4ds.had.co.nz/ R for Data Science

rm(list = ls()) #clear workspace

##load packages==========================================================================
##first time user will need to install the packages below
#install.packages(c("tidyverse", "caret", "corrplot", "GGally", "gridExtra"))
##data wrangling
library(readxl)                                               
library(dplyr)

##modeling
library(caret)                                                 
library(broom)

##Extra vis
library(corrplot)
library(GGally)
library(gridExtra)

##load data / cleanup======================================================================
dt <- read_excel("M:/Shared/ADAPT/Final DB/Final Production Drive Dataset.xlsx")
#dt <- read.csv("FinalProductionDriveDataset.csv")

names(dt) = gsub(" ", "", names(dt)) #remove spaces from column names

na_count <- sapply(dt, function(y) sum(is.na(y))) #count the number of na per column
(na_percent <- data.frame(na_count)/nrow(dt)) #na percent per column

#dt <- dt[,na_percent==0]   #remove na columns if needed
dt <- dt[!(is.na(dt$`365DayCum-MBOE`)),]   #remove na from target variable



##multicollinearity test===================================================================
dt_num <- dt %>% 
  na.omit() %>%   #have to remove NA for findCorelation function to work
  select_if(is.numeric)  #select numeric columns only

(highCor <- names(dt_num[,findCorrelation(abs(cor(dt_num)), 0.8)]))
correlate <- cor(dt_num, use = "everything", method = "pearson")
corrplot(correlate, method = "pie", type = "lower", sig.level = 0.01, insig = "blank")
ggpairs(dt_num[1:5])

##split data into train/test===============================================================
set.seed(1234) #set.seed for reproducibility
trainRow <- createDataPartition(dt$`365DayCum-MBOE`, p = 0.8, list = FALSE)

train <- dt[trainRow, ] #create train set
test <- dt[-trainRow, ] #create test set

na_count <-sapply(test, function(y) sum(is.na(y))) #count the number of na per column
(na_percent <- data.frame(na_count)/nrow(test)) #na percent per column

##create a for loop to check the distribution of train/test set
dflist <- list(dt, train, test)                                 
dfname <- c("dt", "train", "test")

##for more information on for loops go to the website below
#http://r4ds.had.co.nz/iteration.html#for-loops

metric <- data.frame()  #create an empty data.frame to hold each loop
for(i in 1:length(dflist)){
  df <- data.frame(dflist[i])
  df_tbl <- data.frame(wellcount = nrow(df))   
  df_tbl$mean <- mean(df$X365DayCum.MBOE, na.rm = TRUE)   
  df_tbl$median <- median(df$X365DayCum.MBOE, na.rm = TRUE)
  df_tbl$variance <- var(df$X365DayCum.MBOE, na.rm = TRUE)
  df_tbl$sd <- sd(df$X365DayCum.MBOE, na.rm = TRUE)
  df_tbl$P10 <- quantile(df$X365DayCum.MBOE, probs = (0.90), na.rm = TRUE)
  df_tbl$P90 <- quantile(df$X365DayCum.MBOE, probs = (0.10), na.rm = TRUE)
  #df_tbl$SoPhiH <- mean(df$SoPhiH_LEF, na.rm = TRUE)
  #df_tbl$tvd <- mean(df$TotalDepthTVD, na.rm = TRUE)
  metric <- rbind(metric, df_tbl)#store the results of each loop
  #rownames(metric) <- dfname[i]
  #print(metric)
}
rownames(metric) <- dfname
metric

##model setup==============================================================================

##formula
sm_fmla <- paste("`365DayCum-MBOE` ~ STIMULATEDLATERALLENGTH + `ClusterSpacing(ft)` + Carbon13Isotope +", 
                 "Pressure + Fluidperft + `Sw-CALC` + `%LEF` + `Phi-CALC` + STAGESPACING + `NG_3-CALC` +",
                 "`NG_11-CALC` + MinWellSpacing + TEF_H + `VClay-CALC` + `SandLoading(#/ft)`")

fmla <- as.formula(sm_fmla)

mod_lm <- lm(`365DayCum-MBOE` ~ STIMULATEDLATERALLENGTH + `ClusterSpacing(ft)` + Carbon13Isotope + Pressure +
               Fluidperft + `Sw-CALC` + `%LEF` + `Phi-CALC` + STAGESPACING + `NG_3-CALC` + `NG_11-CALC` + 
               MinWellSpacing + TEF_H + `VClay-CALC` + `SandLoading(#/ft)`, data = train)

mod_lm <- lm(fmla, train)

summary(mod_lm)

data.frame(names(dt_num)) #get column number so you can exclude from model
##need to remove cum columns for backwards / forwards
names(dt_num[-c(1, 10:16, 18:19, 22, 23, 39)])

lmMod <- lm(`365DayCum-MBOE` ~ ., dt_num[-c(1, 10:16, 18:19, 22, 23, 39)])


step_back_Mod <- step(lmMod , direction = "backward")
step_forward_Mod <- step(lmMod, direction = "forward") #not sure if forward is working properly....
summary(step_back_Mod)
summary(step_forward_Mod)


##broom is a great package for working of lm models, for more information look at pdf below
##https://opr.princeton.edu/workshops/Downloads/2016Jan_BroomRobinson.pdf
tidy(mod_lm)
augment(mod_lm) 
glance(mod_lm)


##bootstrap==============================================================================

##create random samples with replacement
set.seed(1)
(sam <- sample(c(1:1000), size = 1000, replace=TRUE))

#test
#sam == 2

##create output placeholder for each loop
rmse <- vector("double", 1000)  #rmse in units of response variable
adj_r2 <- vector("double", 1000) #adj r2 since we have multiple explanatory variable
train_mean <- vector("double", 1000) #mean of response variable in train set
test_mean <- vector("double", 1000) #mean of response variable in test set
mod_coef <- matrix(0,1000,16) #matrix to hold coefficients from each loop

for(i in 1:1000){
  
  set.seed(sam[i]) 
  trainRow <- createDataPartition(dt$`365DayCum-MBOE`, p = 0.8, list = FALSE) #stratification on explanatory variable
  train <- dt[trainRow, ] #create train set
  test <- dt[-trainRow, ] #create test set
  train_mean[i] <- mean(train$`365DayCum-MBOE`) #mean of train
  test_mean[i] <- mean(test$`365DayCum-MBOE`) #mean of test

  mod_lm <- lm(fmla, data = train) #linear model
  pred_lm <- predict(mod_lm, test) #predict on test set
  rmse[i] <- sqrt(mean((test$`365DayCum-MBOE` - pred_lm)^2, na.rm = TRUE)) #calc rmse
  adj_r2[i] <- signif(summary(mod_lm)$adj.r.squared, 3) #retreive adj r2 from model
  mod_coef[i,] <- mod_lm$coefficients #retreive coefficients from model

}

##create a histrogram of rmse values with red line as your mean
hist(rmse, density=35, main = "Test RMSE over 1000 Samples", xlab = "Value of Obtained RMSE", col="blue", border="black")
abline(v=mean(rmse), lwd=3, col="red")

##create a histrogram of adj r2 values with red line as your mean
hist(adj_r2, density=35, main = "Test RMSE over 1000 Samples", xlab = "Value of Obtained RMSE", col="blue", border="black")
abline(v=mean(adj_r2), lwd=3, col="red")

colnames(mod_coef) <- names(coefficients(mod_lm)) #apply names of coefficients to matrix
con_int <- as.data.frame(apply(mod_coef, 2, function(x) quantile(x, c(0.025, 0.975)))) #calc confidence interval


mod_coef <- data.frame(mod_coef) #change mod_coef from matrix to data.frame to use with ggplot2
names(mod_coef) #display names of mod_coef

##make a denisty plot of coef ranges with 95% confindence interval
p1 <- ggplot(mod_coef, aes(x = X.NG_11.CALC.)) + geom_density(fill="blue", size=1) + 
  geom_vline(xintercept = c(127.6732, 192.4520), linetype = "solid", size = 2)

##make a denisty plot of coef ranges with 95% confindence interval
p2 <- ggplot(mod_coef, aes(x = X.NG_11.CALC.)) + geom_histogram(fill="red", size=1) +
  geom_vline(xintercept = c(127.6732, 192.4520), linetype = "solid", size = 2)

grid.arrange(p1, p2,ncol=1,nrow=2) #combine and display plots
