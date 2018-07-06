rm(list = ls())


dt <- read.csv("dt.csv")
train <- read.csv("train.csv")


mod <- lm(AcK_mbt_EarlyTime ~., dt[-1])
summary(mod)

mod_test <- lm(AcK_mbt_EarlyTime ~., train[-1])
summary(mod_test)
