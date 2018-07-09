rm(list = ls())

library(caret, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)

dt <- read.csv("dt.csv")
train <- read.csv("train.csv")


sapply(train, class)

dt1 <- dt[-1]
train1 <- train[-1]

##create a list of columns that have a correlation higher than 0.87
highCor_dt <- names(dt1[,findCorrelation(abs(cor(dt1)), 0.85)])
highCor_train1 <- names(train1[,findCorrelation(abs(cor(train1)), 0.85)])

dt1[,highCor_dt] <- NULL
train1[, highCor_train1] <- NULL


mod <- lm(AcK_mbt_EarlyTime ~., dt1)
(summary(mod))

mod_test <- lm(AcK_mbt_EarlyTime ~., train1)
(summary(mod_test))

par(mfrow=c(2,2))
plot(mod)

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

