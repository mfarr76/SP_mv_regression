rm(list = ls())

load("C:/Users/mfarr/Documents/R_files/Spotfire.data/corr_plot.RData")
load("C:/Users/mfarr/Documents/R_files/Spotfire.data/daily_tables.RData")
response <- "GasEURBCF"

suppressWarnings(library(dplyr, warn.conflicts = FALSE))
suppressWarnings(library(corrplot, warn.conflicts = FALSE))
suppressWarnings(library(RinR, warn.conflicts = FALSE))


ActualPathToOpenSourceR <- "C:/Program Files/R/R-3.5.0/bin/R.exe"

options( RinR_R_FULL_PATH = ActualPathToOpenSourceR )

##input parmeters
join
response
input

##create data table====================================================


output <- data.frame(strsplit(input, ","))
names(output) <- "MAIN"

dt <- join[response]


for(i in 1:nrow(output))
{
  idx <- join[which(colnames(join)==as.character(output[i,1]))]
  dt <- cbind(dt, idx)
}

dt <- dt %>% filter(!is.na(.[response]))

dt <- bind_cols(join %>% 
                  filter(!is.na(join[response])) %>% 
                  select(LEASE), 
                dt)


##data prep=============================================================
#dt <- join %>%
  #filter(!is.na(join[response]))
#  filter(!is.na(SwCALC), 
#         !is.na(initialwhpsiavg), 
#         !is.na(distancebtwnstagesftavg), 
#         !is.na(perfclusterspacing))

na_count <- sapply(dt, function(y) sum(is.na(y)))
(na_percent <- data.frame(na_count)/nrow(dt))
#names(public[,na_percent<0.95])
#training_remove_sparse_records<-public[,na_percent<0.95]
dt <- dt[,na_percent==0]
dt$model_H <- NULL

numericVars <- which(sapply(df, is.numeric)) #index vector numeric variables
dt <- dt[, numericVars]

##create correlation matrix
correlate <- cor( dt, use = "everything" )


corr.plot<- RGraph(print(corrplot(correlate, method="circle", type="lower",  
                                  sig.level = 0.01, insig = "blank")), 
                   display = FALSE,
                   data = c("correlate"),
                   packages = c("dplyr","corrplot"), 
                   height = 1000, width = 1000)

corrName <- data.frame(colName = rownames(correlate))

#correlate <- cbind(ColName = rownames(correlate), correlate)
correlate <- cbind(corrName, correlate)

TimeStamp=paste(date(),Sys.timezone())
tdir = 'C:/Users/MFARR/Documents/R_files/Spotfire.data' # place to store diagnostics if it exists (otherwise do nothing)
if(file.exists(tdir) && file.info(tdir)$isdir) suppressWarnings(try(save(list=ls(), file=paste(tdir,'/corr_plot.RData',sep=''), RFormat=T )))

























