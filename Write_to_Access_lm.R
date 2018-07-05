rm(list = ls())

#load("C:/Users/mfarr/Documents/R_files/Spotfire.data/lm.RData")
load("C:/Users/mfarr/Documents/R_files/Spotfire.data/lm_access.RData")

AccessFilePath <- "C:/Users/mfarr/Documents/Spotfire.accdb"

suppressWarnings(library(dplry, warn.conflicts = FALSE))
suppressWarnings(library(RODBC, warn.conflicts = FALSE))



##input parameters
lm.data
aug.stats
lm.error
lm.stats
AccessFilePath




dataTable <- cbind(modname, lm.data)
augStats <- cbind(modname, aug.stats)
lmError <- cbind(modname, lm.error)
lmStats <- cbind(modname, lm.stats)



##write to access file============================================================
##user must have a odbc data source "Microsoft Access Driver (*.mdb, *.accdb)
##installed on their machine to communicate with Access

##load drivers, file location, and name of the table you want to save
driver <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)}"  ##load the odbc driver
#dLocation <- "C:/Users/mfarr/Documents/Spotfire.accdb"
dLocation <- AccessFilePath                                    ##file path from input table

##channel created for connection
ch <- odbcDriverConnect(paste(driver,';DBQ=',dLocation))

##save function to save table created to access
sqlSave(ch, dataTable, tablename = "dataTable", rownames = FALSE, append = TRUE)
sqlSave(ch, augStats, tablename = "augStats", rownames = FALSE, append = TRUE)
sqlSave(ch, lmError, tablename = "lmError", rownames = FALSE, append = TRUE)
sqlSave(ch, lmStats, tablename = "lmStats", rownames = FALSE, append = TRUE)

close(ch)



