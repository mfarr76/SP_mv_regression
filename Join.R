rm(list = ls())

load("C:/Users/mfarr/Documents/R_files/Spotfire.data/daily_tables.RData")


##join adapt rta table

##input parameters
geo
rta
comp
wellhead


suppressWarnings(library(dplyr,warn.conflicts=FALSE))


names(geo) <- gsub("-","", names(geo))
names(geo) <- gsub(" ", "_", names(geo))
names(geo) <- gsub("[()]","", names(geo))
names(geo) <- gsub("_","", names(geo))
names(comp) <- gsub(" ","",  names(comp))


join <- left_join(geo, rta, by = c("AMAPI" = "API")) %>%
  rename(LEF_PER = `%LEF`,
         UEF_PER = `%UEF`) %>% 
  left_join(., wellhead %>%
              select(API, 
                     Develop = REGION2, 
                     BTM_LON, 
                     BTM_LAT), by = c("AMAPI" = "API")) %>%
  left_join(., comp %>%
              mutate(Mesh100_Perc = proppantsize100qty / proppanttotalqty,
                     Mesh4070_Perc = proppantsize4070qty / proppanttotalqty,
                     Mesh3050_Perc = proppantsize3050qty / proppanttotalqty, 
                     Mesh2040_Perc = proppantsize2040qty / proppanttotalqty) %>%
              select(Mesh100_Perc, Mesh3050_Perc, Mesh4070_Perc, Mesh2040_Perc, wellcompletionapi), 
            by = c("AMAPI" = "wellcompletionapi")) %>%
  mutate(spacing_prodyear_bin = case_when(PRODYRAVGWELLSPACING < 200 ~ "200", 
                                          PRODYRAVGWELLSPACING > 400 | PRODYRAVGWELLSPACING < 600 ~ "400-600", 
                                          PRODYRAVGWELLSPACING > 600 | PRODYRAVGWELLSPACING < 800 ~ "600-800", 
                                          PRODYRAVGWELLSPACING > 800 | PRODYRAVGWELLSPACING < 1000 ~ "800-1000",
                                          PRODYRAVGWELLSPACING > 800 | PRODYRAVGWELLSPACING < 1000 ~ "1000-1200",
                                          TRUE ~ "1200"))

join %>% mutate(spacing = case_when(PRODYRAVGWELLSPACING < 400 ~ "400")) %>%
  select(PRODYRAVGWELLSPACING , spacing) 

##create a Rdata file to load in R========================================================
TimeStamp=paste(date(),Sys.timezone())
tdir = 'C:/Users/MFARR/Documents/R_files/Spotfire.data' # place to store diagnostics if it exists (otherwise do nothing)
if(file.exists(tdir) && file.info(tdir)$isdir) suppressWarnings(try(save(list=ls(), file=paste(tdir,'/join_daily.RData',sep=''), RFormat=T )))
