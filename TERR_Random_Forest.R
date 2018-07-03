rm(list = ls())

##load spotfire data===================================================================

load("C:/Users/MFARR/Documents/R_files/Spotfire.data/terr_RF.RData")



# [TERR] Cluster with Variable Importance

# ----- Description --------------------------------------------------------------------------------------
# Accepts input table with numeric columns, and uses k-means clustering to find groups of rows where each
#    row belongs to the cluster with the nearest mean.
# This results in identifying each row with a "ClusterID" that can be used in Spotfire for further analysis.
# Next, a Random Forest model is built with the cluster ID as the response, to determine which variables
#    are most influential in determing the clusters.
# The names of the 2 most influential variables are returned. If these are captured in Spotfire as 
#    document properties, they can be used to dynamically control axes of a scatter plot.
# The VariableSummary table lists the variables, their importances, and whether a linear or log transformation
#    seems best.  If a logarithmic transform seems appropriate, this is applied prior to the clustering and 
#    variable importance calculation.

# References
# https://en.wikipedia.org/wiki/K-means_clustering
# https://en.wikipedia.org/wiki/Random_forest
# https://cran.r-project.org/web/packages/randomForest/index.html
# A. Liaw and M. Wiener (2002). Classification and Regression by randomForest. R News 2(3), 18--22. 

################################ License ################################
# TIBCO Component Exchange License
# Copyright (c) 2016 TIBCO Software Inc. All Rights Reserved.
# Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
# 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
# 3. Neither the name of TIBCO Software Inc.  nor the names of any contributors may  be used to endorse or promote products derived from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT OWNER AND CONTRIBUTORS  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE

# ----- Inputs/ Outputs ----------------------------------------------------------------------------------
# Input Parameters
#   Name          Structure                                                      Required
#   AnalysisData  Numeric Data table with arbitrary # and names of columns       yes
#                 Suggest sending multiple columns using Spotfire Expression
#                      $map("[AnalysisData].[${AnalysisColumns}]", ",")
#                   where AnalysisColumns is a document property limited through
#                      datatype:Real or datatype:Integer
#   user.NClusters     integer value                                             no (default=0)
# Output Parameters
#   Name                   Structure              Description
#   ClusterID              column                 Column holding the cluster identifier for each incoming data row
#                                                    Suggest returning this back to original table as new column
#   TopClusterVariable1    string value           Name of most important variable in determining cluster
#   TopClusterVariable2    string value           Name of 2nd most important variable
#   VariableSummary        Table with 4 columns
#                                                 variable   = name of incoming variable
#                                                 importance = numerical measure of how important each variable is.
#                                                   (Mean Decrease in Accuracy)
#                                                 transform  = either "Linear" or "Logarithmic
#   clusterMetrics         Table
#   N.clusters             integer value          Best # clusters using Hartigan method

#   status.message         string value           Any error messages regarding missing R packages or other configuration issues

# ----- Versions, Dates, Author ---------------------------------------------------------------------------
# Developed and tested with these code versions:
#   TERR Version: TERR 4.1
#   Package randomForest: Version 4.6-10

# Creation Date: "Wed Mar 30 16:50:28 2016"
# Last Modified: "Fri May 13 13:40:57 2016"

# Author: Peter Shaw
# Contact Info: pshaw@tibco.com

# ----- Save Data Snapshot ------------------------------------------------------------------------------
# Save a snapshot of the environment to an RData file for code development.  Comment out when finished:
#TimeStamp=paste(date(),Sys.timezone())
#if(file_test("-d", "C:/Temp")) suppressWarnings(try(save(list=ls(), file="C:/Temp/cluster.in.RData", RFormat=T )))
# remove(list=ls()); load(file='C:/Temp/cluster.in.RData'); print(TimeStamp) # use in development

# ----- Begin Function Definitions ----------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------
CheckLoadPackage = function(packagename){
  # Function to be used inside data functions, for loading one R package.
  # Usage:
  #   checkLoadPackage(packagename)
  # Arguments:
  #   packagename
  #     (string) The name of one package.
  # Details
  #   checkLoadPackage tests to see if the package can be loaded.
  #   If the package can be loaded, it will be; 
  #   If the package is not available to be loaded, a message is generated to instruct the user how to obtain the package.
  # Value
  #   checkLoadPackage returns a list with two components:
  #     Report.vec is a string vector that can be included with other messages and finally
  #       concatenated with paste(Report.vec,collapse="\n") to return to Spotfire
  #   continue is a Boolean value on whether the package was successfully loaded.
  # Examples
  #   package.result <- checkLoadPackage("randomForest")
  #		message <- paste(package.result$Report.vec, collapse="\n)
  #		continue <- package.result$continue
  # 
  # Peter Shaw
  #  "Wed Mar 30 10:31:15 2016"
  suppressWarnings(  suppressPackageStartupMessages( packageTest <- library(packagename, character.only=TRUE, logical.return=TRUE)))
  if(packageTest){
    # If require() returns TRUE it's already loaded the package successfully.
    Report.vec = paste("package",packagename,"loaded")
    continue=TRUE
  }else{
    Report.vec = c(
      paste('R package ',packagename,' needs to be installed.',sep=''),
      'Please go to Tools -> TERR Tools;',
      'select "Package Managment" tab;',
      'hit "Load" to populate list of available packages;',
      paste('find ',packagename,' in this list, highlight it, then click "Install".',sep=''),
      'Close the TERR Tools dialog and try hitting "Recalculate" again.'
    )
    continue=FALSE
  }
  return(list(Report.vec=Report.vec, continue=continue))
}
# ---------------------------------------------------------------------------------------------------------------------
CleanData = function(AnalysisData, max.missingprop=0.9){
  
  # AnalysisData is your (numerical) data table.
  # max.missingprop is the maximum proportion of columns with missing values, for dropping rows.
  
  # AnalysisDataClean = CleanData(AnalysisData)
  
  # Peter Shaw
  # "Fri May 06 11:35:38 2016"
  
  # Achtung, need categorical handling
  # Test for string vs numerical columns
  # Integer columns: Test for # levels possibly convert to factor
  
  continue=TRUE
  
  # In this version, only keep numeric columns.
  col.classes = sapply(AnalysisData,class)
  col.keep = which(col.classes %in% c("numeric","integer"))
  if(length(col.keep)>0){
    AnalysisDataClean = AnalysisData[,col.keep,drop=F] # trim down to just numeric columns
  }else{
    AnalysisDataClean = data.frame()
    continue=FALSE
  }
  
  if(continue){
    
  }
  # ----- Possibly delete entire rows if not enough data present 
  missing.proportion = rowSums(do.call("cbind",lapply(
    X=AnalysisDataClean,
    FUN=function(x){
      return(as.numeric(is.na(x)))
    }
  ))) / ncol(AnalysisDataClean)
  
  # Possibly snuff some entire rows
  row.keep.bool = missing.proportion < max.missingprop
  # Generate UniqueID first
  UniqueID = 1:nrow(AnalysisDataClean)
  UniqueID.out = UniqueID[row.keep.bool]
  AnalysisDataClean = AnalysisDataClean[row.keep.bool,,drop=FALSE]
  
  # If number of rows is 0 here we're done
  if(nrow(AnalysisDataClean)){
    # Test for all-missing; possibly drop entire column
    col.keep.bool = sapply(X=AnalysisDataClean, FUN=function(x){ !all(is.na(x))})
    AnalysisDataClean = AnalysisDataClean[,col.keep.bool,drop=FALSE]
    
    # Now fill in missing values
    AnalysisDataClean = as.data.frame(do.call("cbind",lapply(
      X=AnalysisDataClean,
      FUN=function(x){
        xmiss = !is.finite(x) # triggers on NA, 1/0, log(0) etc
        if(any(xmiss)){
          x[xmiss] = median(x[!xmiss])
        }
        return(x)
      }
    )))
    # drop columns if variance=0
    # library(terrUtils)
    col.keep.bool = colVars(AnalysisDataClean)>0
    AnalysisDataClean = AnalysisDataClean[,col.keep.bool,drop=FALSE]
  }
  
  if(nrow(AnalysisDataClean)==0) continue=FALSE
  if(ncol(AnalysisDataClean)==0) continue=FALSE
  
  return(list(
    continue = continue,
    AnalysisDataClean = AnalysisDataClean,
    rowKeep = which(row.keep.bool),
    UniqueID     = UniqueID,
    UniqueID.out = UniqueID.out
  ))
}
# ---------------------------------------------------------------------------------------------------------------------
LogData = function(AnalysisDataClean){
  # Possibly takes log of data. Do this sucker after cleaning.
  # AnalysisDataLog = LogData(AnalysisDataClean)
  
  # Peter Shaw
  # "Thu Mar 24 13:45:47 2016"
  
  # library(terrUtils)
  # If all values are >=0, take the log.
  logTest.vec = colMins(AnalysisDataClean) >=0
  logTest = data.frame(
    variable = names(logTest.vec),
    transform = c("Linear","Logarithmic")[1+as.numeric(logTest.vec)],
    stringsAsFactors=F
  )
  
  AnalysisDataLog = do.call("cbind.data.frame",lapply(
    X=AnalysisDataClean,
    FUN=function(x){
      if(min(x)>=0){ #duplicated test
        x.eps = max(x)/100
        z=log(x+x.eps)
      }else{
        z=x
      }
      return(z)
    }
  ))
  result = list(
    logTest = logTest,
    AnalysisDataLog = AnalysisDataLog
  )
}

# ---------------------------------------------------------------------------------------------------------------------
Best.Hartigan.Cluster = function(AnalysisDataScaled, user.NClusters=0){
  # Peter Shaw
  # loop over K for kmeans
  
  # use automatic method to find best K.
  # New stuff 4-2016
  N.data = nrow(AnalysisDataScaled)
  K.max = ncol(AnalysisDataScaled)
  if(K.max>1){
    # Run the loop of kmeans over all k to test, even if user specified something.
    vec.K = 1:K.max
    kmeans.list = lapply(
      X=vec.K,
      #X=2:pmin(8,ncol(AnalysisDataScaled)),
      FUN=function(K,AnalysisDataScaled){
        print(K)
        if(K==1){
          result = list(tot.withinss=NA, betweenss=NA, totss=NA) # replace below
        }else{
          result = kmeans(x=AnalysisDataScaled, centers=K, nstart=10)
        }
        return(result)
      },
      AnalysisDataScaled=AnalysisDataScaled
    )
    
    # only here when ncol()>1
    vec.withinss  = sapply(X=kmeans.list,FUN=function(x){return(x$tot.withinss)})
    vec.betweenss = sapply(X=kmeans.list,FUN=function(x){return(x$betweenss)})
    vec.totss     = sapply(X=kmeans.list,FUN=function(x){return(x$totss)})
    
    vec.withinss[1] = vec.totss[2]
    vec.totss[1] = vec.totss[2]
    
    u.k = vec.K[-length(vec.K)]
    u.kplus1 = vec.K[-1]
    
    # Starts at k=2
    Hartigan.metric = (vec.withinss[u.k]/vec.withinss[u.kplus1]-1) * (N.data - vec.K[u.k]-1)
    Hartigan.K = vec.K[u.kplus1] # the candidate K being tested
    
    # Prepare table for informational
    clusterMetrics = data.frame(
      Number = vec.K,
      "Within-Cluster SS" = vec.withinss,
      "Between-Cluster SS" = vec.betweenss,
      Hartigan = c(NA,Hartigan.metric),
      "Hartigan Threshold" = c(NA,Hartigan.metric) - 10,
      "Hartigan flag" = c(NA,as.numeric(Hartigan.metric>10)),
      check.names=F
    )
    
    # from kmeans doc; test for >10:
    # ( sum(k$withinss) / sum(kplus1$withinss) -1 )  *  (nrow(x)-k-1)
    
    
    # Figure out max K from Hartigan method
    u.candidates = which(Hartigan.metric>10)
    if(length(u.candidates)==0){
      K.use=1 #none of the add-on candidates met the Hartigan criterion
      cluster.use = rep(1,nrow(AnalysisDataScaled))
    }else{
      K.use.H = Hartigan.K[length(u.candidates)] # use the last one
      if(user.NClusters>0){
        # use user-supplied N (limit to <= K.use.H)
        K.use = min(K.use.H, user.NClusters) # allow user to choose k <= Hartigan K
      }else{
        K.use = K.use.H
      }
      if(K.use==1){
        cluster.use = rep(1,nrow(AnalysisDataScaled)) # kmeans.list[[1]] is not a kmeans object
      }else{
        cluster.use = fitted(kmeans.list[[K.use]],method="classes")
      }
    }
    
    cluster.vec <- match(cluster.use, order(-table(cluster.use))) # from SPK
  }else{
    # Just one incoming column
    K.use=1
    clusterMetrics = data.frame(
      Number = 1,
      "Within-Cluster SS" = sum(AnalysisDataScaled[,1]^2),
      "Between-Cluster SS" = NA,
      Hartigan = NA,
      "Hartigan Threshold" = NA,
      "Hartigan flag" = NA,
      check.names=F
    )
    cluster.vec = rep(1,N.data)
  }
  return(list(
    N.clusters = as.integer(K.use),
    clusterMetrics = clusterMetrics,
    cluster.vec = cluster.vec
  ))
}
# ---------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------
# ----- End Function Definitions ------------------------------------------------------------------------


# ----- Provide default values to optional inputs not specified ----------------------------------------
if(is.null(user.NClusters))  user.NClusters = 0 # 0 signals use maximum number by Hartigan method.

# ----- create null outputs in case all else fails -----------------------------------------------------
# Make null results
TopClusterVariable1 = ""
TopClusterVariable2 = ""
ClusterID = data.frame(ClusterID=rep("",nrow(AnalysisData)))
VariableSummary = data.frame(
  variable   = character(0),
  importance = numeric(0),
  transform  = character(0),
  #index      = integer(0), 
  stringsAsFactors = F
)
clusterMetrics = data.frame(
  Number = 1,
  WithinSS = NA,
  BetweenSS = NA,
  Hartigan = NA,
  "Hartigan Threshold" = NA
)
N.clusters = as.integer(0)
status.message.vec = character(0)
continue=TRUE # initialize



# ----- Package Loading --------------------------------------------------------------------------------
packageResult = CheckLoadPackage("randomForest")
continue = packageResult$continue
if(!continue) status.message.vec = c(status.message.vec, packageResult$Report.vec) # only annotate if package didn't load

# library(terrUtils) # for interactive use e.g. for functions like colVars() that are loaded at runtime

# ----- Input Data Checking ----------------------------------------------------------------------------
if(continue){
  if(nrow(AnalysisData)==0) {
    continue=FALSE
    status.message.vec = c(status.message.vec,"No rows in input data")
  }
  if(ncol(AnalysisData)==0) {
    continue=FALSE
    status.message.vec = c(status.message.vec,"No columns in input data")
  }
}

if(continue){
  CleanedList = CleanData(AnalysisData)
  AnalysisDataClean = CleanedList$AnalysisDataClean
  continue          = CleanedList$continue
  if(!continue){
    status.message.vec = c(status.message.vec,"Incoming data failed cleaning step")
  }
}


# ----- Begin main section of data function ------------------------------------------------------------

set.seed(1) # process has random element.  Setting seed adds some repeatability
if(continue){
  LogResult = LogData(AnalysisDataClean)
  
  logTest = LogResult$logTest
  AnalysisDataLog = LogResult$AnalysisDataLog
  
  
  AnalysisDataScaled = scale(x=AnalysisDataLog, center = FALSE, scale = TRUE)
  
  # Use the R function kmeans() for cluster analysis of scaled data within this function:
  Hartigan.Result = Best.Hartigan.Cluster(AnalysisDataScaled=AnalysisDataScaled, user.NClusters=user.NClusters)  
  N.clusters      = Hartigan.Result$N.clusters
  cluster.numID   = Hartigan.Result$cluster.vec
  clusterMetrics  = Hartigan.Result$clusterMetrics 
  
  # Format the labels
  N.clusterdigits = nchar(as.character(N.clusters)) 
  cluster.ID.internal = sprintf( paste("Segment", "%0",N.clusterdigits,".0f",sep="") ,cluster.numID)
  
  # Assemble output column of ID
  clusterID.vec = rep("",nrow(AnalysisData))
  clusterID.vec[CleanedList$rowKeep] = cluster.ID.internal
  ClusterID = data.frame(ClusterID=as.character(clusterID.vec), stringsAsFactors = F)
  
  # Follow this by randomForest. Prepare data.
  work.data.rf = data.frame(
    Fitted.KMeans.Cluster = factor(cluster.numID),
    AnalysisDataScaled,
    check.names=F
  )
  
  # Prepare table to map actual names to R-friendly names
  variableNames = colnames(work.data.rf)
  colnames(work.data.rf) = make.names(colnames(work.data.rf), unique=TRUE)
  RvariableNames= colnames(work.data.rf)
  
  if(length(table(cluster.numID)) > 1){
    cluster.rf = suppressWarnings(randomForest( Fitted.KMeans.Cluster ~ ., data=work.data.rf , importance=TRUE))
    cluster.importance.obj = importance(cluster.rf) # R versions of original names are stored as row names
    
    Cluster.Variable.Importances = merge(
      x=data.frame(
        Rvariable = row.names(cluster.importance.obj),
        importance = as.numeric(cluster.importance.obj[,"MeanDecreaseAccuracy"]), # "MeanDecreaseGini" other choice
        stringsAsFactors=F
      ), 
      by.x="Rvariable",
      y=data.frame(
        variable=variableNames, 
        Rvariable=RvariableNames, stringsAsFactors=F
      ), 
      by.y="Rvariable"
    )
    
    # Toss in the log/linear transformation indicator; only keep interesting columns.
    VariableSummary = merge(
      x=LogResult$logTest, by.x="variable",
      y=Cluster.Variable.Importances, by.y="variable"
    )[,c("variable","importance","transform")]
    
    # re-order rows by decreasing importance:
    VariableSummary = VariableSummary[rev(order(VariableSummary$importance)),]
    
    if(nrow(VariableSummary)>0) TopClusterVariable1 = VariableSummary$variable[1]
    if(nrow(VariableSummary)>1) TopClusterVariable2 = VariableSummary$variable[2]
  } else {
    # only one class in the cluster, can't do variable importances
    VariableSummary = data.frame(
      variable = variableNames,
      importance = rep(NA,length(variableNames)),
      transform  = rep("",length(variableNames))
    )
    # already have null top variables defined.
  }
  
} # end of main continue test

#if(file_test("-d", "C:/Temp")) suppressWarnings(try(save(list=ls(), file="C:/Temp/cluster.out.RData", RFormat=T )))
# remove(list=ls()); load(file='C:/Temp/cluster.out.RData'); print(TimeStamp)

# ----- Wrap up ---------------------------------------------------------------------------------------
if(length(status.message.vec)==0){
  status.message = "OK"
}else{
  status.message = paste(status.message.vec,collapse="\n")
}


# [TERR] Cluster with Variable Importance
