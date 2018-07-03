rm(list = ls())
download.file("https://spark-public.s3.amazonaws.com/dataanalysis/cd4.data", 
              destfile ="cd4.data")#, method="curl")
cd4Data <- read.table("cd4.data", 
              col.names=c("time", "cd4", "age", "packs", "drugs", "sex", "cesd", "id"))

cd4Data <- cd4Data[order(cd4Data$time),]

plot(cd4Data$time, cd4Data$cd4, pch=19, cex=0.1)
aveTime <- aveCd4 <- rep(NA, length(201: (dim(cd4Data)[1]-200)))
for(i in 201:(dim(cd4Data)[1]-2)){
  aveTime[i] <- mean(cd4Data$time[(i-200):(i+200)])
  aveCd4[i] <- mean(cd4Data$cd4[(i-200):(i+200)])
}
lines(aveTime, aveCd4, col="blue", lwd=3)

filtTime <- as.vector(filter(cd4Data$time, filter = rep(1,200))/200)
filtCd4 <- as.vector(filter(cd4Data$cd4, filter=rep(1,200))/200)
plot(cd4Data$time, cd4Data$cd4, pch=19, cex=0.1); lines(filtTime, filtCd4, col="blue", lwd=3)
