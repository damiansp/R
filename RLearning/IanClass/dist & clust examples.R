#this initial code is an experiment
#in using R's tools for converting binary
#observations into distance measures

#the data table used in ppt slides...
v1 <- c(0, 0, 0)
v2 <- c(0, 0, 1)
v3 <- c(1, 0, 1)
t1 <- data.frame(v1, v2, v3); t1

#using the distance function in R-base
dist(t1, method="binary") #appears to be same as JCC

#load cluster package to get at "daisy"
library(cluster)
#jaccards; same as dist/binary method
daisy(t1, metric="gower", type=list(asymm=1:3))
#simple matching coefficient
daisy(t1, metric="gower", type=list(symm=1:3))

#*********************************************************
#*********************************************************
#an experiment with Aldenderfer and Blashfield's burial data
ald <- read.table("F:/D830 backups Fall 2008/Robertson/courses/Data Analysis/Data Analysis 2009/lectures/aldenderfer burial.dat", header=T)
str(ald)

#isolate the metric data
ald2 <- ald[,6:13]; ald2

#calculate various distance matrices
smc <- daisy(ald2, metric="gower", type=list(symm=1:8))
jcc <- daisy(ald2, metric="gower", type=list(asymm=1:8))
euc <- dist(ald2, method="euclidean")

#comparing different distance measures
plot(jcc, smc)
plot(smc, euc)
plot(jcc, euc)

#various experiments with distance measures...
hc2 <- hclust(smc, method="ward")
plot(hc2, hang=0, labels=ald$comp, cex=.6, main="SMC w/ Wards method")

hc3 <- hclust(jcc, method="ward")
plot(hc3, hang=0, labels=ald$comp, cex=.6, main="Jaccards w/ Wards method")
ct4 <- cutree(hc3, 4)
table(ct4, ald$age)
(cutree(hc3, h=2.5))
(cutree(hc3, h=1.6))

#an interesting solution?
hc2 <- hclust(euc, method="ward")
plot(hc2, hang=0, labels=ald$comp, cex=.6, main="EucD w/ Wards method")
ct4 <- cutree(hc2, 4)
table(ct4, ald$sex)

#more experiments with linkage methods
hc2 <- hclust(euc, method="single")
plot(hc2, hang=0, labels=ald$comp, cex=.6, main="EucD w/ single linkage")

hc2 <- hclust(euc, method="complete")
plot(hc2, hang=1, labels=ald$comp, cex=.6, main="EucD w/ complete linkage")

hc2 <- hclust(euc, method="average")
plot(hc2, hang=0, labels=ald$comp, cex=.6, main="EucD w/ average linkage")

#*********************************************************
#*********************************************************
#k-means example using Mandara knife data

#load the data (from appropriate path)
kf1 <- read.table("F:/D830 backups Fall 2008/Robertson/courses/Data Analysis/Data Analysis 2009/lectures/knife data.dat", header=T)

#check it out...
str(kf1)
summary(kf1)

#isolate some data
L1 <- kf1$L1
W1 <- kf1$W1
T1 <- kf1$T1

#how are these things measured?
boxplot(kf2)

#convert data to z-scores
L2 <- scale(kf1$L1)
W2 <- scale(kf1$W1)
T2 <- scale(kf1$T1)

#kmeans cluster work
km2 <- kmeans(kf3, 2) # 2 cluster solution
km2$cluster
table(km2$cluster, kf1$LOC_LAB)

km3 <- kmeans(kf3, 3) # 3 cluster solution
table(km3$cluster, kf1$LOC_LAB)

#examine cluster constitution using boxplots
boxplot(L2 ~ km3$cluster)
boxplot(W2 ~ km3$cluster)

#examine cluster constitution using scatterplots

kmN <- kmeans(kf3, 3)
palette()
plot(L2, W2, type="n", main="")
  for (i in 1:max(kmN$cluster)) {
    points(L2[kmN$cluster==i], W2[kmN$cluster==i], cex=2, col=i, pch=19)
  }

#assessing SSE drop in multiple kmeans runs
km.plot(kf3, 15)
km4 <- kmeans(kf3, 7)
table(km4$cluster, kf1$LOC_LAB)

#plot %SSE data for a series of K-means solutions...
#pass input data and maximum # of clusters
#desired as parameters
#IGR March, 2006
km.plot <- function(m1, maxcls) {
  #prepare vectors for plotting cluster number vs. %SSE...
  out.clsnum <- c(1:maxcls)
  out.psse <- NULL

  #calculate pre-cluster SSE...
  mv <- apply(m1, 2, mean)
  maxsse <- sum(sweep(m1,2,mv)^2)
  out.psse[1]=100

  #calculate series of k-means solutions...
  for (i in 2:maxcls){
    cl <- kmeans(m1, i)
    #express withinss as fraction of total (pre-cluster) SSE...
    out.psse[i] <- sum(cl$withinss)/maxsse*100
  } #end of clustering loop

#dump the results...
plot(out.clsnum,out.psse, type='l', xlab="cluster solution", ylab="%SSE")

} #end of km.plot function

#calculate distance from cluster centroid
#for a single K-means solution
#pass input data and km cluster object as parameters
#IGR March, 2007
km.dist <- function(input, clsObj) {
  d <- input - clsObj$centers[clsObj$cluster, ]
  dsq <- d^2
  dst <- sqrt(rowSums(dsq))
  dst
} #end of km.dist function
