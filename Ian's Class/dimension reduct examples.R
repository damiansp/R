#MANDARA REGION KNIFE MORPHOLOGY
mnd <- read.table("F:/D830 backups Fall 2008/Robertson/courses/Data Analysis/Data Analysis 2009/lectures/knife data.dat", header=T)
mnd

#remove 4 cases with missing data
#mnd <- mnd[-c(9,51,54,55),]
#an easier way...
complete.cases(mnd)
mnd <- mnd[complete.cases(mnd),]

#create factors for ethnicity and location
ethfct <- factor(mnd[,2]); table(ethfct)
locfct <- factor(mnd[,3]); table(locfct)
#just check the factors...
table(locfct, ethfct)

#isolate the handle morphology data
handle <- mnd[,4:24]
#a bit too optimistic
pairs(handle, pch=".")
round(cor(handle),2)

#do a PCA on handle data
pca1 <- princomp(handle, cor=T)
summary(pca1)

#look at the eigenvalues
plot(pca1)
#a clearer way
plot(pca1$sdev^2, type="b")
sum(pca1$sdev^2)/length(pca1$sdev^2)
abline(h=1, lty=2); abline(h=.7)

#explore the loadings
pca1$loadings
round(pca1$loadings[,1:3], 2)
#sort loadings for 1st component only
sort(pca1$loadings[,1])
#sort various columns together...
pca1$loadings[order(pca1$loadings[,1]), 1:3]

#plot the loadings as a scatter plot
varlist <- c("L1","L2","L3","L4","L5","L6","L7","L8","L9","L10","W1","W2","W3","W4","W5","T1","T2","T3","D1","D2","D3")
plot(pca1$loadings[,1], pca1$loadings[,2], type="n")
text(pca1$loadings[,1], pca1$loadings[,2], varlist, cex=.75)

plot(pca1$loadings[,2], pca1$loadings[,3], type="n")
text(pca1$loadings[,2], pca1$loadings[,3], varlist, cex=1)

#explore the PC scores
round(pca1$scores[,1:2], 2)

plot(pca1$scores[,1], pca1$scores[,2], type="n")
text(pca1$scores[,1], pca1$scores[,2], labels=abbreviate(ethfct, minlength=1), cex=.5)

plot(pca1$scores[,1], pca1$scores[,2], type="n")
text(pca1$scores[,1], pca1$scores[,2], labels=abbreviate(locfct, minlength=1), cex=.5)

#Touchki
locx <- pca1$scores[,1][locfct=="TKI"]
locy <- pca1$scores[,2][locfct=="TKI"]
lhull <- chull(locx, locy)
polygon(locx[lhull], locy[lhull], density=0)

#Double
locx <- pca1$scores[,1][locfct=="DBL"]
locy <- pca1$scores[,2][locfct=="DBL"]
lhull <- chull(locx, locy)
polygon(locx[lhull], locy[lhull], density=0)

#Manaouatchi area
locx <- pca1$scores[,1][locfct=="MME" | locfct=="MNA"]
locy <- pca1$scores[,2][locfct=="MME" | locfct=="MNA"]
lhull <- chull(locx, locy)
polygon(locx[lhull], locy[lhull], density=0)

locx <- pca1$scores[,1][locfct=="BAM"]
locy <- pca1$scores[,2][locfct=="BAM"]
lhull <- chull(locx, locy)
polygon(locx[lhull], locy[lhull], density=0)

#Banki
locx <- pca1$scores[,1][locfct=="BNK"]
locy <- pca1$scores[,2][locfct=="BNK"]
lhull <- chull(locx, locy)
polygon(locx[lhull], locy[lhull], density=0)
#end of MANDARA REGION KNIFE MORPHOLOGY
#*****************************************************
#YELLEN ETHNOARCHAEOLOGY DATA
library(foreign)
yellen <- read.systat("F:\\D830 backups Fall 2008\\Robertson\\courses\\Data Analysis\\Data Analysis 2008\\lectures\\yellen data.syd")

yellen
yellen <- yellen[-8,]
yellen <- yellen[,2:7]

#look at the correlations
pairs(yellen, pch=20, col="red")
round(cor(yellen), 2)

#go ahead with the PCA
pcyR <- princomp(scale(yellen), cor=T)
pcyR <- princomp(yellen, cor=T)
summary(pcyR)
pcyR

#plot the eigenvalues
plot(pcyR)
plot(pcyR$sdev^2, type="b", xlab="principal component", ylab="eigen value")
abline(h=1, lty=2); abline(h=.7)

#screeplot for variance
plot(pcyR$sdev^2/length(pcyR$sdev), type="b", xlab="principal component", ylab="% of variance")

#examine (quickly) the case scores
round(pcyR$scores, 3)
plot(pcyR$scores[,1], pcyR$scores[,2], type="n")
text(pcyR$scores[,1], pcyR$scores[,2], labels=c(1:16))

#explore the loadings (more interesting!)
pcyR$loadings

#sort various columns together...
round(pcyR$loadings[order(pcyR$loadings[,1]), 1:3], 2)
round(pcyR$loadings[order(pcyR$loadings[,2]), 1:3], 2)

#plot(pcyR$loadings[,1], rep(1,6), type="n")
#text(pcyR$loadings[,1], rep(1,6), labels=colnames(yellen), cex=.5)

plot(pcyR$loadings[,1], pcyR$loadings[,2], type="n",
  xlab="PCA Axis 1", ylab="PCA Axis 2")
#text(pcyR$loadings[,1], pcyR$loadings[,2], labels=c("L","I","P","F","A","B"))
text(pcyR$loadings[,1], pcyR$loadings[,2], labels=colnames(yellen), cex=.5)

#EXPERIMENTAL rotation work using prcomp
pc2 <- prcomp(yellen, scale.=T)
summary(pc2)

plot(pc2)
plot(pc2$rotation[,1], pc2$rotation[,2], type="n")
text(pc2$rotation[,1], pc2$rotation[,2], labels=colnames(yellen), cex=.5)

rotTest <- varimax(pc2$rotation)

rotTest
rotTest$rotmat
plot(rotTest$rotmat[,1], rotTest$rotmat[,2], type="n")
text(rotTest$rotmat[,1], rotTest$rotmat[,2], labels=colnames(yellen), cex=.5)

rotTest$loadings
plot(rotTest$loadings[,1], rotTest$loadings[,2], type="n")
text(rotTest$loadings[,1], rotTest$loadings[,2], labels=colnames(yellen), cex=.5)
#end of YELLEN ETHNOARCHAEOLOGY DATA
#*****************************************************
#CORRESPONDENCE ANALYSIS WORK

#Israeli worries
source("E:\\Robertson\\courses\\SU Data Analysis 2008\\lectures\\CA analysis.IGR.R")
isw <- read.table("E:\\Robertson\\courses\\SU Data Analysis 2008\\lectures\\israeli worries.dat", skip=23); isw

rownames(isw) <- c("ENR","SAB","MIL","POL","ECO","OTH","MTO","PER")
colnames(isw) <- c("As/Af","Eu/Am","Is/AA","Is/EA","Is/Is")
isw
clab <- c("As/Af","Eu/Am","Is/AA","Is/EA","Is/Is")
rlab <- c("ENR","SAB","MIL","POL","ECO","OTH","MTO","PER")

ca1 <- ca.igr(isw)

#plot col and row projections, CAA1&2
xlm <- range(ca1$cproj[,1], ca1$rproj[,1])
ylm <- range(ca1$cproj[,2], ca1$rproj[,2])
plot(ca1$cproj[,1], ca1$cproj[,2], xlim=xlm, ylim=ylm, xlab="CA Axis 1", ylab="CA Axis 2", type="n")
abline(h=0, lty=2); abline(v=0, lty=2)
text(ca1$rproj[,1], ca1$rproj[,2], labels=rlab, cex=.75)
text(ca1$cproj[,1], ca1$cproj[,2], labels=clab, cex=.75, col=2)

#plot col and row projections, CAA1&3
xlm <- range(ca1$cproj[,1], ca1$rproj[,1])
ylm <- range(ca1$cproj[,3], ca1$rproj[,3])
plot(ca1$cproj[,1], ca1$cproj[,3], xlim=xlm, ylim=ylm, xlab="CA Axis 1", ylab="CA Axis 3", type="n")
text(ca1$rproj[,1], ca1$rproj[,3], labels=rlab, cex=.75)
text(ca1$cproj[,1], ca1$cproj[,3], labels=clab, cex=.75, col=2)
abline(h=0, lty=2); abline(v=0, lty=2)
#*****************************************************

#self-perception of health status
hlth1 <- read.table("E:\\Robertson\\courses\\SU Data Analysis 2008\\lectures\\health percep.txt",header=T, sep="\t")
clab <- colnames(hlth1[,2:6])
rlab <- hlth1[,1]
clab; rlab

hlth1 <- hlth1[,2:6]
caHp1 <- ca.igr(hlth1)

xlm <- range(caHp1$cproj[,1], caHp1$rproj[,1])
ylm <- range(caHp1$cproj[,2], caHp1$rproj[,2])

#univariate plot
plot(caHp1$cproj[,1], rep(0,5), xlim=xlm, ylim=ylm, xlab="CA Axis 1", ylab="", type="n", yaxt="n")
text(caHp1$cproj[,1], rep(0,5), labels=clab, cex=.5)
text(caHp1$rproj[,1], rep(.01,7), labels=rlab, cex=.5)

#bivariate plot
plot(caHp1$cproj[,1], caHp1$cproj[,2], xlim=xlm, ylim=ylm, xlab="CA Axis 1", ylab="CA Axis 2", type="n")
abline(h=0, lty=2); abline(v=0, lty=2)
text(caHp1$rproj[,1], caHp1$rproj[,2], labels=rlab, cex=.75)
text(caHp1$cproj[,1], caHp1$cproj[,2], labels=clab, cex=.75, col=2)

#consider the effects of gender
hlth2 <- read.table("E:/teaching spr08/SU Data Analysis 2008/lectures/health percep by sex.txt",header=T, sep="\t")

clab <- colnames(hlth2[,2:6])
rlab <- hlth2[,1]

hlth2 <- hlth2[,2:6]
caHp2 <- ca.igr(hlth2)

#bivariate plot
xlm <- range(caHp2$cproj[,1], caHp2$rproj[,1])
ylm <- range(caHp2$cproj[,2], caHp2$rproj[,2])

plot(caHp2$cproj[,1], caHp2$cproj[,2], xlim=xlm, ylim=ylm, xlab="CA Axis 1", ylab="CA Axis 2", type="n")
abline(h=0, lty=2); abline(v=0, lty=2)
text(caHp2$rproj[,1], caHp2$rproj[,2], labels=rlab, cex=.75)
text(caHp2$cproj[,1], caHp2$cproj[,2], labels=clab, cex=.75, col=2)

for (i in 1:7) {
  arrows(caHp2$rproj[i,1], caHp2$rproj[i,2], caHp2$rproj[i+7,1], caHp2$rproj[i+7,2], length=.1, angle=10)
}
#*****************************************************
source("E:/Robertson/courses/SU Data Analysis 2008/lectures/CA analysis.IGR.R")
sjm <- read.table("E:/Robertson/courses/SU Data Analysis 2008/lectures/reynolds sj mogote.dat", header=T, sep=",")
colnames(sjm) <- c("Prov", "CORES", "PERFGR", "BIFPRE", "OBSID", 
  "MANOS", "CELTS", "BONEAWL", "SEWNEED", "BASKNEED", "MUSSEL", 
  "PEARLOY", "SPONDYL", "HORNSHL", "MICA", "IRONMIR", "IRONLUM", 
  "CUTBORD", "CANICA", "JADEORN", "FISH")

sjm1 <- sjm[, 2:ncol(sjm)]

#do the CA
ca1 <- ca.igr(sjm1)

#plot the inertia data
plot(ca1$pInertia, axes=F, type="b", ylim=c(0, 40), ylab="percent of inertia", xlab="component")
axis(1, at=c(1:length(ca1$pInertia)))
axis(2, at=seq(0,100,10))
box()

rlab <- rownames(sjm1)
clab <- colnames(sjm1)

xrange <- range(c(ca1$rproj[,1], ca1$cproj[,1]))
yrange <- range(c(ca1$rproj[,2], ca1$cproj[,2]))

plot(ca1$rproj[,1], ca1$rproj[,2], type="n", xlim=xrange, ylim=yrange, xlab="CA Axis 1", ylab="CA Axis 2")
text(ca1$rproj[,1], ca1$rproj[,2], labels=rlab, cex=.7)
text(ca1$cproj[,1], ca1$cproj[,2], labels=clab, cex=.5, col=2)

abline(h=0, lty=2)
abline(v=0, lty=2)

#a second analysis, leaving out two cases and one variable
sjm2 <- sjm[3:18, c(2:17, 19:21)]
ca2 <- ca.igr(sjm2)

rlab <- rownames(sjm2)
clab <- colnames(sjm2)

xrange <- range(c(ca2$rproj[,1], ca2$cproj[,1]))
yrange <- range(c(ca2$rproj[,2], ca2$cproj[,2]))

plot(ca2$rproj[,1], ca2$rproj[,2], type="n", xlim=xrange, ylim=yrange, xlab="CA Axis 1", ylab="CA Axis 2")
text(ca2$rproj[,1], ca2$rproj[,2], labels=rlab, cex=.7)
text(ca2$cproj[,1], ca2$cproj[,2], labels=clab, cex=.5, col=2)

abline(h=0, lty=2)
abline(v=0, lty=2)

#END of CA work**********************

#MDS*********************************
source("E:/Robertson/courses/SU Data Analysis 2008/lectures/CA analysis.IGR.R")
sjm <- read.table("E:/Robertson/courses/SU Data Analysis 2008/lectures/reynolds sj mogote.dat", header=T, sep=",")
colnames(sjm) <- c("Prov", "CORES", "PERFGR", "BIFPRE", "OBSID",
  "MANOS", "CELTS", "BONEAWL", "SEWNEED", "BASKNEED", "MUSSEL",
  "PEARLOY", "SPONDYL", "HORNSHL", "MICA", "IRONMIR", "IRONLUM",
  "CUTBORD", "CANICA", "JADEORN", "FISH")

sjm1 <- sjm[, 2:ncol(sjm)]

sjm1

#winnow out rare types of artifacts
colSums(sjm1)
sort(colSums(sjm1), decreasing = T)
colSums(sjm1) >= 10
sjm3 <- sjm1[,colSums(sjm1) >= 10]
sjm3
colSums(sjm3)

d1 <- dist(sjm3)
mds1 <- cmdscale(d1)
plot(mds1)
plot(mds1[,1], mds1[,2], type="n")
text(mds1[,1], mds1[,2], labels=rownames(sjm1))

d2 <- dist(scale(sjm3))
mds2 <- cmdscale(d2)
plot(mds2)
plot(mds2[,1], mds2[,2], type="n")
text(mds2[,1], mds2[,2], labels=rownames(sjm1))

#check out the "stress" using a shepard diagram
library(MASS)
mds3 <- isoMDS(dist(sjm3), k=2)
plot(mds3$points[,1], mds3$points[,2], type="n")
text(mds3$points[,1], mds3$points[,2], labels=rownames(sjm1))

mds2.sh <- Shepard(dist(sjm3), mds2$points)
plot(mds2.sh, pch = 20, cex=.5, xlab="input distances", ylab="MDS distances", main="Shepard diagram")
lines(mds2.sh$x, mds2.sh$yf, type = "S", col=2)