################################
# Ch 2 Predicting Algae Blooms #
################################

install.packages('DMwR')
library(DMwR)

# 2.3 Loading the Data into R
head(algae)

algae <- read.table("/Users/damiansp/Desktop/R Files/Data Mining with R/Analysis.txt", header=F, dec='.', col.names=c('season', 'size', 'speed', 'mxPH', 'mnO2', 'Cl', 'NO3', 'NH4', 'oPO4', 'PO4', 'Chla', 'a1', 'a2', 'a3', 'a4', 'a5', 'a6', 'a7'), na.strings=c('XXXXXXX'))



# 2.4 Data Visualization and Summarization
summary(algae)
hist(algae$mxPH, prob=T)

library(car)
par(mfrow=c(1,2))
hist(algae$mxPH, prob=T, xlab="", main="Max pH", ylim=0:1)
lines(density(algae$mxPH, na.rm=T))
rug(jitter(algae$mxPH))
qq.plot(algae$mxPH, main="Normal QQ plot: Max pH")
qqPlot(algae$mxPH)
qqnorm(algae$mxPH); qqline(algae$mxPH)
par(mfrow=c(1,1))

boxplot(algae$oPO4, ylab="orthophosphate")
rug(jitter(algae$oPO4), side=2)

abline(h=mean(algae$oPO4, na.rm=T), lty=2)

plot(algae$NH4)
abline(h=mean(algae$NH4, na.rm=T), lty=1)
abline(h=mean(algae$NH4, na.rm=T) + sd(algae$NH4, na.rm=T), lty=2)
abline(h=median(algae$NH4, na.rm=T), lty=4)
identify(algae$NH4)

library(lattice)
bwplot(size~a1, data=algae, ylab="size")
bwplot(speed~a1, data=algae, ylab="size")

library(Hmisc)
bwplot(size~a1, data=algae, panel=panel.bpplot, probs=seq(0.01, 0.49, 0.01), datadensity=T)
bwplot(speed~a1, data=algae, panel=panel.bpplot, probs=seq(0.01, 0.49, 0.01), datadensity=T)
minO2 <- equal.count(na.omit(algae$mnO2), number=4, overlap= 1/5)
stripplot(season ~ a3 | minO2, data=algae[!is.na(algae$mnO2),])



# 2.5 Unknown Values
library(DMwR)
data(algae)

# 2.5.1 Removing the observations with unknown values
algae[!complete.cases(algae),]
nrow(algae[!complete.cases(algae),])
algae <- na.omit(algae)	#removes all cases with any NAs

data(algae)
algae[c(62,199),]
algae <- algae[-c(62,199),]	#removes only those cases with many NAs

data(algae)
apply(algae, 1, function(x) sum(is.na(x)))	#gives no. of NAs per row
manyNAs(algae, 0.2)	#cases with NAs in 20% or more columns
algae <- algae[-manyNAs(algae),]

# 2.5.2 Filling in the unknowns with the most frequent values
algae[48, "mxPH"] <- mean(algae$mxPH, na.rm=T)
algae[is.na(algae$Chla), "Chla"] <- median(algae$Chla, na.rm=T)

# 2.5.3 Filling in the unknown values by exploring correlations
cor(algae[,4:18], use="complete.obs")
symnum(cor(algae[,4:18], use="complete.obs"))
lm(PO4 ~ oPO4, data=algae)	# PO4 = 42.897 + 1.293(oPO4)
algae[28, "PO4"] <- 42.897 + 1.293*algae[28, "oPO4"]
#or if there were multiple cases:
fillPO4 <- function(oP) {
	if(is.na(oP))
		return(NA)
	else return(42.897 + 1.293*oP)
	}
algae[is.na(algae$PO4), "PO4"] <- sapply(algae[is.na(algae$PO4), "oPO4"], fillPO4)
histogram(~mxPH | season, data=algae)
histogram(~mxPH | size, data=algae)
histogram(~mxPH | size*speed, data=algae)
stripplot(size ~ mxPH | speed, data=algae, jitter=T)