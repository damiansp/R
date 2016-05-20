#=======================================================================#
#																		#
#	Extending the Linear Model with R:									#
#		Generalized Linear, Mixed Effects and NonParametric Regression 	#
#		Models		Faraway (2006)										#
#																		#
#=======================================================================#

#===============#
#				#
#	13. Trees	#
#				#
#===============#

rm(list = ls())
#install.packages('mda', repos = 'http://cran.us.r-project.org')
library(faraway)
library(rpart)
load('~/Desktop/R/Extending the Linear Model/ELM.RData')


# 13.1 Regression Trees
data(ozone)
summary(ozone)
pairs(ozone, pch='.', panel = panel.smooth)

(roz <- rpart(O3 ~ ., ozone))

plot(roz, margin = 0.1)
text(roz)

plot(roz, compress = T, uniform = T, branch = 0.4, margin = 0.02)
text(roz)

# Visualize affect of first two splits:
quartz()
plot(O3 ~ temp, ozone)
abline(h = mean(ozone$O3), col = 'grey')
abline(v = 67.5)
lmean <- mean(ozone$O3[ozone$temp < 67.5])
rmean <- mean(ozone$O3[ozone$temp >= 67.5])
lines(c(0, 67.5), rep(lmean, 2), col = 2)
lines(c(67.5, 100), rep(rmean, 2), col = 4)

oScaled <- ozone$O3 - min(ozone$O3)
oScaled <- oScaled / max(oScaled)

oCR <- colorRamp(c('blue', 'red'))(oScaled)
ocol <- c()
for (i in 1:nrow(oCR)) {
	ocol[i] <- rgb(oCR[i, 1] / 255, 0, oCR[i, 3] / 255, alpha = 0.8)
}

plot(temp ~ ibh, data = ozone, col = ocol, pch = 16)
abline(h = 67.5)
lines(rep(3574, 2), c(67.5, 100))

# This is still a regression model, so diagnostics as usual:
plot(jitter(predict(roz)), resid(roz), xlab = 'Fitted', ylab = 'Residuals')
# (In the case of non-constant variance, transformations of the response are 
# appropriate)
qqnorm(resid(roz)); qqline(resid(roz))

# create a new data point = median values
(xO <- apply(ozone[,-1], 2, median))
predict(roz, data.frame(t(xO)))	# predict 03 levels for new point



# 13.2 Tree Pruning
roze <- rpart(O3 ~ ., ozone, cp = 0.001)	
# cp is a smoothing parameter = ratio of lambda to RSS of root tree (i.e., no 
# branches)
printcp(roze)

# cp at 5 splits, where xerror (cross-validation error) is minimized
rozr <- prune.rpart(roze, 0.0231021)	
plot(rozr, compress = T, uniform = T, margin = 0.02, branch = 0)
text(rozr)

plotcp(roz) # 2-splits (size = 3) is optimal by this mehtod
rozr2 <- prune.rpart(roze, 0.0535415)
plot(rozr2)
text(rozr2)

post(roz, filename='')
# Measure of goodness-of-fit:
1 - sum(resid(rozr)^2) / sum((ozone$O3 - mean(ozone$O3))^2)  # 5 splits = 6 params
1 - sum(resid(rozr2)^2) / sum((ozone$O3 - mean(ozone$O3))^2) # 2 splits = 3 params



# 13.3 Classification Trees
# same principal as in regression trees, but does not use RSS as criterion, but but 
# some other measure, D[i].  Let n[i,k] and p[i,k] be the no. and proportion of 
# observations of type k in terminal node i, respectively.  Then possible D[i]'s 
# include:
#(1) Deviance:
	# D[i] = -2*sum(n[i,k]*log(p[i,k]))	#over all k
#(2) Entropy:
	# D[i] = -sum(p[i,k]*log(p[i,k]))	#over all k
#(3) Gini Index:	#(default for rpart())
	# D[i] = 1-sum(p[i,k]^2)				#over all k
data(kanga)
head(kanga)
x0 <- c(NA, 1115, NA, 748, 182, NA, NA, 178, 311, 756, 226, NA, NA, NA, 48, 1009, 
		NA, 204, 593)	#x0 is of unknow species; we attempt to classify
kanga <- kanga[, c(T, !is.na(x0))]
head(kanga)

apply(kanga, 2, function(x) sum(is.na(x)))	
# note most missing cases in palate.width and mandible.length; throw those out

round(cor(kanga[,-1], use = 'pairwise.complete.obs')[,c(3, 9)], 2)	
# b/c these variables are highly correlated w/ others, we lose little by tossing 
# them
newko <- na.omit(kanga[,-c(4, 10)])	
# remove those 2 cols, and any remaining rows with NAs
dim(newko)	#144 X 10

# My own curiosity: do a PCA plot
kpca <- princomp(newko[, -1])
par(mfrow = c(2, 2))
plot(kpca$scores[, 1] ~ kpca$scores[, 2], col = newko$species, pch = 16)
plot(kpca$scores[, 1] ~ kpca$scores[, 3], col = newko$species, pch = 16)
plot(kpca$scores[, 1] ~ kpca$scores[, 4], col = newko$species, pch = 16)
plot(kpca$scores[, 1] ~ kpca$scores[, 5], col = newko$species, pch = 16)

plot(foramina.length ~ zygomatic.width, data = newko, 
	 pch = substring(species, 1, 1), col = species)	
# classes don't separate well with just these data

kt <- rpart(species ~ ., data = newko, cp = 0.001)	
# specifying cp (< default) allows for trees with more splits
printcp(kt)
ktp <- prune(kt, cp = 0.010526)	# again, where xerror is minimized
ktp
par(mfrow = c(1, 2))
plot(ktp, compress = T, branch = 0.4, margin = 0.1, uniform = T)
text(ktp, cex = 0.5)

# Seems to me it would be better to scale data first, since they might come from a 
# range of ages
newkoScaled <- newko
for (r in 1:nrow(newkoScaled)) {
	newkoScaled[r, -1] = newkoScaled[r, -1] / sum(newkoScaled[r, -1])
}

ktScaled <- rpart(species ~ ., data = newkoScaled, cp = 0.0)
# Note relative error is much smaller (better)
plot(ktScaled, compress = T, branch = 0.4, margin = 0.1, uniform = T)
text(ktScaled, cex = 0.5)


kpcaScaled <- princomp(newkoScaled[, -1])
par(mfrow = c(2, 2))
plot(kpcaScaled$scores[, 1] ~ kpcaScaled$scores[, 2], col = newko$species, pch = 16)
plot(kpcaScaled$scores[, 1] ~ kpcaScaled$scores[, 3], col = newko$species, pch = 16)
plot(kpcaScaled$scores[, 1] ~ kpcaScaled$scores[, 4], col = newko$species, pch = 16)
plot(kpcaScaled$scores[, 1] ~ kpcaScaled$scores[, 5], col = newko$species, pch = 16)



# Compute misclassification error:
(tt <- table(actual = newko$species, predicted = predict(ktp, type = 'class')))
1 - sum(diag(tt) / sum(tt))	# error rate

# and for my scaled set
(ttS <- table(actual = newkoScaled$species, 
			  predicted = predict(ktScaled, type = 'class')))
1 - sum(diag(ttS) / sum(ttS))	# error rate is better

# error high b/c absolute size is most important attribute, and relative proportions 
# not given as much weight.  To amend:
pck <- princomp(newko[,-1])
plot(pck)
pcdf <- data.frame(species = newko$species, pck$scores)
kt <- rpart(species ~., pcdf, cp = 0.001)
printcp(kt)

# remove missing vals and unused variables from test case, then apply PCA transform:
nx0 = x0[!is.na(x0)]
nx0 = nx0[-c(3, 9)]
nx0 = (nx0 - pck$center) / pck$scale
nx0 %*% pck$loadings
ktp = prune.rpart(kt, 0.0421)	# second arg is CP where xerror is minimized
ktp
plot(ktp, compress = T, branch = 0.4, margin = 0.1)
text(ktp, cex = 0.7)

(tt <- table(newko$species, predict(ktp, type = 'class')))
1 - sum(diag(tt) / sum(tt))	# error rate (My method still better)





save.image('~/Desktop/R/Extending the Linear Model/ELM.RData')
