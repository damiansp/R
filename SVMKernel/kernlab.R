# kernlab
rm(list=ls())

library(kernlab)
library(ROCR)

load('~/Desktop/R/SVMKernel/kernlab.RData')

# 1. Linear SVM
	# 1.1 Generate Toy Data
	n <- 400	# no. data pts
	p <- 2		# dim
	
	sigma <- 1	# var
	meanpos <- 0	# center of distrib of pos exs
	meanneg <- 3	# center of distrib of neg exs
	npos <- round(n/2)	# no. of pos exs
	nneg <- n - npos	# no. of neg exs
	
	# Generate the pos and neg exs
	xpos <- matrix(rnorm(npos * p, mean=meanpos, sd=sigma), npos, p)
	xneg <- matrix(rnorm(nneg * p, mean=meanneg, sd=sigma), npos, p)
	x <- rbind(xpos, xneg)
	
	# Generate labels
	y <- matrix(c(rep(1, npos), rep(-1, nneg)))
	
	# Visualize
	plot(x, col=ifelse(y > 0, 1, 2))
	legend('topleft', c('Positive', 'Negative'), col=1:2, pch=1, text.col=1:2)
	
	# Prep training and test sets
	ntrain <- round(n * 0.8)	# no. to train on
	tindex <- sample(n, ntrain)	# indices of training cases
	xtrain <- x[tindex,]
	xtest <- x[-tindex,]
	ytrain <- y[tindex]
	ytest <- y[-tindex]
	istrain <- rep(0, n)
	istrain[tindex] <- 1
	
	# Visualize
	plot(x, col=ifelse(y > 0, 1, 2), pch=ifelse(istrain == 1, 1, 2))
	legend('topleft', c('Positive Train', 'Positive Test', "Negative Train", 'Negative Test'),
		   col=c(1, 1, 2, 2), pch=c(1, 2, 1, 2), text.col = c(1, 1, 2, 2))
		   
	
	# 1.2 Train an SVM
	# train:
	svp <- ksvm(xtrain, ytrain, type='C-svc', kernel='vanilladot', C=100, scaled=c())
	svp
	
	# Accessible attributes
	attributes(svp)
	
	# For example: support vectors
	alpha(svp)
	alphaindex(svp)
	b(svp)
	
	plot(svp, data=xtrain)	# ERROR
	
	# Q1
	
	
	# 1.3 Predict with a SVM
	# Predict labels on test
	ypred <- predict(svp, xtest)
	table(ytest, ypred)
	
	# Compute accuracy
	sum(ypred == ytest) / length(ytest)
	
	# Compute prediction scores
	ypredscore <- predict(svp, xtest, type='decision')
	
	# Check that the predicted labels are the signs of the scores
	table(ypredscore > 0, ypred)
	
	pred <- prediction(ypredscore, ytest)
	
	# Plot ROC curve
	perf <- performance(pred, measure='tpr', x.measure='fpr')
	plot(perf)

	# Plot precision/recall curve
	perf <- performance(pred, measure='prec', x.measure='rec')
	plot(perf)
	
	# Plot accuracy as a function of threshold
	perf <- performance(pred, measure='acc')
	plot(perf)
	
	
	# 1.4 Cross-Validation
	# Split the data into a given number of random sets
	cv.folds <- function(n, folds=3) {
		split(sample(n), rep(1:folds, length=length(y)))
	}
	
	
	# 1.5 Effect of C
	# C is a parameter representing a trade-off between wide margins and amount separated
	


# 2 Nonlinear SVM
xpos1 <- matrix(c(rnorm(npos/2 * p, mean=0, sd=sigma), 
				  rnorm(npos/2 * p, mean=0, sd=sigma)), npos, p)
xpos2 <- matrix(c(rnorm(npos/2 * p, mean=3, sd=sigma), 
				  rnorm(npos/2 * p, mean=3, sd=sigma)), npos, p)

xneg1 <- matrix(c(rnorm(nneg/2 * p, mean=0, sd=sigma), 
				  rnorm(nneg/2 * p, mean=3, sd=sigma)), npos, p)
xneg2 <- matrix(c(rnorm(nneg/2 * p, mean=3, sd=sigma), 
				  rnorm(nneg/2 * p, mean=0, sd=sigma)), npos, p)


x <- rbind(xpos1, xpos2, xneg1, xneg2)
	
# Generate labels
y <- matrix(c(rep(1, npos * p), rep(-1, nneg * p)))

plot(x, col=ifelse(y > 0, 1, 2))
legend('topleft', c('Positive', 'Negative'), col=1:2, pch=1, text.col=1:2)

svp <- ksvm(x, y, type='C-svc', kernel='vanilladot', C=100, scaled=c())
plot(svp, data=x)

# Train a nonlinear SVM
svp <- ksvm(x, y, type='C-svc', kernel='rbf', kpar=list(sigma=1), C=1)
plot(svp, data=x)


save.image('~/Desktop/R/SVMKernel/kernlab.RData')