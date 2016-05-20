#=======================================================================#
#																		#
#	Extending the Linear Model with R:									#
#		Generalized Linear, Mixed Effects and NonParametric Regression 	#
#		Models		Faraway (2006)										#
#																		#
#=======================================================================#

#=========================#
#                         #
#  Ch 14 Neural Networks  #
#                         #
#=========================#

rm(list = ls())

#install.packages('mda', repos = 'http://cran.us.r-project.org')
library(nnet)
library(faraway)
load('~/Desktop/R/Extending the Linear Model/ELM.RData')

data(ozone)

# 14.3 Neural Network Application
nnmdl <- nnet(O3 ~ temp + ibh + ibt, ozone, size = 2, linout = T)
# final value (21115) is the RSS; size = number of hidden layers; linout = T: for 
# linear output--default is logistic
# Fit is poor; easier to initialize weights in a useful way if data are scaled:

sx <- scale(ozone)
bestrss <- 10000
for(i in 1:100) {
  nnmdl <- nnet(O3 ~ temp + ibh + ibt, data = sx, size = 2, linout = T, trace = F)
  # trace optimization
  cat(nnmdl$value, '\n')
  if(nnmdl$value < bestrss) {
    bestnn <- nnmdl
    bestrss <- nnmdl$value
  }
}

# Examine estimated weights
summary(bestnn)
# notation: i2->h1: link from input 2 to first hidden neuron; b is bias o is output


# Calculating r^2 for the fit:
1 - bestrss/sum((sx[, 1] - mean(sx[, 1]))^2)	#0.721

# observe effects of each predictor by varying while others held constant:
# First unscale:
ozmeans <- attributes(sx)$"scaled:center"
ozscales <- attributes(sx)$"scaled:scale"
# expand.grid() creates a data frame fr all combos of factors
xx <- expand.grid(temp = seq(-3, 3, 0.1), ibh = 0, ibt = 0)	
plot(xx$temp * ozscales['temp'] + ozmeans['temp'], 
	 predict(bestnn, new = xx) * ozscales['O3'] + ozmeans['O3'], 
	 xlab = 'Temp', ylab = 'O3', type = 'l')

xx <- expand.grid(temp = 0, ibh = seq(-3, 3, 0.1), ibt = 0)
plot(xx$ibh * ozscales['ibh'] + ozmeans['ibh'], 
	 predict(bestnn, new = xx) * ozscales['O3'] + ozmeans['O3'], xlab = 'IBH', 
	 ylab = 'O3', type = 'l')

xx <- expand.grid(temp = 0, ibh = 0, ibt = seq(-3, 3, 0.1))
plot(xx$ibt * ozscales['ibt'] + ozmeans['ibt'], 
	 predict(bestnn, new = xx) * ozscales['O3'] + ozmeans['O3'], 
	 xlab = 'IBT', ylab = 'O3', type = 'l')

# To smooth the fit ("weight decay")-- minimize E + lambda*sum(w^2); 
# Try with lambda = 0.001 (decay parameter)
bestrss <- 10000
for(i in 1:100) {
  nnmdl <- nnet(O3 ~ temp + ibh + ibt, sx, size = 2, linout = T, decay = 0.001, 
  				trace = F)
  cat(nnmdl$value, '\n')
  if(nnmdl$value < bestrss) {
    bestnn <- nnmdl
    bestrss <- nnmdl$value
  }
}

xx <- expand.grid(temp = seq(-3, 3, 0.1), ibh = 0, ibt = 0)	
plot(xx$temp * ozscales['temp'] + ozmeans['temp'], 
	 predict(bestnn, new = xx) * ozscales['O3'] + ozmeans['O3'], 
	 xlab = 'Temp', ylab = 'O3', type = 'l')

xx <- expand.grid(temp = 0, ibh = seq(-3, 3, 0.1), ibt = 0)
plot(xx$ibh * ozscales['ibh'] + ozmeans['ibh'], 	
	 predict(bestnn, new = xx) * ozscales['O3'] + ozmeans['O3'], 
	 xlab = 'IBH', ylab = 'O3', type = 'l')

xx <- expand.grid(temp = 0, ibh = 0, ibt = seq(-3, 3, 0.1))
plot(xx$ibt * ozscales['ibt'] + ozmeans['ibt'], 	
	 predict(bestnn, new = xx) * ozscales['O3'] + ozmeans['O3'], 
	 xlab = 'IBT', ylab = 'O3', type = 'l')
# Note that interactions terms are necessarily "built-in" to the NN

# expand to full model:
bestrss <- 10000
for(i in 1:100) {
  nnmdl <- nnet(O3 ~ ., sx, size = 4, linout = T, trace = F)
  cat(nnmdl$value, '\n')
  if(nnmdl$value < bestrss) {
    bestnn <- nnmdl
    bestrss <- nnmdl$value
  }
}

# r^2:
1 - bestnn$value / sum((sx[,1] - mean(sx[,1]))^2)	# 0.845



save.image('~/Desktop/R/Extending the Linear Model/ELM.RData')
