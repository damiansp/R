##########################################
#                                        #
#  Ch 3 Predicting Stock Market Returns  #
#                                        #
##########################################

load("Ch3PredictingStockMarketReturns.RData")
library(DMwR)
library(xts)
library(zoo)
library(TTR)
library(randomForest)
library(nnet)
library(kernlab)
library(e1071)
library(earth)

data(GSPC)

# 3.2 The Available Data
head(GSPC)

	# 3.2.1 Handling time-dependent data in R

#to create an object of the xts class:
x1 <- xts(rnorm(100), seq(as.POSIXct("2000-01-01"), len=100, by="day"))
x2 <- xts(rnorm(100), seq(as.POSIXct("2000-01-01 13:00"), len=100, by="min"))
x3 <- xts(rnorm(3), as.Date(c("2005-01-01", "2005-01-10", "2005-01-12")))

x1[as.POSIXct("2000-01-04")]
x1["2000-01-05"]
x1["2000-04"]	#returns all dates in this category
x1["2000-03-27/"]	#returns all values from this date on
x1["2000-02-26/2000-03-03"]	#returns a fixed range of dates
x1["/20000103"]	#returns all dates up to this date

#combining multiple time series
mts.vals <- matrix(round(rnorm(25), 2),5,5)
colnames(mts.vals) <- paste('ts', 1:5, sep="")
mts <- xts(mts.vals, as.POSIXct(c('2003-01-01', '2003-01-04', '2003-01-05', '2003-01-06', '2003-01-16')))
mts
mts["2003-01", c('ts2', 'ts5')]	#subset
index(mts)	#returns time stamps
coredata(mts)	#returns times series data (w/o time stamps)


	# 3.2.2 Reading the data from a .csv file
GSPC <- as.xts(read.zoo("sp500.csv", header=T)) #requires path
	
	# 3.2.3 Getting data from the web
GSPC <- as.xts(get.hist.quote("^GSPC", start="1970-01-02", end="2009-09-15", quote=c("Open", "High", "Low", "Close", "Volume", "AdjClose")))
head(GSPC); plot(GSPC)



# 3.3 Defining the Prediction Task
	# 3.3.1 What to predict?
T.ind <- function(quotes, tgt.margin=0.025, n.days=10) {	#NOTE: tgt.margin indicates a 2.5% change
	v <- apply(HLC(quotes), 1, mean)
	r <- matrix(NA, ncol=n.days, nrow=NROW(quotes))
	for(x in 1:n.days)
		r[, x] <- Next(Delt(v, k=x), x)	#Delt calculates %change over k intervals
	x <- apply(r, 1, function(x) sum(x[x > tgt.margin | x < -tgt.margin]))
	if(is.xts(quotes))
		xts(x, time(quotes))
	else
		x
	}

candleChart(last(GSPC, "3 months"), TA=NULL)
avgPrice <- function(p) apply(HLC(p), 1, mean)
addAvgPrice <- newTA(FUN = avgPrice, col="white", legend="AvgPrice")
addT.ind <- newTA(FUN = T.ind, col=2, legend="tgtRet")
addAvgPrice(on=1)	#on=1 means add to 1st plotting window
addT.ind()	#not specifying on=... creates a new  plotting window


	# 3.3.2 Which predictors?
myATR <- function(x) ATR(HLC(x))[,"atr"]	#Average True Range
mySMI <- function(x) SMI(HLC(x))[,"SMI"]	#Stochastic Momentum Index
myADX <- function(x) ADX(HLC(x))[,"ADX"]	#WW's Direction Movement Index
myAroon <- function(x) aroon(x[,c("High", "Low")])$oscillator	#Aroon indicator to identify starting trends
myBB <- function(x) BBands(HLC(x))[,"pctB"]	#Volatility agst price
myChaikinVol <- function(x) Delt(chaikinVolatility(x[,c("High", "Low")]))[,1]	#Measures rate of change
myCLV <- function(x) EMA(CLV(HLC(x)))[,1]	#CLV: Close location value (relative to high and low); EMA: exponentially-weighted moving average
myEMV <- function(x) EMV(x[,c("High","Low")], x[,"Volume"])[,2]	#Ease of movement value
myMACD <- function(x) MACD(Cl(x))[,2]	#Cl: close; MACD: Oscillator that compares slower and faster MAs
myMFI <- function(x) MFI(x[,c("High","Low","Close")], x[,"Volume"])	#Money Flow Index
mySAR <- function(x) SAR(x[,c("High","Close")])[,1]	#Parabolic Stop and Reverse
myVolat <- function(x) volatility(OHLC(x), calc="garman")[,1]	#OHLC converts ts data into an OHLC (?) series

data(GSPC)
data.model <- specifyModel(T.ind(GSPC) ~ Delt(Cl(GSPC), k=1:10) + myATR(GSPC) + mySMI(GSPC) + myADX(GSPC) + myAroon(GSPC) + myBB(GSPC) + myChaikinVol(GSPC) + myCLV(GSPC) + CMO(Cl(GSPC)) + EMA(Delt(Cl(GSPC))) + myEMV(GSPC) + myVolat(GSPC) + myMACD(GSPC) + myMFI(GSPC) + RSI(Cl(GSPC)) + mySAR(GSPC) + runMean(Cl(GSPC)) + runSD(Cl(GSPC)))	#CMO: Chande Momentum Oscillator; RSI: Relative Strength Index (ration of recent increases to decreases)
set.seed(1234)
rf <- buildModel(data.model, method='randomForest', training.per=c(start(GSPC), index(GSPC["1999-12-31"])), ntree=50, importance=T)

##EXAMPLE ONLY: If you want to specify a method not available in buildModel():
#ex.model <- specifyModel(T.ind(IBM) ~ Delt(Cl(IBM), k=1:3))
#data <- modelData(ex.model, data.window=c("2009-01-01", "2009-08-10"))
##data is a zoo object
#m <- myFavoriteModelingTool(ex.model@model.formula, as.data.frame(data))

#After obtaining the model, to check the importance of the variables:
varImpPlot(rf@fitted.model, type=1)

#(arbitrarily) set threshold value at 10%:
imp <- importance(rf@fitted.model, type=1)
rownames(imp)[which(imp > 10)]

data.model <- specifyModel(T.ind(GSPC) ~ myATR(GSPC) + myAroon(GSPC) + myEMV(GSPC) + myVolat(GSPC) + myMACD(GSPC))	#note randomness makes my model different from one given in the book

	# 3.3.3 The prediction task
Tdata.train <- as.data.frame(modelData(data.model, data.window=c('1970-01-02', '1999-12-31')))
Tdata.eval <- na.omit(as.data.frame(modelData(data.model, data.window=c('2000-01-01', '2009-09-15'))))
Tform <- as.formula('T.ind.GSPC ~ .')

	# 3.3.4 Evaluation Criteria



# 3.4 The Prediction Models
	# 3.4.2 The Modeling Tools
		# 3.4.2.1 Artificial Neural Networks
set.seed(1234)
norm.data <- scale(Tdata.train)
nn <- nnet(Tform, norm.data[1:1000,], size=10, decay=0.01, maxit=1000, linout=T, trace=F)
norm.preds <- predict(nn, norm.data[1001:2000,])
preds <- unscale(norm.preds, norm.data)

sigs.nn <- trading.signals(preds, 0.1, -0.1)
true.sigs <- trading.signals(Tdata.train[1001:2000,'T.ind.GSPC'], 0.1, -0.1)
sigs.PR(sigs.nn, true.sigs)

set.seed(1)
signals <- trading.signals(Tdata.train[,'T.ind.GSPC'], 0.1,-0.1)
norm.data <- data.frame(signals=signals, scale(Tdata.train[,-1]))
nn <- nnet(signals~., norm.data[1:1000,], size=10, decay=0.01, maxit=1000, trace=F)
preds <- predict(nn, norm.data[1001:2000,], type='class')

sigs.PR(preds, norm.data[1001:2000, 1])

		# 3.4.2.2 Support Vector Machines
sv <- svm(Tform, Tdata.train[1:1000,], gamma=0.001, cost=100)
s.preds <- predict(sv, Tdata.train[1001:2000,])
sigs.svm <- trading.signals(s.preds, 0.1, -0.1)
true.sigs <- trading.signals(Tdata.train[1001:2000,'T.ind.GSPC'], 0.1, -0.1)
sigs.PR(sigs.svm, true.sigs)

data <- cbind(signals=signals, Tdata.train[,-1])
ksv <- ksvm(signals~., data[1:1000,], C=10)
ks.preds <- predict(ksv, data[1001:2000,])
sigs.PR(ks.preds, data[1001:2000, 1])

e <- earth(Tform, Tdata.train[1:1000,])
e.preds <- predict(e, Tdata.train[1001:2000,])
sigs.e <- trading.signals(e.preds, 0.1, -0.1)
true.sigs <- trading.signals(Tdata.train[1001:2000,"T.ind.GSPC"], 0.1, -0.1)
sigs.PR(sigs.e, true.sigs)



# 3.5 From Predictions into Actions
	# 3.5.3 Putting everything together: A simulated trader
policy.1 <- function(signals, market, opened.pos, money, bet=0.2, hold.time=10, exp.prof=0.025, max.loss=0.05){
	d <- NROW(market)	#ID of today
	orders <- NULL
	nOs <- NROW(opened.pos)
	#nothing to do!
	if(!nOs && signals[d]=='h')
		return(orders)
	
	#first check if we can open new positions
	# (A) Long positions
	if(signals[d]=='b' && !nOs){
		quant <- round(bet*money/market[d, 'Close'], 0)
		if(quant > 0)
			orders <- rbind(orders, data.frame(order=c(1,-1,1), order.type=c(1,2,3), val=c(quant, market[d, 'Close']*(1+exp.prof), market[d, 'Close']*(1-max.loss)), action=c('open', 'close', 'close'), posID=c(NA, NA, NA)))
		# (B) Short Positions	
		} else if(signals[d]=='s' && !nOs) {
			#this is the nr of stocks we already need to buy b/c of currently opened positions
			need2buy <- sum(opened.pos[opened.pos[,'pos.type']==-1, 'N.stocks'])*market[d,'Close']
			quant <- round(bet*(money - need2buy)/market[d, 'Close'], 0)
			if(quant > 0)
				orders <- rbind(orders, data.frame(order=c(-1,1,1), order.type=c(1,2,3), val=c(quant, market[d, 'Close']*(1-exp.prof), market[d, 'Close']*(1+max.loss)), action=c('open', 'close', 'close'), posID=c(NA,NA,NA)))
			}
		# Now check if we need to clos positions b/c over holding time
		if(nOs)
			for(i in 1:nOs){
				if(d-opened.pos[i, 'Odate'] >= hold.time)
					orders <- rbind(orders, data.frame(order=-opened.pos[i, 'pos.type'], order.type=1, val=NA, action='close', posID=rownames(opened.pos)[i]))
				}
		orders		
	}
	
##################################
# Code for policy.2 here (p.135) #
##################################

#Train & Test Periods:
start <- 1
len.tr <- 1000
len.ts <- 500
tr <- start:(start+len.tr-1)
ts <- (start+len.tr):(start+len.tr+len.ts-1)
#getting quotes for test period
data(GSPC)
date <- rownames(Tdata.train[start+len.tr,])
market <- GSPC[paste(date, '/', sep='')][1:len.ts]
#learning the model and obtaining signal predictions
s <- svm(Tform, Tdata.train[tr,], cost=10, gamma=0.01)
p <- predict(s, Tdata.train[ts,])
sig <- trading.signals(p, 0.1, -0.1)
#now using the simulated trainer
t1 <- trading.simulator(market, sig, 'policy.1', list(exp.prof=0.05, bet=0.2, hold.time=30))
summary(t1)
tradingEvaluation(t1)
plot(t1, market, theme="black", name="S&P 500")



# 3.6 Model Evaluation and Selection
	# 3.6.1 Monte Carlo estimates


############################
#                          #
#  Apply to My Stock Data  #
#                          #
############################

#Load data and convert to xts object:
LODE <- as.xts(get.hist.quote("LODE", start="2010-07-21", quote=c("Open", "High", "Low", "Close", "Volume", "AdjClose")))
candleChart(last(LODE, "12 months"), TA=NULL)
addAvgPrice(on=1)	#on=1 means add to 1st plotting window
addT.ind()	#not specifying on=... creates a new  plotting window

#Continue from 3.3.2

save.image(file="Ch3PredictingStockMarketReturns.RData")
quit()