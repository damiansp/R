#===========================#
#							#
# Machine Learning with R	#
#	Book by	Brett Lantz		#
#							#
#===========================#

rm(list = ls())
library(arules) # Market Basket analyses--sparse matrix creation
library(C50)		# C5.0 Decision Tree Algo
library(caret)
library(class)
library(e1071)	# naiveBayes()
library(gmodels)
library(ipred)
library(irr)
library(kernlab)
library(neuralnet)
library(randomForest)
library(ROCR)
library(rpart)
library(rpart.plot)
library(RWeka)	# OneR()
library(tm)		# text mining
library(vcd)
library(wordcloud)
load('~/Desktop/R/MachineLearning/ML.RData')

#=======================================#
#										#
# 2. Managing and Understanding Data	#
#										#
#=======================================#

# Exploring the structure of the data
usedcars = read.csv('~/Desktop/R/MachineLearning/chapter 2/usedcars.csv')
str(usedcars)

conservativeColors = c('Black', 'Gray', 'Silver', 'White')
usedcars$conservative = usedcars$color %in% conservativeColors
table(usedcars$conservative)
CrossTable(usedcars$model, usedcars$conservative)
table(usedcars$model, usedcars$conservative)



#===========================================================#
#															#
# 3. Lazy Learning - Classification Using Nearest Neighbor	#
#															#
#===========================================================#

# The kNN Algortihm
# Diagnosing Breast Cancer with the kNN Algorithm
wbcd = read.csv('~/Desktop/R/MachineLearning/chapter 3/wisc_bc_data.csv')
 
# Remove 'id' column
wbcd = wbcd[-1]
round(prop.table(table(wbcd$diagnosis)), 3)

normalize = function(x) {
	return ((x - min(x)) / (max(x) - min(x)))
}

cs = dim(wbcd)[2]

wbcd.n = as.data.frame(lapply(wbcd[2:cs], normalize))
wbcd.n = cbind(wbcd[1], wbcd.n)
wbcd.s = as.data.frame(lapply(wbcd[2:cs], scale))
wbcd.s = cbind(wbcd[1], wbcd.s)

trn = sample(1:569, 469)

wbcd.n.train = wbcd.n[trn, 2:cs]
wbcd.n.test = wbcd.n[-trn, 2:cs]
wbcd.n.train.labels = wbcd.n[trn, 1]
wbcd.n.test.labels = wbcd.n[-trn, 1]

wbcd.s.train = wbcd.s[trn, 2:cs]
wbcd.s.test = wbcd.s[-trn, 2:cs]
wbcd.s.train.labels = wbcd.s[trn, 1]
wbcd.s.test.labels = wbcd.s[-trn, 1]


# build the classifier and make predictions
wbcd.n.pred = knn( train = wbcd.n.train, test = wbcd.n.test, 
				   cl = wbcd.n.train.labels, k = 21 )
wbcd.s.pred = knn( train = wbcd.s.train, test = wbcd.s.test, 
				   cl = wbcd.s.train.labels, k = 21 )


CrossTable(wbcd.n.test.labels, wbcd.n.pred, prop.chisq = F)
CrossTable(wbcd.s.test.labels, wbcd.s.pred, prop.chisq = F)

#===================================================================#
#																	#
#	4. Probabilistic Learning - Classification Using Naïve Bayes	#
#																	#
#===================================================================#

# Exploring and Prepping the Data
sms_raw = read.csv( '~/Desktop/R/MachineLearning/chapter 4/sms_spam.csv', 
					stringsAsFactors = F )
sms_raw$type = factor(sms_raw$type)
table(sms_raw$type)

# Data Preparation - Processing Text for Data Analysis
sms_corpus = Corpus(VectorSource(sms_raw$text))
# To see corpus contents:
inspect(sms_corpus[c(3, 11, 1976)])
# Convert all to lower case:
corpus_clean = tm_map(sms_corpus, tolower)
# remove numbers:
corpus_clean = tm_map(corpus_clean, removeNumbers)
stopwords()
corpus_clean = tm_map(corpus_clean, removeWords, stopwords())
corpus_clean = tm_map(corpus_clean, removePunctuation)
corpus_clean = tm_map(corpus_clean, stripWhitespace)
# Observe:
inspect(sms_corpus[c(3, 11, 1976)])
sms_dtm = DocumentTermMatrix(corpus_clean)	# Constructs a matrix of counts of 
											# each word (tokenized) in each 
											# message

# Data Preparation - Creating Training and Test Datasets
samp = sample(1:5559, 4169, F)
sms_raw_train = sms_raw[samp, ]
sms_raw_test = sms_raw[-samp, ]
sms_dtm_train = sms_dtm[samp,]
sms_dtm_test = sms_dtm[-samp,]
sms_corpus_train = corpus_clean[samp]
sms_corpus_test = corpus_clean[-samp]
prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))

# Visualizing Text Data - Word Clouds
par(mfrow = c(1, 2))
wordcloud( sms_corpus_train, min.freq = 20, random.order = T, 
		   colors = c( 'violet', 'purple', 'blue', 'darkgreen', 'green', 
		   			   'yellow', 'orange', 'red' ))
wordcloud( sms_corpus_test, min.freq = 20, random.order = T, 
		   colors = c( 'violet', 'purple', 'blue', 'darkgreen', 'green', 
		   			   'yellow', 'orange', 'red' ))
spam = subset(sms_raw, type == 'spam')
ham = subset(sms_raw, type == 'ham')
wordcloud( spam$text, min.freq = 10, random.order = T, 
		   colors = c( 'violet', 'purple', 'blue', 'darkgreen', 'green', 
		   			   'yellow', 'orange', 'red' ), 
		   scale = c(3, 0.5) )
wordcloud( ham$text, min.freq = 30, random.order = T, 
		   colors = c( 'violet', 'purple', 'blue', 'darkgreen', 'green', 
		   			   'yellow', 'orange', 'red'), 
		   scale = c(3, 0.5) )

# Data Preparation - Creating Indicator Features for Frequent Words
sms_dict = findFreqTerms(sms_dtm_train, lowfreq = 5)
sms_train = DocumentTermMatrix(sms_corpus_train, list(dictionary = sms_dict))
sms_test = DocumentTermMatrix(sms_corpus_test, list(dictionary = sms_dict))

# Convert count data to factors
convert_counts = function(x) {
	x = ifelse(x > 0, 1, 0)
	x = factor(x, levels = c(0, 1), labels = c('No', 'Yes'))
	return (x)
}

sms_train = apply(sms_train, 2, convert_counts)
sms_test = apply(sms_test, 2, convert_counts)

# Step 3 - Training a Model on the Data
sms_classifier = naiveBayes(sms_train, sms_raw_train$type)

# Step 4 - Evaluating Model Performance
sms_test_pred = predict(sms_classifier, sms_test)	# SLOW
CrossTable( sms_test_pred, sms_raw_test$type, prop.chisq = F, prop.t = F, 
		    dnn = c('predicted', 'actual') )

# Step 5 - Improving Model Performance
# Introduce Laplacian smoothing so single occurrence words (with 1 in one categoray and 0 in 
# other) don't have 100% influence
sms_classifier2 = naiveBayes(sms_train, sms_raw_train$type, laplace = 1)
sms_test_pred2 = predict(sms_classifier2, sms_test)
CrossTable( sms_test_pred2, sms_raw_test$type, prop.chisq = F, prop.t = F, 
		    dnn = c('predicted', 'actual') )



#===========================================#
#											#
#	5. Divide and Conquer - Classification	#  
#		Using Decision Trees and Rules		#
#											#
#===========================================#

# The C5.0 Decision Tree Algorithm
curve(-x * log2(x) - (1 - x) * log2(1 - x), xlab = 'x', ylab = 'Entropy')

# Exploring and Preparing the Data
credit = read.csv('~/Desktop/R/MachineLearning/chapter 5/credit.csv')
table(credit$checking_balance)
table(credit$savings_balance)
truehist(credit$months_loan_duration)
truehist(credit$amount)
lines(density(credit$amount))
credit_rand = credit[order(runif(1000)), ]
credit_train = credit_rand[1:900, ]
credit_test = credit_rand[901:1000, ]
prop.table(table(credit_train$default))

# Training a Model on the Data
?C5.0Control
credit_model = C5.0(credit_train[-17], credit_train$default)
credit_model
summary(credit_model)

# Evaluating Model Performance
credit_pred = predict(credit_model, credit_test)
CrossTable( credit_test$default, credit_pred, prop.chisq = F, prop.c = F, 
			prop.r = F, dnn = c('actual default', 'predicted default') )

# Improving Model Performance
credit_boost10 = C5.0(credit_train[-17], credit_train$default, trials = 10)
credit_boost10
summary(credit_boost10)
credit_boost_pred10 = predict(credit_boost10, credit_test)
CrossTable( credit_test$default, credit_boost_pred10, prop.chisq = F, 
			prop.c = F, prop.r = F, 
			dnn = c('actual default', 'predicted default') )

# Making Some Mistakes More Costly than Others
error_cost = matrix(c(0, 1, 4, 0), nrow = 2)	
error_cost	# i.e. pred 'n' but actually 'y', more costly
credit_cost = C5.0(credit_train[-17], credit_train$default, costs = error_cost)
credit_cost_pred = predict(credit_cost, credit_test)
CrossTable( credit_test$default, credit_cost_pred, prop.chisq = F, prop.c = F, 
			prop.r = F, dnn = c('actual default', 'predicted default') )

# Exploring and Preparing the Data
mushrooms = read.csv('~/Desktop/R/MachineLearning/chapter 5/mushrooms.csv')
mushrooms$veil_type = NULL	# All entries have same value
table(mushrooms$type)

# Training a Model on the Data
# Find the single best feature rule
mushroom_1R = OneR(type ~ ., data = mushrooms)
# Examine rule
mushroom_1R

# Evaluating Model Performance
summary(mushroom_1R)

# Improving Model Performance
mushroom_JRip = JRip(type ~ ., data = mushrooms)
mushroom_JRip



#=======================================================#
#														#
#	6. Forecasting Numeric Data - Regression Methods	#
#														#
#=======================================================#

# Classification and Regression Trees
tee = c(1, 1, 1, 2, 2, 3, 4, 5, 5, 6, 6, 7, 7, 7, 7)
# subdivide tee (A)
at1 = tee[1:9]
at2 = tee[10:15]
# subdivision (B)
bt1 = tee[1:7]
bt2 = tee[8:15]
# Amount of reduction of sd in each grouping
sdr_a = sd(tee) - (length(at1) / length(tee) * sd(at1) + 
		length(at2) / length(tee) * sd(at2))
sdr_b = sd(tee) - (length(bt1) / length(tee) * sd(bt1) + 
		length(bt2) / length(tee) * sd(bt2))
sdr_a	# 1.20
sdr_b	# 1.39

# Wine rating case study
# Exploring and prepping the data
wine = read.csv('~/Desktop/R/MachineLearning/chapter 6/whitewines.csv')
summary(wine)
truehist(wine$quality)
train = sample(1:4898, 3750)
wine_train = wine[train,]
wine_test = wine[-train,]

# Training a model on the data
?rpart.control
m.rpart = rpart(quality ~ ., data = wine_train)
m.rpart
plot(m.rpart)
rpart.plot(m.rpart, digits = 3)
rpart.plot(m.rpart, digits = 3, fallen.leaves = T, type = 4, extra = 101)

# Evaluating the model performance
p.rpart = predict(m.rpart, wine_test)
rbind(summary(p.rpart), summary(wine_test$quality)) # does a good job in middle 
													# 50%
cor(p.rpart, wine_test$quality)

# Measuring performance with mean absolute error
mae = function(actual, predicted) {
	mean(abs(actual - predicted))
}

mae(p.rpart, wine_test$quality)
hist(abs(p.rpart - wine_test$quality))

# Improving model performance
m.m5p = M5P(quality ~ ., data = wine_train)	# M5P in RWeka library
m.m5p
summary(m.m5p)

p.m5p = predict(m.m5p, wine_test)
rbind(summary(p.m5p), summary(wine_test$quality))
cor(p.m5p, wine_test$quality)
mae(wine_test$quality, p.m5p)



#===========================================#
#											#
# 7. Black Box Methods--Neural Networks and	#
#	 Support Vector Machines				#
#								  			#
#===========================================#
concrete = read.csv('~/Desktop/R/MachineLearning/chapter 7/concrete.csv')
head(concrete)

normalize = function(x) {
	return ((x - min(x)) / (max(x) - min(x)))
}

concreteNorm = as.data.frame(lapply(concrete, normalize))
concreteScaled = as.data.frame(lapply(concrete, scale))

sampRows = sample(1:1030, 773)
concreteTrain = concreteNorm[sampRows, ]
concreteTest = concreteNorm[-sampRows, ]

concreteTrain2 = concreteScaled[sampRows, ]
concreteTest2 = concreteScaled[-sampRows, ]

# Training the model
concreteModel = neuralnet( strength ~ cement + slag + ash + water + 	
						   superplastic + coarseagg + fineagg + age, 
						   data = concreteTrain )
concreteModel2 = neuralnet( strength ~ cement + slag + ash + water + 	
							superplastic + coarseagg + fineagg + age, 
							data = concreteTrain2 )
plot(concreteModel)
plot(concreteModel2)

# Evaluating model performance
modelResults = compute(concreteModel, concreteTest[1:8])
predictedStrength = modelResults$net.result
plot(concreteTest$strength ~ predictedStrength)
cor(concreteTest$strength, predictedStrength)	# 0.8101 ~ 0.8378

modelResults2 = compute(concreteModel2, concreteTest2[1:8])
predictedStrength2 = modelResults2$net.result
plot(concreteTest2$strength ~ predictedStrength2)
cor(concreteTest2$strength, predictedStrength2)	# 0.8107 ~ 0.8385
												# NOTE: Scaling does 
												# better than normalizing 
												# here

# Improving model performance
concreteModel2 = neuralnet( strength ~ cement + slag + ash + water + 
							superplastic + coarseagg + fineagg + age, 
							data = concreteTrain2, hidden = 5 )
plot(concreteModel2)
modelResults2 = compute(concreteModel2, concreteTest2[1:8])
predictedStrength2 = modelResults2$net.result
plot(concreteTest2$strength ~ predictedStrength2)
cor(concreteTest2$strength, predictedStrength2)	# 0.9396 ~ 0.9406

# SVMs

# Exploring and Prepping the Data
letters = read.csv('~/Desktop/R/MachineLearning/chapter 7/letterdata.csv')
head(letters)
# pairs(letters[, c(2:8)], pch = 16, col = rgb(0,0,0, 0.01))
nTrain = 16000
trainRows = sample(1:20000, nTrain)
lettersTrain = letters[trainRows, ]
lettersTest = letters[-trainRows, ]

# Training a Model on the Data
# Packages with svm algos:
#	e1071	see: http://www.csie.ntu.edu.tw/~cjlin/libsvm/
#	klaR	see: http://svmlight.joachims.org
#	kernlab (with package: caret --automates training/evaluating)
#			see: http://www.jstatsoft.org/v11/i09
?ksvm

letterClassifier = ksvm( letter ~ ., data = lettersTrain, 
						 kernel = 'vanilladot' )
letterClassifier

# Evaluating the Model Performance
letterPredictions = predict(letterClassifier, lettersTest)
head(letterPredictions)
table(letterPredictions, lettersTest$letter)
agreement = letterPredictions == lettersTest$letter
table(agreement)
prop.table(table(agreement))		# 86.3% accuracy


# Improving Model Performance
letterClassifierRBF = ksvm(letter ~ ., data = lettersTrain, kernel = 'rbfdot')
letterPredictionsRBF = predict(letterClassifierRBF, lettersTest)
agreementRBF = letterPredictionsRBF == lettersTest$letter
table(agreementRBF)
prop.table(table(agreementRBF))	# 94.1% accuracy



#===============================================#
#											 	#
# 8. Finding Patterns - Market Basket Analysis	#
#	 Using Association Rules				 	#
#								  			 	#
#===============================================#

# Identifying grocery items frequently purchased together w association rules
# Collecting data
groceries = read.csv( '~/Desktop/R/MachineLearning/chapter 8/groceries.csv', 
					  header = F )
head(groceries)
dim(groceries)
rm(groceries)

# Exploring and prepping the data
# Data prep: creating a sparse matrix for transaction data
# Overwrite 'groceries' as a sparse matrix
groceries = read.transactions( 
				'~/Desktop/R/MachineLearning/chapter 8/groceries.csv', 
				sep = ',' 
			)
summary(groceries)
inspect(groceries[1:5])
itemFrequency(groceries[, 1:5])

# Visualizing item support - item frequency plots
itemFrequencyPlot(groceries, support = 0.08)
itemFrequencyPlot(groceries, topN = 20)

# Visualize the sparse matrix
image(groceries[1:50])

# Training the model on the data
apriori(groceries)	# default params no good...
grocery.rules = apriori( groceries, 
						 parameter = list( support = 0.006, 
										   confidence = 0.25, 
										   minlen = 2 ))

# Evaluating the model performance
summary(grocery.rules)
arules::inspect(grocery.rules[1:10])
liftOrder = rev(order(grocery.rules@quality$lift))
arules::inspect(grocery.rules[liftOrder][1:10])

# Improving model performance
# Turns out there's an easier way to do what I coded above, namely:
arules::inspect(sort(grocery.rules, by = 'lift')[1:10])

berry.rules = subset(grocery.rules, items %in% 'berries')
arules::inspect(berry.rules)
fruit.rules = subset(grocery.rules, items %pin% 'fruit') # pin: partial match
arules::inspect(sort(fruit.rules, by = 'lift')[1:10])
dairy.rules = subset( grocery.rules, 
					  items %ain% c('yogurt', 'whipped/sour cream') )
					  # ain: match all
arules::inspect(sort(dairy.rules, by = 'lift'))

# Write rules to .csv
write( grocery.rules, '~/Desktop/R/MachineLearning/groceryRules.csv', sep = ',',
	   quote = T, row.names = F )

# Or convert to data.frame
grocery.rules.df = as(grocery.rules, 'data.frame')
head(grocery.rules.df)
plot(lift ~ confidence, data = grocery.rules.df)



#===============================#
#							 	#
# 9. Finding Groups of Data - 	#
#	 Clustering with k-Means	#
#				  			 	#
#===============================#

# Exploring and Prepping the Data
teens = read.csv('~/Desktop/R/MachineLearning/chapter 9/snsdata.csv')
head(teens)
table(teens$gender, useNA = 'ifany')

summary(teens$age)
# Clean: convert non-teen ages to NAs
teens$age = ifelse(teens$age > = 13 & teens$age < 20, teens$age, NA)

# Data Preparation - Dummy Coding Missing Values
teens$female = ifelse(teens$gender == 'F' & !is.na(teens$gender), 1, 0)
teens$no_gender = ifelse(is.na(teens$gender), 1, 0)

# Data Preparation - Imputing Missing Values
# Estimate age from mean age per graduation year
mean(teens$age, na.rm = T)
aggregate(data = teens, age ~ gradyear, mean, na.rm = T)	# same as:
tapply(teens$age, teens$gradyear, FUN = mean, na.rm = T)

ave.age = ave(teens$age, teens$gradyear, FUN = function(x) { 
	mean(x, na.rm = T) 
})

teens$age = ifelse(is.na(teens$age), ave.age, teens$age)
summary(teens$age)


# Training the Model on the Data
interests = teens[5:40]
interests.z = as.data.frame(lapply(interests, scale))
teen.clusters = kmeans(interests.z, 5)


# Evaluating Model Performance
teen.clusters$size
round(prop.table(teen.clusters$size), 3)
teen.clusters$centers


# Improving Model Performance
teens$cluster = teen.clusters$cluster
head(teens)

aggregate(data = teens, age ~ cluster, mean) # same as:
tapply(teens$age, teens$cluster, FUN = mean, na.rm = T)
aggregate(data = teens, female ~ cluster, mean)
aggregate(data = teens, friends ~ cluster, mean)



#===================================#
#							 		#
#  10. Evaluating Model Performance	#
#				  			 		#
#===================================#

# Working with Classification Prediction Data in R
sms.results = read.csv( 
	'~/Desktop/R/MachineLearning/chapter 10/sms_results.csv' 
)

head(sms.results)
# show just some results where predictions were wrong
head(subset(sms.results, actual_type ! = predict_type))

# A closer look at confusion matrices
table(sms.results$actual_type, sms.results$predict_type)
CrossTable(sms.results$actual_type, sms.results$predict_type)

# Beyond Accuracy - Other Measures of Performance
confusionMatrix( sms.results$predict_type, sms.results$actual_type, 
				 positive = 'spam' )

# Calculting the Kappa statistic
# Sum proportions of the main diagonal (accuracy: TP + TN)
pr.a = 0.865 + 0.111		# 0.976
# Calculate expected values for these cells as the product of their marginals
pr.e = 0.868 * 0.886 + 0.132 * 0.114		# 0.784

# there is a built in kappa function, so +x:
kappax = function(pr.a, pr.e) {
	return ((pr.a - pr.e) / (1 - pr.e))
}

kappax(pr.a, pr.e)

# library(vcd)
Kappa(table(sms.results$actual_type, sms.results$predict_type))


# Sensitivity and Specificity
# Sensitivity: TP / (TP + FN) 	("True positive rate")
# Specificity: TN / (TN + FP)	("True negative rate")
sens = 154 / (154 + 29)	# True Postive / all actual positives	0.842
# i.e., of all spam, 84% were id'ed as such
spec = 1202 / (1202 + 5) # True Negs / all actual negatives	0.996
# i.e., of all legit emails, 99.6% were id'ed as such
# with library(caret)
sensitivity( sms.results$predict_type, sms.results$actual_type, 
			 positive = 'spam' )
specificity( sms.results$predict_type, sms.results$actual_type, 
			 negative = 'ham' )


# Precision and Recall
# Precision = TP / (TP + FP)
# Recall    = TP / (TP + FN)
prec = 154 / (154 + 5)	# True pos / all predicted as pos	0.969
# i.e., of all things predicted as spam, 96.9% actually were
rec =  154 / (154 + 29) # ERROR: This definition is the same as given for 
						# sensitivity above; one is wrong

# The F-measure
F1 = (2 * prec * rec) / (prec + rec)				 

# library(ROCR)
pred = prediction( predictions = sms.results$prob_spam, 
				   labels = sms.results$actual_type )

# ROC Curves
perf = performance(pred, measure = 'tpr', x.measure = 'fpr')
plot(perf, main = 'ROC Curve for SMS Spam Filter', col = 2)
abline(0, 1, col = 'grey')
abline(h = c(0, 1), col = 'darkgrey')
abline(v = c(0, 1), col = 'darkgrey')
perf.auc = performance(pred, measure = 'auc')
str(perf.auc)
unlist(perf.auc@y.values)	# 0.983

# The Holdout Method
credit = read.csv('~/Desktop/R/MachineLearning/chapter 10/credit.csv')
random.ids = order(runif(1000))
credit.train = credit[random.ids[1:500], ]
credit.validate = credit[random.ids[501:750], ]
credit.test = credit[random.ids[751:1000], ]

in.train = createDataPartition(credit$default, p = 0.75, list = F)
credit.train = credit[in.train, ]
credit.test = credit[-in.train, ]

# Cross-Validation
folds = createFolds(credit$default, k = 10)
str(folds)

credit01.train = credit[folds$Fold01, ]
credit02.test = credit[-folds$Fold01, ] # too tedious to do each this way...

cv.results = lapply(folds, function(x) {
	credit.train = credit[x, ]
	credit.test = credit[-x, ]
	credit.model = C5.0(default ~ ., data = credit.train)
	credit.pred = predict(credit.model, credit.test)
	credit.actual = credit.test$default
	kap = kappa2(data.frame(credit.actual, credit.pred))$value
	return(kap)
})

str(cv.results)
mean(unlist(cv.results))	# 0.182 (poor)

# Creating a simple tuned model
m = train(default ~ ., data = credit, method = 'C5.0')
p = predict(m, credit)
table(p, credit$default)
cbind(head(predict(m, credit, type='prob')), head(credit$default))
				 
# Customizing the tuning process
ctrl = trainControl(method = 'cv', number = 10, selectionFunction = 'oneSE')
grid = expand.grid( .model = 'tree', 
					.trials = c(1, 5, 10, 15, 20, 25, 30, 35),
					.winnow = F )
m = train( default ~ ., data = credit, method = 'C5.0', metric = 'Kappa',
		   trControl = ctrl, tuneGrid = grid)
		   
# Ensemble Methods
# Bagging
# library(ipred)
mybag = bagging(default ~., data = credit, nbagg = 25)
credit.pred = predict(mybag, credit)
table(credit.pred, credit$default)

# library(caret)
ctrl = trainControl(method = 'cv', number = 10)
train(default ~., data = credit, method = 'treebag', trControl = ctrl)

# Make a bagged SVM
str(svmBag)
svmBag$fit

# Investigate caret docs for examples with naive bayes models (nbBag), decision trees (ctreeBag), and neural networks (nnetBag)
bagctrl = bagControl( fit = svmBag$fit, predict = svmBag$pred, 
					  aggregate = svmBag$aggregate )
svmbag = train(	default ~., data = credit, 'bag', trControl = ctrl,	
				bagControl = bagctrl )
svmbag
svmbag.pred = predict(svmbag, credit)
table(svmbag.pred, credit$default)

# Training random forests
# library(randomForest)
rf = randomForest(default ~ ., data = credit)
rf

# Evaluating random forest performance
ctrl = trainControl(method = 'repeatedcv', number = 10, repeats = 10)
grid.rf = expand.grid(.mtry = c(2, 4, 8, 16))

m.rf = train( default ~., data = credit, method = 'rf', metric = 'Kappa', 
			  trControl = ctrl, tuneGrid = grid.rf )

grid.c50 = expand.grid( .model = 'tree', .trials = c(10, 20, 30, 40),
						.winnow = F )				
m.c50 = train( default ~., data = credit, method = 'C5.0', metric = 'Kappa',
			   trControl = ctrl, tuneGrid = grid.c50)				
m.rf
m.c50



save.image('~/Desktop/R/MachineLearning/ML.RData')