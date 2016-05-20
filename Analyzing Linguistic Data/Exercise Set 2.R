#Analyzing Linguistic Data Ch. 2: Graphic Data Exploration
library(languageR)

#(1)
data(warlpiri)
#contingency tables: ergative~subj, age, word order
(CMSubj <- xtabs(~CaseMarking+AnimacyOfSubject, data=warlpiri))
(CMAge <- xtabs(~CaseMarking+AgeGroup, data=warlpiri))
(CMWordOrd <- xtabs(~CaseMarking+WordOrder, data=warlpiri))

g.test(CMSubj); g.test(CMAge); g.test(CMWordOrd);

par(mfrow=c(1,3))
mosaicplot(CMSubj)
mosaicplot(CMAge)
mosaicplot(CMWordOrd)
par(mfrow=c(1,1))


#(2)
data(heid)

heid2 <- aggregate(heid$RT, list(heid$Word), mean)
colnames(heid2) <- c("Word", "MeanRT")
items <- heid[,c("Word", "BaseFrequency")]
items <- unique(items)
heid2 <- merge(heid2, items, by.x="Word", by.y="Word")
heid2$MeanRT <- exp(heid2$MeanRT)
heid2$BaseFrequency <- exp(heid2$BaseFrequency)
par(mfrow=c(1,2))
plot(heid2$MeanRT~heid2$BaseFrequency)
plot(log(heid2$MeanRT)~log(heid2$BaseFrequency))


#(3)
data(moby)

moby.table <- table(moby)
moby.table <- sort(moby.table, decreasing=T)
ranks <- 1:length(moby.table)
plot(log(moby.table)~log(ranks), xlab="log(rank)", ylab="log(frequency)")
abline(lm(log(moby.table)~log(ranks)))



#(4)
xylowess.fnc(Correct~Trial, data=lexdec)
xylowess.fnc(Correct~Trial | Class, data=lexdec)

#(5)
data(english)
attach(english)
truehist(english$RTnaming)
lines(density(english$RTnaming))
bwplot(RTnaming~Voice | AgeSubject)
detach(english)