library(faraway)


# 1
data(teengamb)
head(teengamb)
summary(teengamb)
pairs(teengamb)
table(teengamb$sex)
hist(teengamb$status)
hist(teengamb$income) # skew
hist(teengamb$verbal) 
hist(teengamb$gamble) # extreme skew