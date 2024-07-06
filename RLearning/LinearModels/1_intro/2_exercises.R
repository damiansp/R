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


# 2
data(uswages)
head(uswages)
summary(uswages)
pairs(uswages)
hist(uswages$wage)  # extreme skew
hist(uswages$educ)  # moderate skew
hist(uswages$exper) # slight skew; neg values
table(uswages$race) # imbalance
table(uswages$smsa) # ""
table(uswages$ne)
table(uswages$mw)
table(uswages$so)
table(uswages$we)
table(uswages$pt)   # ""


# 3
data(prostate)
head(prostate)
summary(prostate)
pairs(prostate)
hist(prostate$lcavol)   # normalish
hist(prostate$lweight)  # one maybe outlier
hist(prostate$age)      # normalish (slight left skew)
hist(prostate$lbph)     # bimodal w/ huge spike at/near min
table(prostate$svi)     # 76:21
hist(prostate$lcp)      # almost uniform except spike at/near min
hist(prostate$gleason)
hist(prostate$pgg45)    # extreme skew
hist(prostate$lpsa)     # normalish