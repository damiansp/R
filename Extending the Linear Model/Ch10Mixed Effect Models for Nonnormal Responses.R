#####################################################
# Ch 10 Mixed Effect Models for Nonnormal Responses #
#####################################################

# 10.1 Generalized Linear Mixed Models
data(ctsib); head(ctsib)
ctsib$stable <- ifelse(ctsib$CTSIB==1, 1, 0)
summary(ctsib)
gf <- glm(stable ~ Sex + Age + Height + Weight + Surface + Vision, binomial, data=ctsib)	#ignore subject data
summary(gf); plot(gf)
#but there could be a signif subj effect:
gfs <- glm(stable ~ Sex + Age + Height + Weight + Surface + Vision + factor(Subject), binomial, data=ctsib)
anova(gf, gfs, test="Chi")	#strong evidence for subject effect

library(brlr)	#no longer available
library(MASS)

gg <- glmmPQL(stable ~ Sex + Age + Height + Weight + Surface + Vision, random=~1 | Subject, family=binomial, data=ctsib)	#generalized linear mixed model with penalized quasi-likelihood
summary(gg)
