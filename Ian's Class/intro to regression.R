stNum <- c(3, 5, 7,  4, 6, 8, 6, 8, 11, 8, 11, 13, 12); stNum
cerNum <- c(180, 260, 250, 380, 400, 370, 460, 450, 400, 580, 580, 650, 700)
plot(cerNum, gstNum)

#the regression, using "linear model"
lm1 <- lm(gstNum ~ cerNum) #"y" comes first!

lm1
summary(lm1)

plot(cerNum, gstNum)
abline(lm1)
abline(1.014, .0156, col="red")

#Tukey line
tk <- line(cerNum, gstNum)
tk
abline(tk, col="green")
abline(coef(tk), col="black")

#lowess example
#plot(cerNum, gstNum)
lines(lowess(cerNum, gstNum, f=2/3), col="blue")

#############################
#transformation example
library(foreign)
tt <- read.systat("C:\\Documents and Settings\\igr\\Desktop\\today\\transform ex data.SYD")
tt <- read.systat("F:/D830 backups Fall 2008/Robertson/courses/BYU/BYU Quant 2/work/transform ex data.SYD")
str(tt)

tt
plot(tt$DISTANCE, tt$DENSITY)
#make an lm object and look at the residuals...
lm1 <- lm(tt$DENSITY~tt$DISTANCE)
abline(lm1)
lines(lowess(tt$DISTANCE, tt$DENSITY), col="blue")


#try a log transform of y
plot(tt$DISTANCE, log(tt$DENSITY))
lm2 <- lm(log(tt$DENSITY)~tt$DISTANCE)
abline(lm2)
#plot(lm2)
summary(lm2)

#plotting the regression in original space
plot(tt$DISTANCE, tt$DENSITY)
dst1 <- seq(5, 75, .01)
denhat <- 1.697-.053*dst1
lines(dst1, exp(denhat))

#other possible transformations
plot(log(tt$DISTANCE), tt$DENSITY)
plot(log(tt$DISTANCE), log(tt$DENSITY))

old.par<-par(no.readonly = TRUE)
  plot(tt$DISTANCE, tt$DENSITY, log="y")
  plot(tt$DISTANCE, tt$DENSITY) #check effects on "par"
par(old.par)

plot(tt$DISTANCE, log(tt$DENSITY))
  reg1 <- lm(log(tt$DENSITY) ~ tt$DISTANCE)
  abline(reg1)
  plot(fitted(reg1), resid(reg1))
  qqnorm(resid(reg1))
  qqline(resid(reg1))

#****************************
#multiple regression example
frmdat <- read.table("F:/D830 backups Fall 2008/Robertson/courses/Data Analysis/Data Analysis 2009/lectures/formative site data.dat", header=T, sep=",")
#lm <- lm(frmdat$size ~ log(frmdat$agland) + log(frmdat$prod))
lm3 <- lm(frmdat$size ~ frmdat$agland + frmdat$prod)
summary(lm3)
plot(fitted(lm3), resid(lm3))
qqnorm(resid(lm3))

#****************************
#calculate betacoefficients

with(frmdat, {
  Bsize <- (size-mean(size))/sd(size)
  Bagland <- (agland-mean(agland))/sd(agland)
  Bprod <- (prod-mean(prod))/sd(prod) })

lmBeta <- lm(Bsize ~ Bagland + Bprod)
summary(lmBeta)

with(frmdat, {
  Bsize <- (size-mean(size))/sd(size)
  Bagland <- (agland-mean(agland))/sd(agland)
  Bprod <- (prod-mean(prod))/sd(prod) })

lmBeta <- lm(scale(size) ~ scale(agland) + scale(prod))
summary(lmBeta)
with(frmdat, {lmBeta <- lm(scale(size) ~ scale(agland) + scale(prod))})
summary(lmBeta)