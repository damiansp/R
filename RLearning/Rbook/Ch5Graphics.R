#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
setwd('~/Learning/R/RLearning/Rbook')
#library(effects)
#library(lattice)
library(MASS)
library(mgcv)


# 4 Drawing Mathematical Functions
curve(x^3 - 3*x, -2, 2)

xv <- 0:100
yA <- 482 * xv * exp(-0.045*xv)
yB <- 518 * xv * exp(-0.055*xv)

plot(c(xv, xv), c(yA, yB), type='n')
lines(xv, yA, col=2)
lines(xv, yB, col=4)



jaws <- read.table('data/jaws.txt', header=T)
head(jaws)
par(mfrow=c(2,2))
plot(jaws$age, jaws$bone, main='lowess')
lines(lowess(jaws$age, jaws$bone), col=4)

plot(jaws$age, jaws$bone, main='loess')
model <- loess(bone~age, data=jaws)
xv <- 0:50
yv <- predict(model, data.frame(age=xv))
lines(xv, yv, col=4)

plot(jaws$age, jaws$bone, main='gam')
model <- gam(bone ~ s(age), data=jaws)
yv <- predict(model, list(age=xv))
lines(xv, yv, col=4)

plot(jaws$age, jaws$bone, main='polynomial (lm)')
model <- lm(bone ~ age + I(age^2) + I(age^3), data=jaws)
yv <- predict(model, list(age=xv))
lines(xv, yv, col=4)


# Joining the dots
smooth <- read.table('data/smoothing.txt', header=T)
head(smooth)
sequence <- order(smooth$x)
par(mfrow=c(1,1))
plot(smooth$x, smooth$y)
lines(smooth$x[sequence], smooth$y[sequence], col=4)
lines(smooth$x, smooth$y, col=2)


# Plotting with a categorical explanatory variable
weather <- read.table('data/SilwoodWeather.txt', header=T)
head(weather)
weather$month <- factor(weather$month)
plot(weather$month, weather$upper)

trial <- read.table('data/compexpt.txt', header=T)
head(trial)
plot(trial$clipping, trial$biomass, xlab="treatment", ylab="yield")
par(mfrow=c(1, 2))
boxplot(trial$biomass ~ trial$clipping)
boxplot(trial$biomass ~ trial$clipping, notch=T)

means <- tapply(trial$biomass, trial$clipping, mean)
par(mfrow=c(1, 1))
barplot(means, xlab="treatment", ylab="yield")

data <- read.table('data/box.txt', header=T)
head(data)
plot(data$response ~ factor(data$fact))
index <- order(tapply(data$response, data$fact, mean))
ordered <- factor(rep(index, rep(20, 8)))
boxplot(data$response ~ ordered, 
        notch=T, 
        names=as.character(index), 
        xlab="ranked treatments", 
        ylab="response")
model <- aov(data$response ~ factor(data$fact))
summary(model)
TukeyHSD(model)
plot(TukeyHSD(model))

# Plots for single samples
# Histograms
hist(rpois(1000, 3.7))
hist(rpois(1000, 3.7), breaks=seq(-0.5, 11.5, 1))
y <- rnbinom(158, mu=1.5, size=1)
bks <- -0.5:(max(y) + 0.5)
hist(y, bks, ylim=c(0,70))
mean(y)	#1.5949
var(y)	#4.1151
mean(y)^2 / (var(y) - mean(y))	#1.0094 (size)
xs <- 0:13
ys <- dnbinom(xs, size=1.0094, mu=1.5949)
lines(xs, ys*158)

head(faithful)
# rule of thumb for bandwidth
(bw <- (max(faithful$eruptions) - min(faithful$eruptions))
        / (2 * (1 + log(length(faithful$eruptions), base=2))))	
par(mfrow=c(1, 2))
hist(faithful$eruptions, 15, probability=T)
lines(density(faithful$eruptions, width=bw), col=2)
lines(density(faithful$eruptions, width=0.6), col=4)
rug(faithful$eruptions)
truehist(faithful$eruptions, 15)
lines(density(faithful$eruptions))
rug(faithful$eruptions)

#Time series plots
data(UKLungDeaths)
par(mfrow=c(1, 1))
ts.plot(ldeaths, mdeaths, fdeaths, xlab="year", ylab='deaths', col=c(1:3))
data(sunspots)
plot(sunspots)
class(sunspots)
time(sunspots)

#The stripchart function
data(OrchardSprays)
with(OrchardSprays, 
     stripchart(decrease ~ treatment, ylab='decrease', vertical=T, log='y'))

# Plots with multiple variables
# The pairs function
ozone <- read.table("data/ozone.data.txt", header=T)
names(ozone)
pairs(ozone, panel=panel.smooth)

#The coplot function
coplot(ozone ~ wind | temp, panel=panel.smooth, data=ozone)

#Interaction plots
yields <- read.table("data/splityield.txt", header=T)
head(yields)
interaction.plot(yields$fertilizer, yields$irrigation, yields$yield)

# Bubble plots
bubble <- function(x, y, r, bubble.size, x.scale, y.scale) {
  theta <- seq(0, 2*pi, pi / 200)
  yv <- r*sin(theta)*bubble.size*y.scale
  xv <- r*cos(theta)*bubble.size*x.scale
  lines(x + xv, y + yv)
}

bubble.plot <- function(xv, yv, rv, bs=0.1, ...) {
  r <- rv / max(rv)
  y.scale <- max(yv) - min(yv)
  x.scale <- max(xv) - min(xv)
  plot(xv, yv, type='n', ...)
  for (i in 1:length(xv)) bubble(xv[i], yv[i], r[i], bs, x.scale, y.scale)
}

x <- runif(30)
y <- runif(30)
r <- rpois(30, 5)
bubble.plot(x, y, r)

# Sunflower plot
x <- rpois(100, 2)
y <- rpois(100, 3)
sunflowerplot(x, y)