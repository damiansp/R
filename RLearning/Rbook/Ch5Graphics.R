#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
setwd('~/Learning/R/RLearning/Rbook')
#library(effects)
#library(lattice)
#library(MASS)
#library(mgcv)


# 4 Drawing Mathematical Functions
curve(x^3 - 3*x, -2, 2)

xv <- 0:100
yA <- 482 * xv * exp(-0.045*xv)
yB <- 518 * xv * exp(-0.055*xv)

plot(c(xv, xv), c(yA, yB), type='n')
lines(xv, yA, col=2)
lines(xv, yB, col=4)



# from p. 151

data <- read.table("/Users/damiansp/Desktop/R Files/Rbook/jaws.txt", header=T)
attach(data)
par(mfrow=c(2,2))
plot(age, bone)
text(45, 20, "lowess", pos=2)
lines(lowess(age, bone))

plot(age, bone)
text(45,20, "loess", pos=2)
model <- loess(bone~age)
xv <- 0:50
yv <- predict(model, data.frame(age=xv))
lines(xv, yv)

plot(age, bone)
text(45,20,"gam", pos=2)
model <- gam(bone~s(age))
yv <- predict(model, list(age=xv))
lines(xv, yv)

plot(age, bone)
text(45,20, "polynomial", pos=2)
model <- lm(bone~age+I(age^2)+I(age^3))
yv <- predict(model, list(age=xv))
lines(xv, yv)


# Joining the dots
smooth <- read.table("/Users/damiansp/Desktop/R Files/Rbook/smoothing.txt", header=T)
attach(smooth)
sequence <- order(x)
par(mfrow=c(1,1))
plot(x, y)
lines(x[sequence], y[sequence])
lines(x,y, col="red")


# Plotting with a categorical explanatory variable
weather <- read.table("/Users/damiansp/Desktop/R Files/Rbook/SilwoodWeather.txt", header=T)
attach(weather)
month <- factor(month)
plot(month, upper)

trial <- read.table("/Users/damiansp/Desktop/R Files/Rbook/compexpt.txt", header=T)
attach(trial)
plot(clipping, biomass, xlab="treatment", ylab="yield")
par(mfrow=c(1,2))
boxplot(biomass~clipping)
boxplot(biomass~clipping, notch=T)

means <- tapply(biomass, clipping, mean)
par(mfrow=c(1,1))
barplot(means, xlab="treatment", ylab="yield")

data <- read.table("/Users/damiansp/Desktop/R Files/Rbook/Files/box.txt", header=T)
attach(data); names(data)
plot(response~factor(fact))
index <- order(tapply(response, fact, mean))
ordered <- factor(rep(index, rep(20,8)))
boxplot(response~ordered, notch=T, names=as.character(index), xlab="ranked treatments", ylab="response")
model <- aov(response~factor(fact))
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

attach(faithful)
(max(eruptions) - min(eruptions)) / (2*(1 + log(length(eruptions), base=2)))	#rule of thumb for bandwidth = 0.1926
par(mfrow=c(1,2))
hist(eruptions, 15, probability=T)
lines(density(eruptions, width=0.1926))
lines(density(eruptions, width=0.6), col=2)
rug(eruptions)
truehist(eruptions, 15)
lines(density(eruptions))
rug(eruptions)

#Time series plots
data(UKLungDeaths)
par(mfrow=c(1,1))
ts.plot(ldeaths, mdeaths, fdeaths, xlab="year", ylab='deaths', col=c(1:3))
data(sunspots)
plot(sunspots)
class(sunspots)
time(sunspots)

#Pie charts
data <- read.csv("/Users/damiansp/Desktop/R Files/Rbook/Files/piedata.csv")
data
pie(data$amounts, labels=data$names)

#The stripchart function
data(OrchardSprays)
with(OrchardSprays, stripchart(decrease ~ treatment, ylab='decrease', vertical=T, log='y'))

#Plots with multiple variables
#The pairs function
ozonedata <- read.table("/Users/damiansp/Desktop/R Files/Rbook/Files/ozone.data.txt", header=T)
attach(ozonedata)
names(ozonedata)
pairs(ozonedata, panel=panel.smooth)

#The coplot function
coplot(ozone ~ wind | temp, panel=panel.smooth)

#Interaction plots
yields <- read.table("/Users/damiansp/Desktop/R Files/Rbook/Files/splityield.txt", header=T)
attach(yields); names(yields)
interaction.plot(fertilizer, irrigation, yield)

#Special plots
#Trellis graphics
data <- read.table("/Users/damiansp/Desktop/R Files/Rbook/Files/panels.txt", header=T)
attach(data); names(data)
xyplot(weight ~ age | gender)

#Trellis functions:
#barchart = barplot
#bwplot = boxplot
#densityplot = density?
#dotplot
#histogram = hist
#qqmath
#stripplot
#qq = qqplot?
#xyplot = plot(x, y)
#levelplot
#contourplot
#cloud (3D scatterplot)
#wireframe (3D surfaces)
#splom (scatterplot matrix)
#parallel (parallel coordinate plots)
#rfs (residual and fitted value plot)
#tmd (Tukey mean-difference)
help(package = lattice)

data <- read.table("/Users/damiansp/Desktop/R Files/Rbook/Files/daphnia.txt", header=T)
attach(data); names(data)
trellis.par.set(col.whitebg())
bwplot(Growth.rate ~ Water + Daphnia | Detergent)

#Design plots
plot.design(Growth.rate~Water*Detergent*Daphnia)
plot.design(Growth.rate~Water*Detergent*Daphnia, fun="sd")plot.design(Growth.rate~Water*Detergent*Daphnia, fun="median")

#Effect sizes
model <- lm(Growth.rate~Water*Detergent*Daphnia)
summary(model)
plot(model)
daph.effects <- all.effects(model)
daph.effects2 <- allEffects(model)
plot(daph.effects, "Water:Detergent:Daphnia")
plot(daph.effects2, "Water:Detergent:Daphnia")

#Bubble plots
ddd <- read.table("/Users/damiansp/Desktop/R Files/Rbook/Files/pgr.txt", header=T)
detach(data); attach(ddd); names(ddd)
bubble.plot(hay, pH, FR)	#not available

bubble.plot <- function(xv, yv, rv, bs=0.1) {
	r <- rv / max(rv)
	yscale <- max(yv) - min(yv)
	xscale <- max(xv) - min(xv)
	plot(xv, yv, type='n', xlab=deparse(substitute(xv)), ylab=deparse(substitute(yv)))
	
	for(i in 1:length(xv)) bubble(xv[i], yv[i], r[i], bs, xscale, yscale)
	}
	
bubble <- function(x, y, r, bubble.size, xscale, yscale) {
	theta <- seq(0, 2*pi, pi/200)
	yv <- r*sin(theta)*bubble.size*yscale
	xv <- r*cos(theta)*bubble.size*xscale
	lines(x + xv, y + yv)
	}

numbers <- read.table("/Users/damiansp/Desktop/R Files/Rbook/Files/longdata.txt", header=T)
detach(ddd); attach(numbers); names(numbers)
plot(xlong, ylong)
plot(jitter(xlong), jitter(ylong), pch=".")
sunflowerplot(xlong, ylong)

save.image("Ch5Graphics.RData")
quit()