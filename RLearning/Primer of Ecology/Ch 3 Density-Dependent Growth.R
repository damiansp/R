#p. 64 Graphing Population Size
Nts <- dlogistic()
t <- 15
a <- 0.01
plot(0:t, Nts)

#p. 65 Per Capita Population Growth Increment vs. N
total.incr <- Nts[1:t + 1] - Nts[1:t]
per.capita.incr <- total.incr / Nts[1:t]
plot(Nts[1:t], total.incr, type="l")
plot(Nts[1:t], per.capita.incr, type="l")

#p. 68a Numerical Evaluation of Initial Conditions
N0s <- c(0, runif(70) * 1.9 * 1/a)
N <- sapply(N0s, function(n) dlogistic(N0 = n))
matplot(0:t, N, type="l", lty=1, col=1)

#p. 68b Numerical Evaluation of alpha
a.s <- 1 / runif(70, min=50, max=1000)
N <- sapply(a.s, function(a) dlogistic(alpha = a, t=15))
matplot(0:t, N, type="l", lty=1, col=1)

#...

#p.73 Bifurcation Plot: Attactors as a Function of r[d]
num.rd <- 4001; t <- 400
rd.s <- seq(1, 3, length=num.rd)
tmp <- sapply(rd.s, function(r) dlogistic(rd=r, N0=99, t=t))
tmp.s <- stack(as.data.frame(tmp))
names(tmp.s) <- c("N", "Old.Column.ID")
tmp.s$rd <- rep(rd.s, each=t+1)
tmp.s$time <- rep(0:t, num.rd)
N.bif <- subset(tmp.s, time > 0.5*t)
plot(N~rd, data=N.bif, pch=".", xlab=quote("r"["d"]), xlim=c(2.65, 3))
abline(h=0)

#p. 74 Sensitivity to Initial Conditions
N.init <- c(97,98,99)
t <- 30
Ns <- sapply(N.init, function(n0) dlogistic(rd=2.7, N0=n0, t=t))
matplot(0:t, Ns, type="l")

#p. 78 Density Dependence on Birth and Death Rates
B.N <- expression(-a * N^2 + e*N -f)
D.N <- expression(g*N^2)
a <- 1/1600; e <- 80 * a; f <- 0.2; g <- 4e-05; N <- 0:100
plot(N, eval(B.N), type="l", ylab="B(N), D(N)")
lines(N, eval(D.N), lty=2)
abline(h=0, lty=3)
legend("bottom", c("Effect on Birth Rate", "Effect on Death Rate"), lty=1:2, bty="n")

plot(N, eval(B.N) + eval(D.N), type="l", ylim=c(-0.4, 1), ylab="Net Dens. Dep., F(N)")
abline(h=0, lty=2)
curve(1-0.01*x, 0, 100, lty=2, add=T)
legend("topright", c("Generalized", "Logistic"), lty=1:2, bty="n")

#p. 82 Growth Rate vs N.
pop.growth.rate <- expression(r*N*(1-alpha*N))
r <- 1; alpha <- 0.01; N <- 0:120

plot(N, eval(pop.growth.rate), type="l", ylab="Pop. Growth Rt (dN/dt)", xlab="N")
abline(h=0)
abline(v=100, lty=2)
legend("topright", c("r=1", "K"), lty=c(1,2))
N <- c(0, 10, 50, 100, 115)
points(N, eval(pop.growth.rate))
text(N, eval(pop.growth.rate), letters[1:5], adj=c(0.5, 2))
arrows(20, 2, 80, 2, length=0.1, angle=15)
arrows(122, -2, 109, -2, length=0.1, angle=15)
text(30, -15, "a and d are equilibria points;\n d is stable")

dF.dN <- deriv(pop.growth.rate, "N")
N <- c(0, 1/alpha)
eval(dF.dN)	#1 corresponds to first N val (0), pos. val. indicates perturbation will incr w time (N=0 is a repellor); -1 indicates that the perturbation will decrease w time for N=1/alpha (N=1/a is an attractor)

#3.2.4 Dynamics
#p. 85 Function for an ODE (Ordinary Differential Equation)
clogistic <- function(times, y, parms) {
	n <- y[1]
	r <- parms[1]
	alpha <- parms[2]
	dN.dt <- r*n*(1 - alpha*n)
	return(list(c(dN.dt)))
	}

prms <- c(r=1, alpha=0.01); init.N <- c(1); t.s <- seq(0.1, 10, 0.1)
library(deSolve)
out <- ode(y=init.N, times=t.s, clogistic, parms=prms)
plot(out[,1], out[,2], type="l", xlab="time", ylab="N")

# p. 86 Plotting Random Populations	
outmat <- matrix(NA, nrow=length(t.s), ncol=20)
for(j in 1:20) outmat[,j] <- {
	y <- runif(1, 0, 120)
	prms <- c(r=runif(1, 0.01, 2), alpha=0.01)
	ode(y, times=t.s, clogistic, prms)[,2]
	}
matplot(t.s, outmat, type="l", ylab="Random Populations")



#3.3 Other Forms of Density Dependence
#p. 87 Theta-logistic function
thetalogistic <- function(times, y, parms){
	n <- y[1]
	with(as.list(parms), {
		dN.dt <- r*n*(1 - (alpha*n)^theta)
		return(list(c(dN.dt)))
		})
	}

#p. 88 Theta-logistic density dependence
r <- 0.75; alpha <- 0.01; theta <- c(0.5, 1, 2)
N <- 0:110
theta.out <- sapply(theta, function(th) {
	1 - (alpha*N)^th
	})	
matplot(N, theta.out, type='l')
abline(h=0)
legend("topright", legend=paste('theta = ', c(2,1,0.5)), lty=c(3:1), col=c(3:1))

#p. 89 Theta-logistic growth rate
thetaGR.out <- sapply(theta, function(th) {
	r*N*(1 - (alpha*N)^th)
	})
matplot(N, thetaGR.out, type='l')
abline(h=0)
legend("bottomleft", legend=paste("theta = ", c(2, 1, 0.5)), lty=3:1, col=3:1)

#Theta-logistic dynamics
prms <- c(r <- 0.75, alpha <- 0.01, theta = 1)
thetaN <- sapply(theta, function(th) {
	prms["theta"] <- th
	ode(y = 1, t.s, thetalogistic, prms)[,2]
	})
matplot(t.s, thetaN, type='l') ##error in this code: only does 1 theta val



#3.4 Maximum Sustained Yield
#p. 91 Maximum Sustained Yield and Harvesting
r <- 0.5; alpha <- 0.01; N <- 0:105 
plot(N, eval(pop.growth.rate), type='l', ylim=c(-3,15), ylab="dN/dt and FN")
abline(h=0)
F <- r/2	#per capita fishing mortality desired for max harvest
abline(c(0,F), lty=2) #same as points(N, F*N); desired harvest rate for each 



#3.5 Fitting Models to Data
#3.5.2 Initial Data Exploration
library(nlme)
library(lattice)
data(ClostExp)
summary(ClostExp)
xyplot(No.per.ml ~ Day | Nutrients, ClostExp, group=rep, type="b", scales=list(relation="free"), auto.key=list(columns=4, linest=T))
#extract days with very high no.per.ml
subset(ClostExp, Nutrients=="high" & No.per.ml > 1000)
subset(ClostExp, Nutrients=="low" & No.per.ml > 100)

#3.5.3 A Time-Implicit Approach
Hi.c <- subset(ClostExp, Nutrients=="high" & rep=="c")
n <- nrow(Hi.c)
N.change <- Hi.c$No.per.ml[-1] / Hi.c$No.per.ml[-n]
interval <- diff(Hi.c$Day)
pgr <- log(N.change) / interval
Nt <- Hi.c$No.per.ml[-n]
plot(pgr ~ Nt)
mod1 <- lm(pgr ~ Nt)
abline(mod1)
summary(mod1)

EachPop <- lapply(split(ClostExp, list(ClostExp$Nutrients, ClostExp$rep)), function(X) {
	n <- nrow(X)
	N.change <- (X$No.per.ml[-1] / X$No.per.ml[-n])
	interval <- diff(X$Day)
	data.frame(Nutrients=as.factor(X$Nutrients[-n]), rep=as.factor(X$rep[-n]), pgr=log(N.change) / interval, Nt=X$No.per.ml[-n])
	})

AllPops <- NULL
for(i in 1:length(EachPop)) AllPops <- rbind(AllPops, EachPop[[i]])

xyplot(pgr ~ Nt | rep*Nutrients, AllPops, layout=c(4,2,1), scales=list(x=list(rot=90)), panel=function(x, y) {
	panel.grid(h=-1, v=-4)
	panel.xyplot(x, y, type=c("p","r"))
	})

AllPops$ID <- with(AllPops, Nutrients:rep)
modSlope <- lme(pgr ~ Nt*Nutrients, data=AllPops, random=~1 | ID)	#random: each ID level has a unique random contribution to the model

xyplot(resid(modSlope) ~ fitted(modSlope))
qqmath(~resid(modSlope) | ID, data=AllPops)
qqmath(~ranef(modSlope))

modSlope2 <- update(modSlope, weights=varExp())
anova(modSlope, modSlope2)
anova(modSlope2)
summary(modSlope2)$tTable
cfs <- fixef(modSlope2)
cfs
-cfs[2] / cfs[1]	#Nt coef / intercept = alpha 
-(cfs[2] + cfs[4]) / (cfs[1] + cfs[3])	#(Nt coef + Nt:NutrientsLow) / (Intercept + NutrientsLow)

#3.5.4 A Time-Explicit Approach
ilogistic <- function(t, alpha, N0, r) {
	N0*exp(r*t) / (1 + alpha*N0*(exp(r*t) - 1))
	}

plot(No.per.ml ~ Day, ClostExp, subset=Nutrients=="high")
curve(ilogistic(x, alpha=-cfs[2] / cfs[1], N0=6, r=cfs[1]), 1, 60, add=T)

Cmod.list <- nlsList(No.per.ml ~ ilogistic(Day, alpha, N0, r) | ID, data=ClostExp, start=c(alpha=1/100, N0=5, r=0.15), control=list(maxiter=1000))

plot(coef(Cmod.list), layout=c(3,1))
plot(Cmod.list)

Cmod.all <- nlme(Cmod.list, random=pdDiag(form=alpha + N0 + r ~ 1), weights=varPower())
cfs2 <- fixef(Cmod.all)
cfs2

Cmod.all2 <- update(Cmod.all, fixed=list(alpha~Nutrients, N0, r~1), start=c(cfs[1], 0, cfs2[2], cfs2[3]))	#Error in code; N0 not defined