###############################
#                             #
#  Ch 4 Populations in Space  #
#                             #
###############################

load('/Users/damiansp/Desktop/R Files/Primer of Ecology/Ch4PopulationsInSpace.RData')
library(deSolve)
library(primer)

# 4.1 Source-Sink Dynamics
		# p. 114 The spatial demographic Pulliam-like model
L1 <- 2; L2 <- 0.4	#lambda for each pop
A <- matrix(c(1, L1-1, 0, L2), nrow=2)
eigen(A)
L1s <- seq(1, 3, 0.01)	#vary source lambda from 1 ~ 3
p1 <- sapply(L1s, function(l1) {
	A[2, 1] <- l1 - 1
	eigen(A)$vectors[1,1] / sum(eigen(A)$vectors[,1])
	})	#proportion of population in the source
plot(L1s, p1, type='l', ylab="Proportion of population at source", xlab=expression(lambda[1]))



# 4.3 Related Models
	# 4.3.1 The classic Levins model
		# p. 118 The Levins metapopulation model
levins <- function(t, y, parms) {	#parms should include ci, e
	p <- y[1]
	with(as.list(parms), {
		dp <- ci*p*(1 - p) - e*p
		return(list(dp))
		})
	}


parms <- c(ci=0.15, e=0.05); Initial.p <- 0.01
out.L <- data.frame(ode(y=Initial.p, times=1:100, func=levins, parms=parms))	#ode = ordinary differential equations
plot(out.L[,2] ~ out.L[,1], type="l", ylim=c(0,1), ylab="p", xlab="time")

		# p. 119 The propagule rain (island-mainland) metapopulation model
gotelli <- function(t, y, parms) {
	p <- y[1]
	with(as.list(parms), {
		dp <- ce*(1 - p) - e*p
		return(list(dp))
		})
	}



	# 4.3.3 The Rescue Effect and the Core-Sattelite Model
		# p. 121a The core-sattelite metapopulation model
hanski <- function(t, y, parms) {
	p <- y[1]
	with(as.list(parms), {
		dp <- ci*p*(1 - p) - e*p*(1 - p)
		return(list(dp))
		})
	}
		# p. 121b Graphing propagule rain and core-sattelite models
prms <- c(ci=0.15, ce=0.15, e=0.05)
out.IMH <- data.frame(ode(y=Initial.p, times=1:100, func=gotelli, parms=prms))
out.IMH[["pH"]] <- ode(y=Initial.p, times=1:100, func=hanski, parms=prms)[,2]
matplot(out.IMH[,1], out.IMH[,2:3], type='l', lty=1, col=1:2, ylab="y", xlab="time")
legend("topleft", c("Hanski", "Gotelli"), lty=1, col=2:1, bty="n")

		# Core-satellite equilibria
		# p. 123 An equilibrium for the core-satellite metapopulation model
dpdtCS <- expression((ci - e)*p*(1-p))
ci <- 0.15; e <- 0.05; p <- seq(0,1,length=50)
plot(p, eval(dpdtCS), type='l', ylab='dp/dt')
legend('topleft', legend=(expression(c[i] > e)))



# 4.4 Parallels with Logistic Growth



# 4.5 Habitat Destruction
		# p. 125 Habitat destruction model
lande <- function(t, y, parms) {
	p <- y[1]
	with(as.list(parms), {
		dp <- ci*p*(1 - D - p) - e*p
		return(list(dp))
		})
	}

		# p. 126 Illustrating the effects of habitat destruction
prmsD <- c(ci=0.15, e=0.05, D=0)
Ds <- c(0, 0.2, 0.5)
Initial.p <- 0.01
t <- 1:200

ps <- sapply(Ds, function(d) {
	prmsD["D"] <- d
	ode(y=Initial.p, times=t, func=lande, parms=prmsD)[,2]
	})

matplot(t, ps, type='l', ylab='p', xlab='time')
text(c(200,200,200), ps[200,], paste("D = ", Ds, sep=''), adj=c(1,0))

		# p. 128 The unexpected collapse of core populations
C1 <- ode(y=0.999, times=t, func=hanski, parms=c(ci=0.2, e=0.01))
C2 <- ode(y=0.999, times=t, func=hanski, parms=c(ci=0.2, e=0.25))
L2 <- ode(y=0.95, times=t, func=levins, parms=c(ci=0.2, e=0.25))

matplot(t, cbind(C1[,2], C2[,2], L2[,2]), type='l', ylab='p', xlab='Time')
legend('center', c('c > e', 'c < e', 'c < e (Levins)'), lty=1:3, col=1:3, bty='n')



# 4.6 Core-Satellite Simulations
args(MetaSim)	#primer package
out.CS.10 <- MetaSim(Time=50, method='hanski', NSims=10)	#outputs are: $method, $t (time points), $Ns (a matrix of t rows and NSims cols), $Parameters (NSims, ci, e, phi, p0, D)
matplot(out.CS.10$t, out.CS.10$Ns, type='l', xlab='Time', ylab='Occupancy', sub=out.CS.10$method)

system.time(out.CS.Lots <- MetaSim(Time=1000, method='hanski', NSims=50))
matplot(out.CS.Lots$t, out.CS.Lots$Ns, type='l', xlab='Time',  ylab='Occupancy', sub=out.CS.Lots$method)
hist(out.CS.Lots$Ns[1001,], breaks=10, main='', xlab=expression('Occupancy('*italic('p')*')'), ylab='No. species', sub=out.CS.Lots$method)

out.L.Lots <- MetaSim(NSims=50, Time=1000, method='levins')
matplot(out.L.Lots$t, out.L.Lots$Ns, type='l')
hist(out.L.Lots$Ns[1001,], breaks=10)


save.image(file="Ch4PopulationsInSpace.RData")
quit()