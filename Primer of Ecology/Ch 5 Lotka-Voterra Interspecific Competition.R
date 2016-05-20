##################################################
#                                                #
#  Ch 5 Lotka-Voterra Interspecific Competition  #
#                                                #
##################################################

load('/Users/damiansp/Desktop/R Files/Primer of Ecology/PrimerOfEcology.RData')
library(deSolve)

# 5.1 Discrete and Continuous Time Models
	# 5.1.1 Discrete time model

# p. 137 Code for model of discrete logistic competeition
dlvcomp2 <- function(N, alpha, rd=c(1,1)) {
	N1.t1 <- N[1] + rd[1]*N[1]*(1 - alpha[1,1]*N[1] - alpha[1,2]*N[2])
	N2.t1 <- N[2] + rd[2]*N[2]*(1 - alpha[2,1]*N[1] - alpha[2,2]*N[2])
	c(N1.t1, N2.t1)
	}	#Note N is a vector and alpha a matrix

	# 5.1.2 Effects of alpha
# p. 138 Discrete logistic competition dynamics
alphs <- matrix(c(0.01, 0.005, 0.008, 0.01), ncol=2, byrow=T)
t <- 20
N <- matrix(NA, nrow=t+1, ncol=2)
N[1, ] <- c(10,10)

for(i in 1:t) {
	N[i+1, ] <- dlvcomp2(N[i,], alphs)
	}

matplot(0:t, N, type='l', ylim=c(0,110))
abline(h=1/alphs[1,1], lty=3)
text(0, 1/alphs[1,1], "K", adj=c(0,0))
legend("bottomright", c(expression("Sp1"*(alpha[21]==0.008)), expression("Sp2"*(alpha[12]==0.005))), lty=1:2, col=1:2, bty="n")

	# 5.1.3 Continuous time model
# p. 140 Continuous logistic competition
lvcomp2 <- function(t, n, parms) {
	with(as.list(parms), {
		dn1dt <- r1*n[1]*(1 - a11*n[1] - a12*n[2])
		dn2dt <- r2*n[2]*(1 - a22*n[2] - a21*n[1])
		list(c(dn1dt, dn2dt))
		})
	}

parms <- c(r1=1, r2=0.1, a11=0.2, a21=0.1, a22=0.02, a12=0.01)
initialN <- c(2,1)
out <- ode(y = initialN, times = 1:100, func = lvcomp2, parms = parms)
matplot(out[,1], out[,-1], type='l')



# 5.2 Equilibria
	# 5.2.1 Isoclines
# p. 142 Graphing an isocline
a <- matrix(c(0.01, 0.005, 0.005, 0.01), ncol=2, byrow=T)
N2iso <- expression(1/a[2,2] - (a[2,1]/a[2,2])*N1)
N1 <- 0:200
plot(N1, eval(N2iso), type='l', ylim=c(0,200), xlim=c(0,200), xlab=expression("N"[1]), ylab=expression("N"[2]))
arrows(x0=90, y0=150, x1=90, y1=80, length=0.1)
arrows(x0=75, y0=0, x1=75, y1=50, length=0.1)

save.image('/Users/damiansp/Desktop/R Files/Primer of Ecology/PrimerOfEcology.RData')
quit()