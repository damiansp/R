#Chapter 1: Simple Density-Independent Growth
#1.1 Geometric growth

#(a)
t <- 1996:2005
N <- c(150, 100, 125, 200, 225, 150, 100, 175, 100, 150)
plot(t, N, type="l")

#(b)
R <- N[2 : (length(N))] / N[1 : (length(N) - 1)]
plot(t[2 : length(t)], R, type="l", xlim=c(1996, 2005))

#(c)
(lamA <- mean(R))	#1.088
(lamB <- prod(R)^(1/length(R)))	#1

#(d)
#b/c geomean(R) = 1, pop size at 2025 = N0 = 150

#(d*)
N0 <- 150
years <- 1996:2010
popSim <- function(N0, lamA, lamB, years) {
	popProj.arith <- popProj.geo <-  numeric(length(years))
	popProj.arith[1] <- popProj.geo[1] <- N0
	for(i in 2:length(years)) {
		popProj.arith[i] <- popProj.arith[i-1] * lamA
		popProj.geo[i] <- popProj.geo[i-1] * lamB
		}
	list(popProj.arith, popProj.geo)	
	}
(proj <- popSim(N0, lamA, lamB, years))

plot(t, N, xlim=c(1996, 2010), ylim=c(150, 500))
lines(years, proj[[1]], lty=2)
lines(years, proj[[2]])
legend(1996, 500, c("GeoMean", "ArithMean"), title="Projections based on:", lty=1:2, xjust=0)



#1.2 Doubling Time
#(a) Doubling means: N[t]==2*N0; 2*N0==N0*exp(r*t); 2==exp(r*t); log(2)==r*t*log(exp(1));
#	t==log(2) / r
#(b) Tripling time: N[t]==3*N0; 3*N0==N0*exp(r*t); 3==exp(r*t); log(3)==r*t*log(exp(1));
#	log(3)==r*t; t==log(3)/r
#(c) N0 <- 1e3; N[6] <- 2e9 <<t = 6 hrs>>
#	(2e6)*N0==N0*exp(r*t); 2e6==exp(6*r); log(2e6)==6*r;
(r <- log(2e6)/6);	#r = 2.418110
(t <- log(2)/r);	#t = 0.2866484 hrs = 17.19890 mins



#1.3 Human Population Growth
#(a) N1700 <- 630000000; N2003 <- 6300000000 What is r?
#	10*(N1700)==N1700*exp(r*t)	#t = 2003 - 1700 = 303
#	10 == exp(303*r);	
r <- log(10)/303; #r = 0.00759929
#(b)
t <- 1700:2003
plot(t, 630000000*exp(r*(t-1700)), type="l")
#(c) 2N == N*exp(r*t); 2 == exp(r*t); log(2) == r*t
t2 <- log(2)/r; #t2 (doubling time) = 91.21209 yrs
doubYrs <- numeric(10)
doubYrs[1] <- 1700 + t2
for(i in 2:10) {doubYrs[i] <- doubYrs[i-1] + t2}
points(doubYrs, 630000000*exp(r*(doubYrs-1700)))