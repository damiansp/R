######################
#                    #
#  Ch 2 Correlation  #
#     Exercises      #
#                    #
######################

load("TimeSeries.RData")
options(digits = 3)

# (1)
	#(a)
varnish <- read.table("http://www.massey.ac.nz/~pscowper/ts/varnish.dat", header=T)
head(varnish)
attach(varnish)
plot(x, y); abline(lm(y~x))
cor(x, y)	#-0.253
detach(varnish)

	#(b)
guesswhat <- read.table("http://www.massey.ac.nz/~pscowper/ts/guesswhat.dat", header=T)
head(guesswhat)
attach(guesswhat)
plot(x, y); abline(lm(y~x))
cor(x, y)	#0.0646
detach(guesswhat)

# (2)
shiraz <- ts(c(39, 35, 16, 18, 7, 22, 13, 18, 20, 9, -12, -11, -19, -9, -2, 16))
chardonnay <- ts(c(47, -26, 42, -10, 27, -8, 16, 6, -1, 25, 7, -5, 3))
plot(shiraz); abline(h=0, lty=2)
plot(chardonnay); abline(h=0, lty=2)
plot(shiraz[1:(length(shiraz)-1)], shiraz[2:length(shiraz)])
plot(chardonnay[1:(length(chardonnay)-1)], chardonnay[2:length(chardonnay)])
acf(shiraz)
acf(chardonnay)

# (3)
#Objects already stored as Global, Global.ts
	#(a)
Global.ts
plot(decompose(Global.ts))
Global.decomp <- decompose(Global.ts)
sd(Global.ts)	#0.274
sd(Global.ts - Global.decomp$seasonal)	#0.272; i.e. seasonal effect is negligible
plot(Global.decomp$trend)
lines(Global.decomp$trend + Global.decomp$seasonal, col=2)
	#(b)
acf(Global.decomp$ran[7:(length(Global.decomp$ran)-6)])

# (4)
# Font.dat doesn't exist


save.image("TimeSeries.RData")
quit()
