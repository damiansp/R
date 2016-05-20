#===========================================================================#
#                   															#
# Examples and Exercises from Bivand et al. "Applied Spatial Data Analysis 	#
#  with R"                													#
# Chapter 7: Spatial Point Pattern Analysis		<sppa_mod.r>					#
# Damian Satterthwaite-Phillips <damiansp@gmail.com>      					#
# Updated: 07Jul2015             											#
#                   															#
#===========================================================================#

rm(list = ls())
library(cubature)
library(lattice)
library(maps)
library(maptools)
library(mgcv)
library(raster)
library(RColorBrewer)
library(rgdal)
library(rgeos)
library(sp)
library(spacetime)
library(spatstat)
library(splancs)
library(spgrass6)
library(xts)

load('~/Desktop/R/Spatial/SpatialAnalysisBook/SpatialAnalysis.RData')



# 7.2 Packages for the Analysis of Spatial Point Patterns
data(japanesepines)
summary(japanesepines)
plot(japanesepines)

spjpines = as(japanesepines, 'SpatialPoints')
summary(spjpines)
plot(spjpines, pch = 16)
box()

# convert to unit square
spjpines1 = elide(spjpines, scale = T, unitsq = T)
summary(spjpines1)
plot(spjpines1, pch = 16)
box()

pppjap = as(spjpines1, 'ppp')
summary(pppjap)
plot(pppjap)

data(redwoodfull)
spred = as(redwoodfull, "SpatialPoints")
data(cells)
spcells = as(cells, "SpatialPoints")
dpp = data.frame(rbind( coordinates(spjpines1), coordinates(spred), 
 					 	 coordinates(spcells) ))
njap = nrow(coordinates(spjpines1))
nred = nrow(coordinates(spred))
ncells = nrow(coordinates(spcells))
dpp = cbind(dpp, c( rep("JAPANESE",njap), rep("REDWOOD", nred), 
					 rep("CELLS", ncells) )) 
names(dpp) = c("x", "y", "DATASET")
print(xyplot(y~x|DATASET, data = dpp, pch = 19, aspect = 1))


spasthma = readOGR('Desktop/R/Spatial/SpatialAnalysisBook/north_derby_asthma/', 
					'spasthma')
spbdry = readOGR( 'Desktop/R/Spatial/SpatialAnalysisBook/north_derby_asthma/', 				  'spbdry' )
spsrc = readOGR( 'Desktop/R/Spatial/SpatialAnalysisBook/north_derby_asthma/', 				 	 'spsrc' )
sproads = readOGR( 'Desktop/R/Spatial/SpatialAnalysisBook/north_derby_asthma/', 					'sproads' )

par(mar = c(0, 0, 0, 0))
plot(spbdry, axes = TRUE, lwd = 0.5)
plot(sproads, add = TRUE, lwd = 2, col = "darkslategrey")
c_c = (spasthma$Asthma == "case") + 1
plot(spasthma[c_c == 1,], add = TRUE, pch = 16, cex = 0.6, col = "mediumaquamarine")
plot(spasthma[c_c == 2,], add = TRUE, pch = 16, cex = 0.75, col = "goldenrod2")
plot(spsrc, pch = 22, add = TRUE, cex = 1.2, bg = "brown4")
legend( "bottomright", legend = c("controls", "cases", "pollution sources"), 	
		pch = c(16, 16, 22), pt.cex = c(0.6, 0.75, 1.2), pt.bg = c(NA, NA, "brown4"), 
		col = c("mediumaquamarine", "goldenrod2", "black"), bty = "n" ) 



# 7.3 Preliminary Analysis of a Point Pattern
	# 7.3.1 Complete Spatial Randomness (CSR)
	# 7.3.2 G Function: Distance to the Nearest Event
	# Def: d[i] = min[j]{ d[i, j], All(j ! = i) }
	
	# G(r) = (#{ d[i]: d[i] ≤ r, all(i) }) / n
	# Numerator is no. of elem.s in set of dists < = d; n = total no. of points
	# if CSR -> G(r) 1 - exp(-lambda * pi * r^2); lambda = mean no. of events 
	# per unit area

	r = seq(0, sqrt(2) / 6, by = 0.005)
	# nrank = 3: means show 3 sd (i.e., 99% CI)
	envjap = spatstat::envelope( as(spjpines1, 'ppp'), fun = Gest, r = r, nrank = 3,
								 nsim = 99 )
	envred = spatstat::envelope( as(spred, 'ppp'), fun = Gest, r = r, nrank = 3, 
								 nsim = 99 )
	envcells = spatstat::envelope( as(spcells, 'ppp'), fun = Gest, r = r, nrank = 3, 
									nsim = 99 )
	Gresults = rbind(envjap, envred, envcells)
	Gresults = cbind(Gresults, y = rep( c('Japanese', 'Redwood', 'Cells'), 
					 				 each = length(r) ))
	
	print(xyplot( obs ~ theo | y, data = Gresults, type = "l", xlab = "theoretical",
				 ylab = "observed", 
				 panel = function(x, y, subscripts) {
 				  lpolygon( c(x, rev(x)), 
 				  			c( Gresults$lo[subscripts], 
 							 	 rev(Gresults$hi[subscripts]) ), 
 					 			border = "darkgrey", col = "lightgrey" )
					 llines(x, y, col = "black", lwd = 2)
				 } ))
	# lines below the CI envelope are dispersed; lines above are clustered			 
	# 7.3.3 F Function: Distance from a Point to the Nearest Event
	# if CSR -> F(r) = 1 - exp(-lambda * pi * r^2)
	Fenvjap = spatstat::envelope( as(spjpines1, 'ppp'), fun = Fest, r = r, nrank = 3, 
								 nsim = 99 )
	Fenvred = spatstat::envelope( as(spred, 'ppp'), fun = Fest, r = r, nrank = 3, 
								 nsim = 99)
	Fenvcells = spatstat::envelope( as(spcells, 'ppp'), fun = Fest, r = r, nrank = 3, 
									 nsim = 99 )
	Fresults = rbind(Fenvjap, Fenvred, Fenvcells)
	Fresults = cbind( Fresults, y = rep(c('Japanese', 'Redwood', 'Cells'), 
					 each = length(r)) )
	
	print(xyplot( obs ~ theo | y, data = Fresults, type = "l", xlab = "theoretical", 
				 ylab = "observed", 
				 panel = function(x, y, subscripts) {
 				  lpolygon( c(x, rev(x)), 
 				  			c( Fresults$lo[subscripts], 
 								 rev(Fresults$hi[subscripts]) ),
 							 	border = "gray", col = "cyan" )
 		 		 	 llines(x, y, col = "black", lwd = 2)
				 } ))
	# lines below the CI envelope are dispersed; lines above are clustered

# 7.4 Statistical Analysis of Spatial Point Processes
	# 7.4.1 Homogeneous Poisson Processes (HPP)
	# 7.4.2 Inhomogeneous Poisson Processes (IPP)
	# 7.4.3 Estimation of Intensity
	x = runif(10)
	nx = length(x)
	bw = 0.1

	k = density(x, bw = bw, kernel = "biweight")
	k$y = k$y*nx
	
	plot(k, ylab = "Intensity", main = "", col = 2)
	points(x, rep(0, nx), pch = 20)
	for(i in 1:length(x)) {
 		lines(density(x[i], bw = bw, kernel = "biweight"), col = 4, lty = 2)
 	}
	legend('topleft', legend = c("Intensity", "Kernel"), lty = c(1,2), col = c(2, 4))


	
	mserwq = mse2d( as.points(coordinates(spred)), 
					 as.points(list(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1))), 
					 100, 0.15 )
	bwq = mserwq$h[which.min(mserwq$mse)]
	bwq
	
	mserw = bw.diggle(as(spred, 'ppp'))
	bw = as.numeric(mserw)
	bw
	
	par(mfrow = c(2, 1))
	plot( mserwq$h, mserwq$mse, xlab = "Bandwidth", ylab = "MSE", type = "l", 
		 ylim = c(-2,50), main = "Quartic kernel", xlim = c(0, 0.06))
	i = which.min(mserwq$mse)
	points(mserwq$h[i], mserwq$mse[i], col = 2)
	abline(v = mserwq$h[i], col = 4, lty = 3)
	
	plot( mserw, main = "Gaussian kernel", xlab = "Bandwidth", ylab = "MSE", 
		 xlim = c(0, 0.06) )
	points(attr(mserw, "h")[attr(mserw, "iopt")], bw, col = 2)

	# Choice of kernel makes little difference, but choice of bandwidth matters 
	# a lot
	# Estimate intensity of redwood data using different bandwidths:
	par(mfrow = c(1, 1))
	poly = as.points(list(x = c(0, 0, 1, 1), y = c(0, 1, 1, 0)))
	sG = Sobj_SpatialGrid(spred, maxDim = 100)$SG
	grd = slot(sG, 'grid')
	summary(grd)
	k0 = spkernel2d(spred, poly, h0 = bw, grd)	# bw = 0.020
	k1 = spkernel2d(spred, poly, h0 = 0.05, grd)
	k2 = spkernel2d(spred, poly, h0 = 0.1, grd)
	k3 = spkernel2d(spred, poly, h0 = 0.15, grd)
	df = data.frame(k0, k1, k2, k3)
	kernels = SpatialGridDataFrame(grd, data = df)
	summary(kernels)
	
	# Similar approach with Gaussian kernel from spatstat package:
	cc = coordinates(kernels)
	xy = list(x = cc[,1], y = cc[,2])
 	k4 = density(as(spred, "ppp"), 0.5*bw, dimyx = c(100, 100), xy = xy)
	kernels$k4 = as(k4, "SpatialGridDataFrame")$v
	k5 = density(as(spred, "ppp"), 0.5*.05, dimyx = c(100, 100), xy = xy)
	kernels$k5 = as(k5, "SpatialGridDataFrame")$v
	k6 = density(as(spred, "ppp"), 0.5*.1, dimyx = c(100, 100), xy = xy)
	kernels$k6 = as(k6, "SpatialGridDataFrame")$v
	k7 = density(as(spred, "ppp"), 0.5*.15, dimyx = c(100, 100), xy = xy)
	kernels$k7 = as(k7, "SpatialGridDataFrame")$v
	summary(kernels)
	
	gp = brewer.pal(8, "Blues")
	print(spplot( kernels, 
				 at = c(0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000), 				 col.regions = colorRampPalette(gp)(15)[1:12], 
		 		 names.attr = c( paste( "Q bw = ", round(bw, digits = 4), sep = "",
		 		 	 				 collapse = "" ),
		 			 			"Q bw = 0.05", "Q bw = 0.1","Q bw = 0.15", 
		 			 			paste( "G bw = ", round(.5*bw, digits = 4), sep = "", 
		 			 				 collapse = "" ), 
		 			 			"G bw = 0.025", "G bw = 0.05","G bw = 0.075" ), 	
		 		 cex = 0.7, colorkey = FALSE ))
	
	
	# 7.4.4 Likelihood of an Inhomogeneous Poisson Process
	# Unlike the kernel methods above, this is a (semi-)parametric estimation
	# L(lambda) = sum(log(lambda) * x[i]) - integral(lambda(x) dx)
	# (The integral is over area A, and is the expected number of cases of the 
	# IPP)
	# To estimate parameters:
		# log(lambda) * x = sum(beta[j] * z[j] * x); with covariates z[j]*x
	loglambda = function(x, alpha, beta) {
		l = alpha + sum(beta * c(x, x^2, prod(x)))
		return(l)
	}
	
	L = function(alphabeta, x) {
		l = apply(x, 1, loglambda, alpha = alphabeta[1], beta = alphabeta[-1])
		l = sum(l)
		intL = adaptIntegrate( lowerLimit = c(0, 0), upperLimit = c(1, 1), 
								fDim = 1, tol = 1e-08,
							 f = function( x, alpha = alphabeta[1], 
							 				beta = alphabeta[-1]) {
							 			exp(loglambda(x, alpha, beta))
							 } )
		l = l - intL$integral
		return(l)
	}
	
	data(lansing)
	x = as.points(lansing[lansing$marks == 'maple', ])
	plot(x)
	optbeta = optim( par = c(log(514), 0, 0, 0, 0, 0), fn = L, 
					 control = list(maxit = 1000, fnscale = -1), x = x )
	optbeta	
	
	# Parameters may also be obtained as follows:
	lmaple = lansing[lansing$marks == 'maple',]
	ppm(Q = lmaple, trend = ~ x + y + I(x^2) + I(y^2) + I(x*y))
	
	grd = GridTopology( cellcentre.offset = c(0.005,0.005), 
						 cellsize = c(0.01, 0.01),
 						 cells.dim = c(100, 100) )
	lambda = exp(apply( coordinates(grd), 1, 
						 function(X, alpha, beta) {
  						loglambda(X, alpha, beta)
					  }, 
					  alpha = optbeta$par[1], 
					  beta = optbeta$par[-1] ))

	parint = SpatialGridDataFrame(grd, data = data.frame(intensity = lambda))

	lyt = list("sp.points", SpatialPoints(x), pch = 19, col = "black", cex = 0.7)
	print(spplot( parint, at = seq(0,1400,length.out = 8),
	 			 col.regions = colorRampPalette(gp)(7), sp.layout = list(lyt) ))
 	
 	
 	# 7.4.5 Second-Order Properties
 	Kenvjap = envelope(as(spjpines1, 'ppp'), fun = Kest, r = r, nrank = 3, nsim = 99)
 	Kenvred = envelope(as(spred, 'ppp'), fun = Kest, r = r, nrank = 3, nsim = 99)
 	Kenvcells = envelope(as(spcells, 'ppp'), fun = Kest, r = r, nrank = 3, nsim = 99)
 	Kresults = rbind(Kenvjap, Kenvred, Kenvcells)
 	Kresults = cbind( Kresults, y = rep(c('Japanese', 'Redwood', 'Cells'), 		
 					 each = length(r)))
 	
	print(xyplot( (obs-theo) ~ r|y , data = Kresults, type = "l", 
		 		 ylim = c(-0.06, 0.06), ylab = expression(hat(K) (r) - pi * r^2), 
		 		 panel = function(x, y, subscripts) {
  						Ktheo= Kresults$theo[subscripts]
  						lpolygon( c(r, rev(r)), 
  						 		 c( Kresults$lo[subscripts]-Ktheo, 
  							 		 rev(Kresults$hi[subscripts]-Ktheo) ),
  			 	 		 		 border = "darkgrey", col = "lightgrey" )
  						llines( r, Kresults$obs[subscripts]-Ktheo, lty = 2, 
  								lwd = 1.5, col = "black" )
		 		 } ))
	# width of envelope is proportional to variablity under the null hypothesis 
	# of CSR; Japanese cedars appear to be CSR, and Redwoods clustered



# 7.5 Some Applications in Spatial Epidemiology
	# 7.5.1 Case-Control Studies
		# 7.5.1.1 Spatial Variation of the Relative Risk
		bwasthma = 0.06
		pppasthma = as(spasthma, 'ppp')
		pppasthma$window = as(spbdry, 'owin')
		marks(pppasthma) = relevel(pppasthma$marks$Asthma, 'control')
		
		# Compute intensity, and relative risk using density()
		cases = unmark(subset(pppasthma, marks(pppasthma) == 'case'))
		ncases = npoints(cases)
		controls = unmark(subset(pppasthma, marks(pppasthma) == 'control'))
		ncontrols = npoints(controls)
		kcases = density(cases, bwasthma)
		kcontrols = density(controls, bwasthma)
		
		spkratio0 = as(kcases, 'SpatialGridDataFrame')
		names(spkratio0) = 'kcases'
		spkratio0$kcontrols = as(kcontrols, 'SpatialGridDataFrame')$v
		spkratio = as(spkratio0, 'SpatialPixelsDataFrame')
		spkratio$kratio = spkratio$kcases / spkratio$kcontrols
		spkratio$logratio = log(spkratio$kratio) - log(ncases / ncontrols)
		
		# Monte Carlo method to see if distribution sig diff from null: that 
		# cases are equally likely at all locations (where there are people)
		# Logic: if null, cases and controls should yield the same risk 
		# function, randomly replace/swap labels and reasses risk functions 
		# repeatedly
		niter = 99
		ratio = rep(NA, niter)
		pvaluemap = rep(0, nrow(spkratio))
		rlabelratio = matrix(NA, nrow = niter, ncol = nrow(spkratio))
		
		for (i in 1:niter) {
			pppasthma0 = rlabel(pppasthma)
			casesrel = unmark(subset(pppasthma0, marks(pppasthma0) == 'case'))
			controlsrel = unmark(subset( pppasthma0, marks(pppasthma0) == 
										 'control' ))
			kcasesrel = density(casesrel, bwasthma)
			kcontrolsrel = density(controlsrel, bwasthma)
			kratiorel = eval.im(kcasesrel / kcontrolsrel)
			rlabelratio[i, ] = as(as(kratiorel, 'SpatialGridDataFrame'), 
								 'SpatialPixelsDataFrame')$v
			pvaluemap = pvaluemap + (spkratio$kratio < rlabelratio[i, ])
		}
		
		cellsize = kcontrols$xstep * kcontrols$ystep
		ratiorho = cellsize * sum((spkratio$kratio - ncases / ncontrols)^2)
		ratio = cellsize * apply(rlabelratio, 1, 
				 function(X, rho0 ){
					sum((X-rho0)^2)
				 }, rho0 = ncases/ncontrols )
		pvaluerho = (sum(ratio > ratiorho) + 1) / (niter + 1)
	
		spkratio$pvaluemap = (pvaluemap + 1) / (niter + 1)
		imgpvalue = as.image.SpatialGridDataFrame(spkratio["pvaluemap"])
		clpvalue = contourLines(imgpvalue, levels = c(0,.05, .95, 1))
		cl = ContourLines2SLDF(clpvalue)
		
		cl05 = cl[cl$level == "0.05",]
		xzx = slot(slot(cl05, "lines")[[1]], "Lines")
		cl05a = SpatialLines(list(Lines(xzx, ID = "0.05")))
		lyt05 = list("sp.lines", cl05a, lwd = 2, lty = 2, col = "grey95")
		lyt95 = list("sp.lines", cl[cl$level == "0.95",], lwd = 2, lty = 1)
		lytb = list("sp.polygons", spbdry)
		lytp = list( "sp.points", spsrc, cex = 0.9, pch = 4, col = "grey95",
					  lwd = 3 )
		brks = quantile( spkratio$kratio[spkratio$kratio>0], seq(0,1,1/10), 
						 na.rm = TRUE )
		brks[1] = 0
		lbrks = formatC(brks, 3, 6, "g", " ")
		cols = colorRampPalette(brewer.pal(7, "Reds"))(length(brks)-1)
		colorkey=list(labels = lbrks, at = (0:10)/10, height = .5)

		print(spplot( spkratio, "kratio", col.regions = cols, do.log = TRUE, 
					 colorkey = colorkey, 
					 at = c( 0, brks[-c(1,11)], 
					 		 max(spkratio$kratio, na.rm = TRUE) ),
 					 sp.layout = list(lyt05, lyt95, lytb, lytp) ))
 					 
	# 7.5.2 Binary Regression Estimator
 	rrbw = bw.relrisk(pppasthma, hmax = 0.5)	# 0.2088 optimal bandwidth fr 
 												# this criterion
 	bwasthmap = 0.2088
 	#rr = relrisk(pppasthma, bwasthmap)	 # likely oversmoothed: so,
 	bwasthmap = 0.0.06
 	rr = relrisk(pppasthma, bwasthmap)	
 	
 	spkratio$prob = as( as(rr, 'SpatialGridDataFrame'), 
 						 'SpatialPixelsDataFrame' )$v
 	plot(spkratio$prob, pch = 16, col = rgb(0, 0, 0, 0.25))
 	
 	ats = seq(0,max(spkratio$prob),length.out = 11)
	cols = colorRampPalette(brewer.pal(8, "Reds"))(length(ats)-1)
	print( spplot(spkratio, "prob", col.regions = cols, at = ats, 
		   sp.layout = list(lytb, lytp)) )
		   
	
	
	# 7.5.3 Binary Regression Using Generalized Additive Models
	spasthma$y = as.integer(!as.integer(spasthma$Asthma) - 	1)
	ccasthma = coordinates(spasthma)
	spasthma$x1 = ccasthma[, 1]
	spasthma$x2 = ccasthma[, 2]
	spasthma$dist1 = sqrt(spasthma$d2source1)
	spasthma$dist2 = sqrt(spasthma$d2source2)
	spasthma$dist3 = sqrt(spasthma$d2source3)
	spasthma$droads = sqrt(spasthma$roaddist2)
	spasthma$smoking = as.factor(as.numeric(spasthma$Nsmokers > 0))
	spasthma$Genderf = as.factor(spasthma$Gender)
	spasthma$HayFeverf = as.factor(spasthma$HayFever)
	
	# library(mgcv)
	gasthma = gam( y ~ 1 + dist1 + dist2 + dist3 + droads + Genderf + Age +
				   HayFeverf + smoking + s(x1, x2), 
				   data = spasthma[ spasthma$Gender == 1 | 
									spasthma$Gender == 2, ], 
				   family = binomial )
	summary(gasthma)
	# note: p val of smoothed term (s(x1, x2)) signif., indicating resid variation unexplained by the glm
	
	
	# 7.5.4 Point Source Pollution
	D2_mat = as.matrix(spasthma$dist2)
	RHO = ncases / ncontrols
	expsource2 = tribble( ccflag = spasthma$y, vars = D2_mat, rho = RHO, 
						  alphas = 1, betas = 1 )
	print(expsource2)
	
	hay_mat = as.matrix(spasthma$HayFever)
	exphay = tribble( ccflag = spasthma$y, covars = hay_mat, rho = RHO, 
					  thetas = 1 )
	print(exphay)
	
	# Hay fever accounts for more variation than dist2, but consider both simultaneously
	expsource2hay = tribble( ccflag = spasthma$y, vars = D2_mat, rho = RHO,
							 covars = hay_mat, alphas = 1, betas = 1, 
							 thetas = 1 )
	print(expsource2hay)

		# 7.5.4.1 Assessment of General Spatial Clustering
		Kdif = function(Xppp, r, cr = 'border') {
			k1 = Kest(Xppp[marks(Xppp) == 'case'], r = r, correction = cr)
			k2 = Kest(Xppp[marks(Xppp) == 'control'], r = r, correction = cr)
			res = data.frame(r = r, D = k1[[cr]] - k2[[cr]])
			return (fv(res, valu = 'D', fname = 'D'))
		}
		
		r = seq(0, 0.15, 0.01)
		envKdif = spatstat::envelope( pppasthma, Kdif, r = r, nsim = 99, 
									  cr = 'iso', nrank = 2, savefuns = T, 
									  simulate = expression(rlabel(pppasthma)) )
		khcases = Kest(cases, r = r, correction = 'isotropic')
		khcontrols = Kest(controls, r = r, correction = 'isotropic')
		
		simfuns = as.data.frame(attr(envKdif, 'simfuns'))[, -1]
		khcovdiag = apply(simfuns, 1, var)
		T0 = sum(((khcases$iso - khcontrols$iso) / sqrt(khcovdiag))[-1])
		Tt = apply(simfuns, 2, function(X) {
			sum((X / sqrt(khcovdiag))[-1])
		})
		(pvalue = (sum(Tt > T0) + 1) / (niter + 1))	# 0.33
		# No sig. diff. between distribution of cases and controls
		
		plot(envKdif)
		lines(r, -1.96 * sqrt(khcovdiag), lty = 2, col = 4)
		lines(r, 1.96 * sqrt(khcovdiag), lty = 2, col = 4)



	# 7.5.5 Accounting for Confounding and Covariates
	glasthma = glm(y ~ HayFeverf, data = spasthma, family = 'binomial')
	prob = fitted(glasthma)
	weights = exp(glasthma$linear.predictors)
	lambda0 = interp.im(kcontrols, coords(cases)[, 1], coords(cases)[ ,2])
	lambda1 = weights[marks(pppasthma) == 'case'] * lambda0
	ratiocc = ncases / ncontrols
	kihnocov = Kinhom(cases, ratiocc * lambda0, r = r)
	kih = Kinhom(cases, lambda1, r = r)
	
	rlabelp = function(Xppp, ncases, prob) {
		idxsel = sample(1:npoints(Xppp), ncases, prob = prob)
		marks(Xppp) = 'control'
		marks(Xppp)[idxsel] = 'case'
		return (Xppp)
	}
	
	KIlambda = function(Xppp, r, cr = 'iso', w, sigma) {
		idxrel = marks(Xppp) == 'case'
		casesrel = unmark(Xppp[idxrel])
		controlsrel = unmark(Xppp[!idxrel])
		lambda0rel = interp.im( density(controlsrel, sigma), 
								coords(casesrel)[, 1], 
								coords(casesrel)[, 2] )
		lambda1rel = w[idxrel] * lambda0rel
		KI = Kinhom(casesrel, lambda1rel, r = r, correction = cr)
		res = data.frame(r = r, KI = KI[[cr]])
		return (fv(res, valu = 'KI', fname = 'K_[I, lambda]'))
	}
	
	envKInocov = envelope( pppasthma, KIlambda, r = r, w = weights, 
						   sigma = bwasthma, nsim = 99, nrank = 2, savefuns = T, 
						   simulate = expression(rlabelp( 
						   		pppasthma, ncases = ncases, 
						   		prob = rep( ratiocc, npoints(pppasthma)) 
						   )) )
						   
	envKIcov = envelope( pppasthma, KIlambda, r = r, w = weights, 
						 sigma = bwasthma, nsim = 99, nrank = 2, savefuns = T,
						 simulate = expression(rlabelp( pppasthma, 
						 								ncases=ncases, 
						 								prob=prob )) ) 
						 						
	kinhomrelnocov = as.data.frame(attr(envKInocov, 'simfuns'))[, -1]
	kinhomrel = as.data.frame(attr(envKIcov, 'simfuns'))[, -1]
	
	kinhsdnocov = apply(kinhomrelnocov, 1, sd)
	D0nocov = sum(((envKInocov$obs - envKInocov$mmean) / kinhsdnocov)[-1])
	Dnocov = apply(kinhomrelnocov, 2, function(X) { 
					sum(((X - envKInocov$mmean) / kinhsdnocov)[-1])
			 })
	pvaluenocov = (sum(Dnocov > D0nocov) + 1) / (niter + 1)
	
	kinhsd = apply(kinhomrel, 1, sd)
	D0 = sum(((envKIcov$obs - envKIcov$mmean) / kinhsd)[-1])
	Dx = apply(kinhomrel, 2, function(X) { 
			sum(((X - envKIcov$mmean) / kinhsd)[-1])
		 })
	pvalue = (sum(Dx > D0) + 1) / (niter + 1)
	
	par(mfrow = c(2, 1))
	plot( r, envKInocov$obs - envKInocov$mmean, type = 'l', 
		  ylim = c(-0.06, 0.06), xlab = 's', 
		  ylab = expression(hat(k)[I][','][hat(lambda)] - 'E[s]'), 
		  main = 'No covariates' )
	lines(r, envKInocov$lo - envKInocov$mmean, col = 2)
	lines(r, envKInocov$hi - envKInocov$mmean, co = 2)
	
	plot( r, envKIcov$obs - envKIcov$mmean, type='l', ylim = c(-0.06,  0.06), 
		  xlab='s', ylab = expression(hat(k)[I][','][hat(lambda)] - 'E[s]'),
		  main = 'Adjusting for Hay Fever' )
	lines(r, envKIcov$lo - envKIcov$mmean, col = 2)
	lines(r, envKIcov$hi - envKIcov$mmean, col = 2)
	






save.image('~/Desktop/R/Spatial/SpatialAnalysisBook/SpatialAnalysis.RData')
