#===========================================================================#
#                   														#
# Examples and Exercises from Bivand et al. 'Applied Spatial Data Analysis 	#
#  with R'                													#
# Chapter 9: Modelling Areal Data			<lat_mod.r>						#
# Damian Satterthwaite-Phillips <damiansp@gmail.com>      					#
# Updated: 07Jul2015             											#
#                   														#
#===========================================================================#
rm(list = ls())
load('~/Desktop/R/Spatial/SpatialAnalysisBook/SpatialAnalysis.RData')


library(boot)
library(lmtest)
library(McSpatial)
library(pgirmess)
library(RColorBrewer)
library(rgdal)
library(sandwich)
library(spdep)


# 9.2 Spatial Neighbors and Spatial Weights
	# 9.2.1 Neighbor Objects
	NY8 = readOGR('./Desktop/R/Spatial/SpatialAnalysisBook/NY_data', 
				  'NY8_utm18' )
	TCE = readOGR('./Desktop/R/Spatial/SpatialAnalysisBook/NY_data', 'TCE')
	NY_nb = 
		read.gal('./Desktop/R/Spatial/SpatialAnalysisBook/NY_data/NY_nb.gal', 				 region.id = row.names(NY8) )
	cities = readOGR('./Desktop/R/Spatial/SpatialAnalysisBook/NY_data', 
					 'NY8cities' )

	plot(NY8, border = 'grey60', axes=TRUE)
	text(coordinates(cities), labels = as.character(cities$names), font = 2, 
		 cex=0.9 )
	text(bbox(NY8)[1,1], bbox(NY8)[2,2], labels = 'a)', cex = 0.8)
	plot(NY8, border = 'grey60', axes = TRUE)
	points(TCE, pch = 1, cex = 0.7)
	points(TCE, pch = 3, cex = 0.7)
	text(coordinates(TCE), labels = as.character(TCE$name), cex = 0.7, 
		 font = 1, pos = c(4, 1, 4, 1, 4, 4, 4, 2, 3, 4, 2), offset = 0.3 )
	text(bbox(NY8)[1,1], bbox(NY8)[2,2], labels = 'b)', cex = 0.8)

	summary(NY_nb)

	plot(NY8, border = 'grey60', axes = TRUE)
	plot(NY_nb, coordinates(NY8), pch = 19, cex = 0.6, add = T)
	
	Syracuse = NY8[NY8$AREANAME == 'Syracuse city',]
	Sy0_nb = subset(NY_nb, NY8$AREANAME == 'Syracuse city')
	summary(Sy0_nb)
	
	coords = coordinates(Syracuse)
	IDs = row.names(Syracuse)
	Sy8_nb = knn2nb(knearneigh(coords, k = 1), row.names = IDs)
	Sy9_nb = knn2nb(knearneigh(coords, k = 2), row.names = IDs)
	Sy10_nb = knn2nb(knearneigh(coords, k = 4), row.names = IDs)
	dsts = unlist(nbdists(Sy8_nb, coords))
	Sy11_nb = dnearneigh(coords, d1 = 0, d2 = 0.75 * max(dsts), 
						 row.names = IDs )

	# 9.2.2 Spatial Weights Objects
	Sy0_lw_W = nb2listw(Sy0_nb)
	Sy0_lw_W
	names(Sy0_lw_W)
	names(attributes(Sy0_lw_W))
	
	1 / rev(range(card(Sy0_lw_W$neighbours)))
	summary(unlist(Sy0_lw_W$weights))
	summary(sapply(Sy0_lw_W$weights, sum))	
	# e.g., all weights from a node sum to 1, and by default, that weight is
	# distributed evenly across links to all neighbors
	
	Sy0_lw_B = nb2listw(Sy0_nb, style='B')
	summary(unlist(Sy0_lw_B$weights))
	summary(sapply(Sy0_lw_B$weights, sum))
	# e.g., all links to neighbors given a value of 1, so sum = no. neighbors
	
	dsts = nbdists(Sy0_nb, coordinates(Syracuse))
	# inverse distance weighting
	idw = lapply(dsts, function(x) 1 / (x / 1000)) 
	# use glist to pass in weights
	Sy0_lw_idwB = nb2listw(Sy0_nb, glist = idw, style = 'B')
	summary(unlist(Sy0_lw_idwB$weights))
	summary(sapply(Sy0_lw_idwB$weights, sum))
	
	# Intentional Error:
	Sy0_lw_D1 = nb2listw(Sy11_nb, style = 'B')

	Sy0_lw_D1 = nb2listw(Sy11_nb, style='B', zero.policy = T)
	print(Sy0_lw_D1, zero.policy = T)

	
	
	# 9.2.3 Handling Spatial Weights Objects
	# Sy14_nb = read.gal('Sy_GeoDa1.GAL')
	# isTRUE(all.equal(Sy0_nb, Sy14_nb, check.attributes=FALSE))
	# Cannot find file, but since the same:
	Sy14_nb = Sy0_nb
	
	# 9.2.4 Using Weights to Simulate Spatial Autocorrelation
	n = length(Sy0_nb)
	uncorr_x = rnorm(n)
	rho = 0.5
	autocorr_x = invIrW(Sy0_lw_W, rho) %*% uncorr_x
	
	oopar = par(mfrow = c(1, 2), mar = c(4, 4, 3, 2) + 0.1)
	plot(uncorr_x, lag(Sy0_lw_W, uncorr_x), xlab = '', cex.lab = 0.8,
 		 ylab = 'spatial lag', main = 'Uncorrelated random variable', 
 		 cex.main = 0.8)
	lines(lowess(uncorr_x, lag(Sy0_lw_W, uncorr_x)), col = 2, lwd = 2)

	plot(autocorr_x, lag(Sy0_lw_W, autocorr_x), xlab = '', ylab = '',
		 main = 'Autocorrelated random variable', cex.main = 0.8,
		 cex.lab=0.8)
	lines(lowess(autocorr_x, lag(Sy0_lw_W, autocorr_x)), col = 2, lwd = 2)
	par(oopar)
	
	
	
# 9.3 Testing for Spatial Autocorrelation
moran.test(uncorr_x, listw = Sy0_lw_W)
moran.test(autocorr_x, listw = Sy0_lw_W)
moran.test(autocorr_x, listw = nb2listw(Sy9_nb, style = 'W'))

et = coords[,1] - min(coords[,1])
trend_x = uncorr_x + 0.00025 * et
moran.test(trend_x, listw = Sy0_lw_W)
lm.morantest(lm(trend_x ~ et), listw = Sy0_lw_W)

	# 9.3.1 Global Tests
	moran.test(NY8$Cases, listw = nb2listw(NY_nb))
	
	lw_B = nb2listw(NY_nb, style = 'B')
	moran.test(NY8$Cases, listw = lw_B)
	
	moran.test(NY8$Cases, listw = lw_B, randomisation = F)
	
	# NOTE: the previous is the same as:
	lm.morantest(lm(Cases ~ 1, NY8), listw = lw_B)
	
	lm.morantest.sad(lm(Cases ~ 1, NY8), listw = lw_B)
	lm.morantest.exact(lm(Cases ~ 1, NY8), listw = lw_B)
	
	# and a Monte Carlo method:
	bperm = moran.mc(NY8$Cases, listw = lw_B, nsim = 1000)
	bperm
	
	r = sum(NY8$Cases) / sum(NY8$POP8)
	rni = r * NY8$POP8
	CR = function(var, mle) {
		rpois(length(var), lambda = mle)
	}

	MoranI.pboot = function(var, i, listw, n, S0, ...) {
  		return(moran(x = var, listw = listw, n = n, S0 = S0)$I)
	}
	
	boot2 = boot(NY8$Cases, statistic = MoranI.pboot, R = 1000, 
				 sim='parametric', ran.gen = CR, listw = lw_B, 
				 n = length(NY8$Cases), S0 = Szero(lw_B), mle = rni)
	pnorm((boot2$t0 - mean(boot2$t)) / sd(boot2$t[,1]), lower.tail = F)
	
	# Show simulations of Moran's I with random permutations of the data...
	oopar = par(mfrow = c(1, 2))
	xlim = range(c(bperm$res, boot2$t[, 1]))
	hist(bperm$res[-length(bperm$res)], main='Permutation bootstrap', 	
		 xlab = expression(I[std]), xlim = xlim, col = 'cyan', 	
		 ylim = c(0,260))
	# Plot the observed value
	abline(v = bperm$statistic, lty = 2)
	
	# ...and parametric samples from constant risk (CR) expected vals
	hist(boot2$t, col = 4, main = 'Parametric bootstrap', 	
		 xlab = expression(I[CR]), xlim = xlim, ylim = c(0, 260))
	hist(bperm$res[-length(bperm$res)], col = rgb(0, 1, 1, 0.8), add = T)
	abline(v = boot2$t0, lty = 2)
	par(oopar)

	rni = fitted(glm(Cases ~ 1 + offset(log(POP8)), data = NY8, 
				     family = 'poisson'))
	
	EBImoran.mc(n = NY8$Cases, x = NY8$POP8, 
				listw = nb2listw(NY_nb, style = 'B'), nsim = 1000)
	
	cor8 = sp.correlogram(neighbours = NY_nb, var = NY8$Cases, order = 8, 
						  method = 'I', style = 'C')

	corD = correlog(coordinates(NY8), NY8$Cases, method = 'Moran')
	
	oopar = par(mfrow = c(1, 2))
	plot(cor8, main = 'Contiguity lag orders')
	plot(corD, main='Distance bands')
	par(oopar)
	
	
	
	# 9.3.2 Local Tests
	msp = moran.plot(NY8$Cases, listw = nb2listw(NY_nb, style='C'), 
					 quiet = T)
	title('Moran scatterplot')
	infl = apply(msp$is.inf, 1, any)
	x = NY8$Cases
	lhx = cut(x, breaks = c(min(x), mean(x), max(x)), labels=c('L', 'H'), 
			  include.lowest = T)
	wx = lag(nb2listw(NY_nb, style = 'C'), NY8$Cases)
	lhwx = cut(wx, breaks = c(min(wx), mean(wx), max(wx)), 
			   labels = c('L', 'H'), include.lowest = T)
	lhlh = interaction(lhx, lhwx, infl, drop = T)
	cols = rep(1, length(lhlh))
	cols[lhlh == 'H.L.TRUE'] = 2
	cols[lhlh == 'L.H.TRUE'] = 3
	cols[lhlh == 'H.H.TRUE'] = 4
	plot(NY8, col = brewer.pal(4, 'Accent')[cols])
	legend('topright', legend = c('None', 'HL', 'LH', 'HH'), 
		   fill = brewer.pal(4, 'Accent'), bty = 'n', cex = 0.8,
		   y.intersp = 0.8)
	title('Tracts with influence')
	par(oopar)
	
	lm1 = localmoran(NY8$Cases, listw = nb2listw(NY_nb, style = 'C'))
	lm2 = as.data.frame(localmoran.sad(lm(Cases ~ 1, NY8), nb = NY_nb, 
									   style = 'C'))
	lm3 = as.data.frame(localmoran.exact(lm(Cases ~ 1, NY8), nb = NY_nb, 
										 style = 'C'))

	r = sum(NY8$Cases) / sum(NY8$POP8)
	rni = r * NY8$POP8
	lw = nb2listw(NY_nb, style = 'C')
	sdCR = (NY8$Cases - rni) / sqrt(rni)
	wsdCR = lag(lw, sdCR)
	I_CR = sdCR * wsdCR
	
	gry = c(rev(brewer.pal(8, 'Reds')[1:6]), brewer.pal(6, 'Blues'))
	NY8$Standard = lm1[,1]
	NY8$'Constant_risk' = I_CR
	#nms = match(c('Standard', 'Constant_risk'), names(NY8))
	spplot(NY8, c('Standard', 'Constant_risk'), 
		   at = c(-2.5, -1.4, -0.6, -0.2, 0, 0.2, 0.6, 4, 7), 
		   col.regions = colorRampPalette(gry)(8))
		   
	nsim = 999
	N = length(rni)
	sims = matrix(0, ncol = nsim, nrow = N)
	for (i in 1:nsim) {
	    y = rpois(N, lambda = rni)
	    sdCRi = (y - rni) / sqrt(rni)
	    wsdCRi = lag(lw, sdCRi)
	    sims[,i] = sdCRi * wsdCRi 
	}
	xrank = apply(cbind(I_CR, sims), 1, function(x) rank(x)[1])
	diff = nsim - xrank
	diff = ifelse(diff > 0, diff, 0)
	pval = punif((diff + 1) / (nsim + 1))
	
	NY8$Normal = lm2[,3]
	NY8$Randomisation = lm1[,5]
	NY8$Saddlepoint = lm2[,5]
	NY8$Exact = lm3[,5]
	NY8$Constant_risk = pval
	gry = c(rev(brewer.pal(6, 'Reds')), brewer.pal(6, 'Blues'))
	spplot(NY8, c('Normal', 'Randomisation', 'Saddlepoint', 'Exact', 
				  'Constant_risk'), 
		   at = c(0, 0.01, 0.05, 0.1, 0.9, 0.95, 0.99, 1), 
		   col.regions = colorRampPalette(gry)(7))

	spplot(NY8, c('Normal', 'Exact', 'Constant_risk'), 
		   xlim = c(405200, 432200), ylim = c(4652700, 4672000), 
		   at = c(0, 0.01, 0.05, 0.1, 0.9, 0.95, 0.99, 1), 
		   col.regions = colorRampPalette(gry)(7))



# 9.4 Fitting Models of Areal Data
	# 9.4.1 Spatial Statistics Approaches
	nylm = lm(Z ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME, data = NY8)
	summary(nylm)
	
	NY8$lmresid = resid(nylm)
	
	NYlistw = nb2listw(NY_nb, style = 'B')
	lm.morantest(nylm, NYlistw)
	
	NYlistwW = nb2listw(NY_nb, style = 'W')
	aple(resid(nylm), listw = NYlistwW)
	nyspautolm = spautolm(Z ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME, data = NY8, 
			 			  listw = NYlistwW)
	summary(nyspautolm)
	
	
		# 9.4.1.1 Simultaneous Autoregressive Models
		nysar = spautolm(Z ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME, data = NY8, 
			 			 listw = NYlistw)
		summary(nysar)
		# lambda of 0.04 with p of 0.02 suggest sp autocor in resids
		
		nylam1 = c(nysar$lambda)
		nylam2 = c(LR1.spautolm(nysar)$p.value)

		NY8$sar_trend = nysar$fit$signal_trend
		NY8$sar_stochastic = nysar$fit$signal_stochastic
		rds = colorRampPalette(brewer.pal(8, 'RdBu'))
		tr_at = seq(-1, 1.3, length.out = 21)
		tr_rds = rds(sum(tr_at >= 0) * 2)[
			-(1:(sum(tr_at >= 0) - sum(tr_at < 0)))
		]
		tr_pl = spplot(NY8, c('sar_trend'), at = tr_at, col = 'transparent', 
					   col.regions = tr_rds, 
					   main = list(label = 'Trend', cex = 0.8))
		st_at = seq(-0.16, 0.39, length.out = 21)
		st_rds = rds(sum(st_at >= 0)*2)[
			-(1:(sum(st_at >= 0) - sum(st_at < 0)))
		]
		st_pl = spplot(NY8, c('sar_stochastic'), at = st_at, 
					   col='transparent', col.regions = st_rds, 
					   main = list(label='Stochastic', cex = 0.8))
		plot(tr_pl, split = c(1, 1, 2, 1), more = T)
		plot(st_pl, split = c(2, 1, 2, 1), more = F)

		
		nylmw = lm(Z ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME, data = NY8, 
			 	   weights = POP8)
		summary(nylmw)
		
		gry = c(rev(brewer.pal(6, 'Reds')[1:4]),
			 	colorRampPalette(brewer.pal(5, 'Blues'))(9))
		TCEpts = list('sp.points', TCE, pch = 16, col = 'grey5')
		spplot(NY8, c('lmresid', 'lmwresid'), sp.layout = list(TCEpts), 
			   col.regions = gry, col = 'transparent', lwd = 0.5,
			   at = seq(-2, 4.5, 0.5))

		
		NY8$lmwresid = resid(nylmw)
		lm.morantest(nylmw, NYlistw)
		
		nysarw = spautolm(Z ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME, data = NY8, 
			 			  listw = NYlistw, weights = POP8)
		summary(nysarw)
		
		NY8$sarw_trend = nysarw$fit$signal_trend
		NY8$sarw_stochastic = nysarw$fit$signal_stochastic
		tr_pl = spplot(NY8, c('sarw_trend'), at = tr_at, col = 'transparent', 
					   col.regions = tr_rds, 
					   main = list(label = 'Trend', cex = 0.8))
		st_pl = spplot(NY8, c('sarw_stochastic'), at = st_at, 
					   col = 'transparent', col.regions = st_rds, 
					   main = list(label = 'Stochastic', cex = 0.8))
		plot(tr_pl, split=c(1, 1, 2, 1), more = T)
		plot(st_pl, split=c(2, 1, 2, 1), more = F)

	
		# 9.4 1.2 Conditional Autoregressive Models
		nycar = spautolm(Z ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME, data = NY8,
						 listw = NYlistw, family = 'CAR')
		summary(nycar)
		# Still signif autocorrelation (p[lambda] = 0.016)
		nycarw = spautolm(Z ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME, data = NY8,
						  listw = NYlistw, family = 'CAR', weights = POP8)
		summary(nycarw)
		# NOTE: best model yet by AIC
		
		
		# 9.4.1.3 Fitting Spatial Regression Models
		nysarwM = spautolm(Z ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME, 
						   data = NY8, listw = NYlistw, family = 'SAR',
						   weights = POP8, method = 'Matrix')
		summary(nysarwM)
		
		1 / range(eigenw(NYlistw)) # -0.303, 0.155
		nysar_ll = spautolm(Z ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME, 
							data = NY8, listw = NYlistw, family = 'SAR',
							llprof = 100)
		nysarw_ll = spautolm(Z ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME, 
							 data = NY8, listw = NYlistw, family = 'SAR',
							 weights = POP8, llprof = 100)

		nysmaw = spautolm(Z ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME, data = NY8, 
						  listw = NYlistw, family = 'SMA', weights = POP8)
		summary(nysmaw)

		
	# 9.4.2 Spatial Econometrics Approaches
	bptest(nylm) # library(lmtest)	
	# indicates signif. heteroskedasticity in mod resids--suggests need to
	# adjust SEs w/var-covar matrix:
	
	coeftest(nylm) # library(sandwich)
	coeftest(nylm, vcov = vcovHC(nylm, type = 'HC4'))	
	# changes are minor, and do not change inferences
	
	NYlistwW = nb2listw(NY_nb, style = 'W')
	# Lagrange Multiplier tests:
	res = lm.LMtests(nylm, listw = NYlistwW, test = 'all')
	tres = t(sapply(res, function(x) c(x$statistic, x$parameter, x$p.value)))
	colnames(tres) = c('Statistic', 'df', 'p-value')
	printCoefmat(tres)
	# LMerr responds to both an omitted spatially lagged dep. var. and to 
	# spatially autocorr. resids; RLMerr is designed specif. for spat. 
	# autocorr. in resids. 
	
	nylag = lagsarlm(Z ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME, data = NY8,
					 listw = NYlistwW)
	summary(nylag)	
	bptest.sarlm(nylag)
	
	# library(McSpatial)
	McRes = sarml(Z ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME, data = NY8,
				  wmat = listw2mat(NYlistwW), eigvar = eigenw(NYlistwW))
	nymix = lagsarlm(Z ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME, data = NY8,
					 listw = NYlistwW, type = 'mixed')
	nymix
	anova(nymix, nylag) # don't siginif differ	


























save.image('~/Desktop/R/Spatial/SpatialAnalysisBook/SpatialAnalysis.RData')
