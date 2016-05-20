#===========================================================================#
#                   														#
# Examples and Exercises from Bivand et al. "Applied Spatial Data Analysis 	#
#  with R"                													#
# Chapter 8: Interpolation and Geostatistics		<geos_mod.r>			#
# Damian Satterthwaite-Phillips <damiansp@gmail.com>      					#
# Updated: 07Jul2015             											#
#                   														#
#===========================================================================#

rm(list = ls())
load('~/Desktop/R/Spatial/SpatialAnalysisBook/SpatialAnalysis.RData')

library(gstat)
library(lattice)
library(RColorBrewer)
library(sp)

# 8.2 Exploratory Data Analysis
data(meuse)
coordinates(meuse) = c('x', 'y')
spplot(meuse, 'zinc', do.log = T, colorkey = T)
bubble(meuse, 'zinc', key.space = 'bottom')

xyplot(log(zinc) ~ sqrt(dist), as.data.frame(meuse))
zn.lm = lm(log(zinc) ~ sqrt(dist), meuse)
summary(zn.lm)
meuse$fitted.s = predict(zn.lm, meuse) - mean(predict(zn.lm, meuse))
meuse$residuals = resid(zn.lm)
spplot(meuse, c('fitted.s', 'residuals'))



# 8.3 Non-Geospatial Interpolation Methods
data(meuse.grid)
coordinates(meuse.grid) = c('x', 'y')
meuse.grid = as(meuse.grid, 'SpatialPixelsDataFrame')

	# 8.3.1 Inverse Distance Weighted Interpolation
	# IDW computes a weighted avg:
	#	Z.hat(s[0]) = sum(w(s) * Z(s)) / sum(w(s)) 
	# for vector s, where weights are computed according to their distance 
	# from the interpolation location:
	#	w(s[i]) = || s[i] - s[0] ||^-p
	# where || • || is the Euclidean dist and p the inverse distance weighting 
	# power (default: 2)
	# library(gstat)
	idw.out = idw(zinc ~ 1, meuse, meuse.grid, idp = 2.5)
	head(as.data.frame(idw.out))	# var1.pred are the interpolated values;
									# var1.var are prediction error variances
									# (not provided for inverse dist)
	spplot(meuse, 'zinc')
	quartz()
	spplot(idw.out, 'var1.pred')
	
	
	# 8.3.2 Linear Regression
	zn.lm = lm(log(zinc) ~ sqrt(dist), meuse)
	meuse.grid$pred = predict(zn.lm, meuse.grid)
	meuse.grid$se.fit = predict(zn.lm, meuse.grid, se.fit = T)$se.fit
	pal = function(n = 9) { brewer.pal(n, 'Reds') }
	print(spplot( meuse, c("fitted.s", "residuals"), 
				  col.regions = pal(), cuts = 8, colorkey = T ))
	spplot(meuse.grid, 'pred')
	
	meuse.lm = krige(log(zinc) ~ sqrt(dist), meuse, meuse.grid)
	# in this case, w/o variogram in input, defaults to lm (as previous)
	
	meuse.tr2 = krige(log(zinc) ~ 1, meuse, meuse.grid, degree = 2)
	# polynomials (here 2nd degree) of spatial cooords used as predictors
	# AKA "trend surface analysis" (highly sensitive to outliers)
	spplot(meuse.tr2, 'var1.pred')
	
	meuse.tr3 = krige(log(zinc) ~ 1, meuse, meuse.grid, degree = 3)
	# polynomials (here 3rd degree) 
	spplot(meuse.tr3, 'var1.pred')

	# As a lm, the 2nd degree trend surface analysis is the same as:
	lm(log(zinc) ~ poly(x, y, degree = 2), meuse)	# which is the same as:
	lm(log(zinc) ~ I(x^2) + I(y^2) + I(x * y) + x + y, meuse)
	
	
	
# 8.4 Estimating Spatial Correlation: the Variogram
	# 8.4.1 Exploratory Variogram Analysis
	# Is there spatial correlation?  Visualize point values by distance
	hscat(log(zinc) ~ 1, meuse, (0:9) * 100, col = 'darkgrey')
	cld = variogram(log(zinc) ~ 1, meuse, cloud = T)
	svgm = variogram(log(zinc) ~ 1, meuse)
	d = data.frame( gamma = c(cld$gamma, svgm$gamma), 
					dist = c(cld$dist, svgm$dist), 
					id = c( rep('cloud', nrow(cld)), 
							rep('sample variogram', nrow(svgm)) ))
	xyplot( gamma ~ dist | id, d, 
			scales = list(y = list( relation = 'free', 
									limits = list(NULL, c(-0.005, 0.7)) )),
			layout = c(1, 2), as.table = T,
			panel = function(x, y, ...) {
				if (panel.number() == 2) {
					ltext(x + 10, y, svgm$np, adj = c(0, 0.5))
				}
				panel.xyplot(x, y, ...)
			},
			xlim = c(0, 1590), cex = 0.5, pch = 3 )
	
	sel = plot(variogram(zinc ~ 1, meuse, cloud = T), digitize = T) 
	# SELECT AREA
	plot(sel, meuse)	# shows point pairs selected in polygon
	
	v = variogram(log(zinc) ~ 1, meuse)
	plot(v, type = 'b', pch = 16)
	
	fn = function(n = 1000) {
		for (i in 1:n) {
			meuse$random = sample(meuse$zinc)
			v = variogram(log(random) ~ 1, meuse)
			trellis.focus('panel', 1, 1, highlight = F)
			llines(v$dist, v$gamma, col = rgb(0, 0, 0, 0.1))
			trellis.unfocus
		}
	}
	
	fn()
	# in the simulated (random) variogram, semivariance (gamma) is a constant
	# with respect to distance: i.e., no spatial correlation
	
	








save.image('~/Desktop/R/Spatial/SpatialAnalysisBook/SpatialAnalysis.RData')
