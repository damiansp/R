#=======================================================#
#                                                       #
#  Examples and Exercises from Bivand et al. 			#
#  "Applied Spatial Data Analysis with R"               #
#  Chapter 5: Further Methods for Handling Spatial Data	#
#  Damian Satterthwaite-Phillips <damiansp@gmail.com>   #
#  Updated: 29Mar2014                                   #
#                                                       #
#=======================================================#

rm(list=ls())
library(maptools)
library(raster)
library(RColorBrewer)
library(rgdal)
library(rgeos)
library(spgrass6)
library(testthat)
load('~/Desktop/R/Spatial/SpatialAnalysisBook/SpatialAnalysis.RData')

# 5.1 Support

# 5.2 Handling and Combining Features
	# 5.2.1 The rgeos Package
	getScale()
	
	test_package('rgeos')
	
	# 5.2.2 Using rgeos
#	setwd('~/Desktop/R/Spatial/SpatialAnalysisBook/cm2_bundle')
	olinda <- readOGR( './Desktop/R/Spatial/SpatialAnalysisBook/cm2_bundle/', 
					   'olinda1' )
	plot(olinda)
	proj4string(olinda) <- CRS('+init=epsg:4674')
	olinda_utm <- spTransform(olinda, CRS('+init=epsg:31985'))
	plot(olinda_utm)
	
	Area <- gArea(olinda_utm, byid=T)
	olinda_utm$area <- sapply(slot(olinda_utm, 'polygons'), slot, 'area')
	all.equal(unname(Area), olinda_utm$area)
	olinda_utm$dens <- olinda_utm$V014 / (olinda_utm$area / 1e+06)
	
	spplot( olinda_utm, "dens", at=c(0, 8000, 12000, 15000, 20000, 60000), 
			col.regions=brewer.pal(6, "YlOrBr")[-1], col="grey30", lwd=0.5, 
			colorkey=list(space="right", labels=list(cex=0.7), width=1))

	bounds <- gUnaryUnion(olinda_utm)
	gArea(olinda_utm)
	sapply(slot(slot(bounds, 'polygons')[[1]], 'Polygons'), slot, 'area')
	
	pols_overlap <- gOverlaps(olinda_utm, byid=T)
	any(pols_overlap)
	
	oScale <- getScale()
	setScale(10000)
	pols_overlap <- gOverlaps(olinda_utm, byid=T)
	any(pols_overlap)
	bounds <- gUnaryUnion(olinda_utm)
	setScale(oScale)
	sapply(slot(slot(bounds, 'polygons')[[1]], 'Polygons'), slot, 'area')
	
	pan <- readGDAL(
			'./Desktop/R/Spatial/SpatialAnalysisBook/cm2_bundle/L7_ETM8s.tif' )
	proj4string(pan) <- CRS(proj4string(bounds))
	TM <- readGDAL(
			'./Desktop/R/Spatial/SpatialAnalysisBook/cm2_bundle/L7_ETMs.tif'
		  )
	proj4string(TM) <- CRS(proj4string(bounds))
	names(TM) <- paste('TM', c(1:5, 7), sep='')
	dem <- readGDAL(
			'./Desktop/R/Spatial/SpatialAnalysisBook/cm2_bundle/olinda_dem_utm25s.tif' )
	proj4string(dem) <- CRS(proj4string(bounds))
	is.na(dem$band1) <- dem$band1 <= 0
	
	myGRASS <- '/Applications/GRASS-6.4.app'
	loc <- initGRASS(myGRASS, tempdir(), SG=dem, override=T)
	execGRASS('g.mapset', mapset='PERMANENT')
	execGRASS('g.proj', flag='c', proj4 = proj4string(bounds))
	
	#...none of this seems to be working...#
	
	
# 5.3 Map Overlay or Spatial Join
#setwd('~/Desktop/R/Spatial/SpatialAnalysisBook/')
mCRS <- proj4string(meuse)
proj4string(meuse.grid) <- mCRS
sel <- over(meuse, as(meuse.grid, 'SpatialPixels'))
meuse <- meuse[!is.na(sel),]
# Same as:
meuse <- meuse[meuse.grid,] # Select those points in meuse that fall w/in 
							# grid cells of meuse.grid

	# 5.3.1 Spatial Aggregation
	gt <- GridTopology(c(178480, 329640), c(400, 400), c(8, 11))
	coarseGrid <- SpatialGrid(gt, proj4string(meuse))
	agg <- aggregate(meuse[c('zinc', 'lead')], coarseGrid, max)
	
	pts <- list("sp.points", meuse, col='black')
	
	pal <- function(n = 9) { brewer.pal(n, "Reds") }
	spplot(agg, sp.layout=pts, col.regions=pal, cuts=8)

	
	TM$ndvi <- (TM$TM4 - TM$TM3) / (TM$TM4 + TM$TM3)
	
	TMO <- as(TM, 'SpatialPixelsDataFrame')
	TM1 <- TMO[bounds,]
	PC <- prcomp(as(TM1, 'data.frame')[, 1:6], center=T, scale.=T)
	PCout <- predict(PC)
	TM1$PC1 <- PCout[, 1]
	TM1$PC2 <- PCout[, 2]
	TM1$PC3 <- PCout[, 3]
	
	spplot( TM1, c("PC1", "PC2"), at=seq(-17, 17, length.out=21), 
			col.regions=rev(colorRampPalette(brewer.pal(10, "PiYG"))(20)), 
			sp.layout=list( "sp.lines", as(olinda_utm, "SpatialLines"), 
							lwd=0.5 ), 
			colorkey=list(space="right", labels=list(cex=0.8), width=1) )

	
	o_mean <- over(olinda_utm, TM1[, c('PC1', 'PC2', 'PC3')])
	str(o_mean)
	row.names(o_mean) <- row.names(olinda_utm)
	olinda_utmA <- spCbind(olinda_utm, o_mean)

	o_median <- over(olinda_utm, TM1[, c('PC1', 'PC2', 'PC3')], fn=median)
	row.names(o_median) <- row.names(olinda_utmA)
	names(o_median) <- paste(names(o_median), 'med', sep='_')
	olinda_utmB <- spCbind(olinda_utmA, o_median)
	TM1$count <- 1
	o_count <- over(olinda_utm, TM1[, 'count'], fn=sum)
	olinda_utmB$count <- o_count$count
	
	summary(olinda_utmB[, grep('^PC|count', names(olinda_utmB))])
	
	o_dem_median <- over(olinda_utm, dem, fn=median)
	olinda_utmB$dem_median <- o_dem_median$band1
	summary(olinda_utmB$dem_median)
	o_ndvi_median <- over(olinda_utm, TM1['ndvi'], fn=median)
	olinda_utmB$ndvi_median <- o_ndvi_median$ndvi
	
	# 5.3.2 Using the raster Package for Extract Operations
	TMrs <- stack(TM1)
	e1 <- extract(TMrs, as(olinda_utm, 'SpatialPolygons'))
	e2 <- extract(raster(dem), as(olinda_utm, 'SpatialPolygons'))
	table(sapply(e2, is.null))
	
	all.equal(sapply(e1, nrow), olinda_utmB$count)
	
	# 5.3.3 Spatial Sampling
	#set.seed(9876)
	p_r <- spsample(bounds, 1000, type='random')
	dem <- dem[bounds,]
	dem_sp <- as(dem, 'SpatialPixelsDataFrame')
	g_r <- spsample(dem_sp, 1000, type='random')
	g_rg <- spsample(dem_sp, 1000, type='regular')
	g_rg2k <- spsample(dem_sp, 2000, type='regular')
	
	par(mfrow=c(2, 2))
	plot(bounds)
	plot(p_r, add=TRUE, cex=0.15)
	title(main="polygon_random")
	plot(g_r, cex=0.15)
	title(main="grid_random")
	plot(g_rg, cex=0.15)
	title(main="grid_regular")
	plot( ecdf(p_r_dem$band1), verticals=TRUE, do.p=FALSE, ann=FALSE,
 		  col.hor="green", col.vert="green" )
	title(main="ECDF")
	plot( ecdf(g_r_dem$band1), verticals=TRUE, do.p=FALSE,
 		  col.hor="blue", col.vert="blue", add=TRUE )
	plot( ecdf(g_rg_dem$band1), verticals=TRUE, do.p=FALSE,
		  col.hor="red", col.vert="red", add=TRUE )
	abline(h=c(0.25,0.5,0.75), lty=2, col="grey")
	legend( "bottomright", c("polygon random", "grid random", "grid regular"),
			col=c("green", "red", "blue"), lty=1, bty="n" )
	
	p_r_dem <- over(p_r, dem)
	g_r_dem <- over(g_r, dem)
	g_rg_dem <- over(g_rg, dem)
	
	tab <- rbind( polygon_random=c( fivenum(p_r_dem$band1), 
				  				    nrow(p_r_dem) ),
				  grid_random=c( fivenum(g_r_dem$band1), 
				 				 nrow(g_r_dem) ),
				  grid_regular=c( fivenum(g_rg_dem$band1), 
				 				  nrow(g_rg_dem) ) )
	tab
	
	o_sp <- as(olinda_utm, 'SpatialPolygons')
	whichPoly <- over(p_r, o_sp)
	whichPoly1 <- gContains(o_sp, p_r, byid=T)
	whichPoly1a <- apply(unname(whichPoly1), 1, which)
	table(sapply(whichPoly1a, length))
	all.equal(whichPoly, whichPoly1a)
	

#5.4 Auxiliary Functions
hels <- matrix(c(24.97, 60.17), nrow=1)
p4s <- CRS('+proj=longlat +datum=WGS84')
Hels <- SpatialPoints(hels, proj4string=p4s)
d041224 <- as.POSIXct('2004-12-24', tz='EET')
sunriset(Hels, d041224, direction='sunrise', POSIXct.out=T)



save.image('~/Desktop/R/Spatial/SpatialAnalysisBook/SpatialAnalysis.RData')
