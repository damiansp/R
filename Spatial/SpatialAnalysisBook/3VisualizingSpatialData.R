#=======================================================#
#                                                       #
#  Examples and Exercises from Bivand et al. 			#
#	"Applied Spatial Data Analysis  with R"             #
#  Chapter 3: Visualizing Spatial Data					#
#  Damian Satterthwaite - Phillips <damiansp@gmail.com>	#
#  Updated: 20Jan2014                                   #
#                                                       #
#=======================================================#

rm(list=ls())
load('~/Desktop/R/Spatial/SpatialAnalysisBook/SpatialAnalysis.RData')
#source('~/Desktop/R/Spatial/SpatialAnalysisBook/vis_mod.R', chdir = TRUE)

# install.views('Spatial')
library(classInt)
library(ctv)
library(ggplot2)
library(grid)
library(gstat)
library(lattice)
library(maps)
library(maptools)
library(RColorBrewer)
library(rgdal)
library(sp)

#data(meuse)
#data(meuse.riv)
#data(meuse.grid)

# 3.1 The Traditional Plot System
	# 3.1.1 Plotting Points, Lines, Polygons, and Grids
	coordinates(meuse) <- c('x', 'y')
	plot(meuse)
	title('points')
	
	cc <- coordinates(meuse)
	m.sl <- SpatialLines(list(Lines(list(Line(cc)), '1')))
	plot(m.sl, add=T)
	title('\nlines')
	
	meuse.lst <- list(Polygons(list(Polygon(meuse.riv)), 'meuse.riv'))
	meuse.pol <- SpatialPolygons(meuse.lst)
	plot(meuse.pol, col='blue', add=T)
	title('polygons')
	
	coordinates(meuse.grid) <- c('x', 'y')
	meuse.grid <- as(meuse.grid, 'SpatialPixels')
	image(meuse.grid)

	plot(meuse.pol, col='blue', add=T)	
	plot(meuse, add = T)
	
	# 3.1.2 Axes and Layout Elements
	layout(matrix(c(1,2), 1, 2))
	plot(meuse.pol, axes = T)
	plot(meuse.pol, axes = F)
	axis(1, at = c(178000 + 0:2*2000), cex.axis = 0.7)
	axis(2, at = c(326000 + 0:3*4000), cex.axis = 0.7)
	box()
	
	oldpar <- par(no.readonly = T)
	layout(matrix(c(1,2), 1, 2))
	plot(meuse, axes = T, cex = 0.6)
	plot(meuse.pol, col = 'blue', add = T)
	title('Sample Locations')
	par(mar = c(0, 0, 0, 0) + 0.1)
	plot(meuse, axes = F, cex = 0.6)
	plot(meuse.pol, col = 'blue', add = T)
	box()
	par(oldpar)
#	par(mfrow=c(1,1))
	
	plot(meuse)
	plot(meuse.pol, add=T, col='blue')
	plot(meuse)
	# Add scale legend
	SpatialPolygonsRescale( layout.scale.bar(), offset=locator(1), 
							scale=1000, fill=c('transparent', 'black'), 
							plot.grid=F )
	text(locator(1), '0')
	text(locator(1), '1 km')
	SpatialPolygonsRescale( layout.north.arrow(), offset=locator(1), 	
							scale=400, plot.grid=F )
	box()



	# 3.1.3 Degrees in Axes Labels and Reference Grid
	wrld <- map('world', interior = F, xlim = c(-179, 179), ylim=c(-89, 89))
	wrld_p <- pruneMap(wrld, xlim = c(-179, 179))
	wrld_sp <- map2SpatialLines(wrld_p, proj4string=llCRS)
	prj_new <- CRS('+proj=moll')
	wrld_proj <- spTransform(wrld_sp, prj_new)
	wrld_grd <- gridlines( wrld_sp, 
						   easts = c(-179, seq(-150, 150, 50), 179.5), 
						   norths = seq(-75, 75, 15), ndiscr = 100 )
	wrld_grd_proj <- spTransform(wrld_grd, prj_new)
	at_sp <- gridat( wrld_sp, easts = 0, norths = seq(-75, 75, 15), 
					 offset = 0.3 )
	at_proj <- spTransform(at_sp, prj_new)
#	plot(wrld_proj, col='grey60')
#  	plot(wrld_grd_proj, add = T, lty = 3, col='grey70')
#	text( coordinates(at_proj), pos = at_proj$pos, offset = at_proj$offset, 
#		  labels = parse(text = as.character(at_proj$labels)), cex = 0.6 )



	# 3.1.4 Plot Size, Plotting Area, Map Scale, and Multiple Plots
	par('pin')
	par(pin = c(4, 4))
	dev.off()
	quartz(width = 10, height = 10)
	
	# To write graphics to a file, use, e.g.:
	# postscript('file.ps', width = 10, height = 10)
	pin <- par('pin')
	dxy <- apply(bbox(meuse), 1, diff)
	ratio <- dxy[1] / dxy[2]
	par(pin = c(ratio * pin[2], pin[2]), xaxs = 'i', yaxs = 'i')	
	# xaxs='i' means do not	use default 4% extension to plotting region
	plot(meuse, pch = 1)
	box()
	
	
	
	# 3.1.5 Plotting Attributes and Map Legends
	greys <- grey.colors(4, 0.55, 0.95)
	# Missing object: zn.idw
	image(zn.idw, col = greys, breaks = log(c(100, 200, 400, 800, 1800)))
	plot(meuse.pol, add = T)
	plot(meuse, pch = 1, cex=sqrt(meuse$zinc) / 20, add = T)
	legVals <- c(100, 200, 500, 1000, 2000)
	legend( 'left', legend = legVals, pch = 1, pt.cex = sqrt(legVals) / 20, 
		    bty = 'n', title = 'measured' )
	legend( 'topleft', 
			legend = c('100-200', '200-400', '400-800', '800-1800'), 
		    fill = greys, bty = 'n', title = 'interpolated' )



# 3.2 Trellis/Lattice Plots with spplot
	# 3.2.1 A Straight Trellis Example
	# Missing object: zn
	levelplot( z ~ x + y | name, spmap.to.lev(zn[c('direct', 'log')]), 
			   asp = 'iso' )
	spplot(zn[c('direct', 'log')])
	
	
		
	# 3.2.2 Plotting Points, Lines, Polygons, and Grids
#	coordinates(meuse.grid) <- c('x', 'y')	# already done in code above
	meuse.grid <- as(meuse.grid, 'SpatialPixelsDataFrame')
	im <- as.image.SpatialGridDataFrame(meuse.grid['dist'])
	cl <- ContourLines2SLDF(contourLines(im))
	spplot(cl)
	
	
	
	# 3.2.3 Adding Reference and Layout Elements to Plots
	river <- list('sp.polygons', meuse.pol)
	north <- list( 'SpatialPolygonsRescale', layout.north.arrow(), 
				   offset = c(178750, 332500), scale = 400 )
	scale <- list( 'SpatialPolygonsRescale', layout.scale.bar(), 
				   offset = c(180200, 329800), scale = 1000, 
				   fill = c('transparent', 'black') )
	txt1 <- list('sp.text', c(180200, 329950), '0')
	txt2 <- list('sp.text', c(181200, 329950), '1')
	pts <- list('sp.points', meuse, pch = 3, col = 'black')
	meuse.layout <- list(river, north, scale, txt1, txt2, pts)
	spplot(zn['log'], sp.layout = meuse.layout)
	
	
	
	# 3.2.4 Arranging Panel Layout
	
	
	
# 3.3 Alternative Routes: ggplot, latticeExtra
methods(fortify)

m <- as(meuse, 'data.frame')
ggplot(m, aes(x, y)) + geom_point() + coord_equal()

p <- spplot(meuse['zinc'])
m <- SpatialPolygonsDataFrame(meuse.pol, data.frame(col=1), match.ID=F)
l <- spplot(m)
l + p
p + l

# 3.4 Interactive Plots
	# 3.4.1 Interacting with Base Graphics
	plot(meuse)
	meuse.id <- identify(coordinates(meuse))
	
	plot(meuse)
	region <- locator(type = 'o')
	n <- length(region$x)
	p <- Polygon(cbind(region$x, region$y)[c(1:n, 1), ], hole=F)
	ps <- Polygons(list(p), ID = 'region')
	sps <- SpatialPolygons(list(ps))
	plot(meuse[sps, ], pch=16, cex=0.5, add=T, col=2)
	
	prj <- CRS('+proj=longlat +datum=NAD27')
	nc_shp <- system.file('shapes/sids.shp', package='maptools')[1]
	nc <- readShapePoly(nc_shp, proj4string=prj)
	plot(nc)
	pt <- locator(type='p')
	print(pt)
	pt.sp <- SpatialPoints(cbind(pt$x, pt$y), proj4string=prj)
	over(pt.sp, nc)
	
	# 3.4.2 Interacting with spplot and Lattice Plots
	ids <- spplot(meuse, 'zinc', identify=T)
	trellis.focus('panel', column=1, row=1)
	ids <- panel.identify()
	trellis.unfocus()
	
	trellis.focus('panel', column=1, row=1)
	as.numeric(grid.locator())
	trellis.unfocus()



# 3.5 Color Palettes and Class Intervals
	# 3.5.1 Color Palettes
	rw.colors <- colorRampPalette(c('red', 'white'))
	image(meuse.grid['dist'], col=rw.colors(10))
	image(meuse.grid['dist'], col=rw.colors(20))
	image(meuse.grid['dist'], col=rw.colors(50))
	image(meuse.grid['dist'], col=rw.colors(100))
	image(meuse.grid['ffreq'], col=rw.colors(3))
	image(meuse.grid['soil'], col=rw.colors(3))
#	example(brewer.pal)


	# 3.5.2 Class Intervals
	pal <- brewer.pal(5, 'Reds')
	q5 <- classIntervals(meuse$zinc, n=5, style='quantile')
	q5
	diff(q5$brks)
	plot(q5, pal=pal)
	
	fj5 <- classIntervals(meuse$zinc, n=5, style='fisher')
	fj5
	diff(fj5$brks)
	plot(fj5, pal=pal)
	
	q5Colors <- findColours(q5, pal)
	plot(meuse, col=q5Colors, pch=19)
	legend( 'topleft', fill=attr(q5Colors, 'palette'), 
		    legend=names(attr(q5Colors, 'table')), bty='n' )
	fj5Colors <- findColours(fj5, pal)
	plot(meuse, col=fj5Colors, pch=19, main='Fisher-Jenks')
	legend( 'topleft', fill=attr(fj5Colors, 'palette'), 
		    legend=names(attr(fj5Colors, 'table')), bty='n' )
		   
	cuts <- (0:10) / 10
	spplot(meuse.grid, 'dist', colorkey=list(labels=list(at=cuts)), at=cuts)

	cuts <- (0:50) / 50
	spplot(meuse.grid, 'dist', colorkey=list(labels=list(at=cuts)), at=cuts)
	


save.image('~/Desktop/R/Spatial/SpatialAnalysisBook/SpatialAnalysis.RData')