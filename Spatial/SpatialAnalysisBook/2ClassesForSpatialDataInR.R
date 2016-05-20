##########################################################
#                                                        #
#  Examples and Exercises from Bivand et al.             #                         
#  "Applied Spatial Data Analysis with R                 #
#  Damian Satterthwaite - Phillips <damiansp@gmail.com>  #
#  Updated: 16Jan2014                                    #
#                                                        #
##########################################################

rm(list=ls())
search()



load('~/Desktop/R/Spatial/SpatialAnalysisBook/SpatialAnalysis.RData')
library(ctv)
library(maps)
library(maptools)
library(raster)
library(rgdal)
library(rgeos)
library(sp)
load('~/Desktop/R/Spatial/SpatialAnalysisBook/cm_bundle/high.RData')
load('~/Desktop/R/Spatial/SpatialAnalysisBook/cm_bundle/auck_gshhs.RData')
#source( '~/Desktop/R/Spatial/SpatialAnalysisBook/cm_bundle/cm_mod.R', 
#		chdir = TRUE )
#source( '~/Desktop/R/Spatial/SpatialAnalysisBook/cm_bundle/legend_image.R',
#		chdir = TRUE)
#install.views("Spatial")  #Do once only, then save

# 2.3 Spatial Objects
getClass('Spatial')
getClass('CRS')	# Coordinate Reference System

m <- matrix(c(0, 0, 1, 1), ncol = 2, dimnames = list(NULL, c('min', 'max')) )
crs <- CRS(projargs = as.character(NA))
S <- Spatial(bbox = m, proj4string = crs)
bb <- matrix(c(350, 85, 370, 95), ncol=2, dimnames=list(NULL, c('min','max')))
Spatial(bb, proj4string=CRS('+proj=longlat'))	# Sanity check: intentionly 
												# throws error as coords 370, 
												# and 95 are outside lat/long 
												# ranges

# 2.4 Spatial Points
CRAN_df <- read.table('~/Desktop/R/Spatial/SpatialAnalysisBook/CRAN051001a.txt', header=T)
CRAN_mat <- cbind(CRAN_df$long, CRAN_df$lat)
row.names(CRAN_mat) <- 1:nrow(CRAN_mat)
str(CRAN_mat)

getClass('SpatialPoints')

llCRS <- CRS('+proj=longlat +ellps=WGS84')
CRAN_sp <- SpatialPoints(CRAN_mat, proj4string = llCRS)
summary(CRAN_sp)

	# 2.4.1 Methods
	bbox(CRAN_sp)
	proj4string(CRAN_sp)
	proj4string(CRAN_sp) <- CRS(as.character(NA))
	proj4string(CRAN_sp)
	proj4string(CRAN_sp) <- llCRS
	
	brazil <- which(CRAN_df$loc == 'Brazil')
	brazil
	coordinates(CRAN_sp)[brazil,]
	summary(CRAN_sp[brazil,])
	
	south_of_equator <- which(coordinates(CRAN_sp)[, 2] < 0)
	summary(CRAN_sp[-south_of_equator,])
	

	# 2.4.2 Data Frames for Spatial Point Data
	str(row.names(CRAN_df))
	CRAN_spdf1 <- SpatialPointsDataFrame(coords = CRAN_mat, data = CRAN_df, proj4string = llCRS,
										 match.ID = T)
	CRAN_spdf1[4,]
	str(CRAN_spdf1$loc)
	str(CRAN_spdf1[['loc']])
	
	s <- sample(nrow(CRAN_df))
	CRAN_spdf2 <- SpatialPointsDataFrame(CRAN_mat, CRAN_df[s,], 
										 proj4string = llCRS, match.ID = T)
	all.equal(CRAN_spdf1, CRAN_spdf2)
	
	getClass("SpatialPointsDataFrame")

	names(CRAN_spdf1)
	str(model.frame(lat ~ long, data = CRAN_spdf1), give.attr = F)
	
	CRAN_spdf4 <- SpatialPointsDataFrame(CRAN_sp, CRAN_df)
	all.equal(CRAN_spdf4, CRAN_spdf1)
	
	CRAN_df0 <- CRAN_df
	coordinates(CRAN_df0) <- CRAN_mat
	proj4string(CRAN_df0) <- llCRS
	all.equal(CRAN_df0, CRAN_spdf1)
	str(CRAN_df0, max.level = 2)
		
	CRAN_df1 <- CRAN_df
	names(CRAN_df1)
	coordinates(CRAN_df1) <- c('long', 'lat')
	proj4string(CRAN_df1) <- llCRS
	str(CRAN_df1, max.level = 2)
	# Note that in some cases coordinates may be in the SpatialPointsDataFrame@coords slot, 
	# (in which case @coords.nrs value will also have values), or in the data frame itself 
	# @data
	
	turtle_df <- read.csv('~/Desktop/R/Spatial/SpatialAnalysisBook/seamap105_mod.csv')
	summary(turtle_df)
	
	timestamp <- as.POSIXlt(strptime(as.character(turtle_df$obs_date), 
									 '%m/%d/%Y %H:%M:%S'), 'GMT')
	turtle_df1 <- data.frame(turtle_df, timestamp = timestamp)
	turtle_df1$lon <- ifelse(turtle_df1$lon < 0, turtle_df1$lon + 360, 
							 turtle_df1$lon)
	turtle_sp <- turtle_df1[order(turtle_df1$timestamp), ]
	coordinates(turtle_sp) <- c('lon', 'lat')
	proj4string(turtle_sp) <- llCRS
	


# 2.5 Spatial Lines
getClass('Line')
getClass('Lines')
getClass('SpatialLines')

japan <- map('world', 'japan')
p4s <- CRS('+proj=longlat +ellps=WGS84')
SLjapan <- map2SpatialLines(japan, proj4string = p4s)
str(SLjapan, max.level = 2)

Lines_len <- sapply(slot(SLjapan, 'lines'), function(x) length(slot(x, 'Lines')))
table(Lines_len)

volcano_sl <- ContourLines2SLDF(contourLines(volcano))
plot(volcano_sl)
t(slot(volcano_sl, 'data'))

auck_shore <- MapGen2SL('~/Desktop/R/Spatial/SpatialAnalysisBook/auckland_mapgen.dat', llCRS)
plot(auck_shore)
summary(auck_shore)



# 2.6 SpatialPolygons
lns <- slot(auck_shore, 'lines')
table(sapply(lns, function(x) length(slot(x,  'Lines')) ))

# for each line, is it a polygon? (does coord1 == final.coord?)
islands_auck <- sapply(lns, function(x) {
	crds <- slot(slot(x, 'Lines')[[1]], 'coords')
	identical(crds[1, ], crds[nrow(crds), ])
})

table(islands_auck)

getClass('Polygon')
getClass('Polygons')
getClass('SpatialPolygons')

islands_sl <- auck_shore[islands_auck]
list_of_Lines <- slot(islands_sl, 'lines')
islands_sp <- SpatialPolygons(lapply( list_of_Lines, 
                              		  function(x) {
                              		  	  Polygons(list(Polygon(slot(slot( x, 'Lines')[[1]], 														   						   	   'coords') )), 
										  ID = slot(x, 'ID')  )
							   		   } ), proj4string = llCRS)
summary(islands_sp)
slot(islands_sp, 'plotOrder')
order( sapply(slot(islands_sp, 'polygons'), function(x) slot(x, 'area')), 
       decreasing = T )
plot(auck_shore)
plot(islands_sp, col='cyan', border='blue',  add=T)

	# 2.6.1 SpatialPolygonsDataFrame Objects
	state.map <- map('state', fill=T)
	IDs <- sapply(strsplit(state.map$names, ':'), function(x) x[1])
	state.sp <- map2SpatialPolygons(state.map, IDs = IDs, proj4string = llCRS)
	sat <- read.table('~/Desktop/R/Spatial/SpatialAnalysisBook/stateSATData_mod.txt', 
					  row.names = 5, header = T)
	str(sat)
	
	sat1 <- sat[!is.na(id), ]
	state.spdf <- SpatialPolygonsDataFrame(state.sp, sat)
		
	str(slot(state.spdf, 'data'))
	str(state.spdf, max.level = 2)
	
	DC <- 'district of columbia'
	not_dc <- !(row.names(slot(state.spdf, 'data')) == DC)
	state.spdf1 <- state.spdf[not_dc, ]
	length(slot(state.spdf1, 'polygons'))
	dim(state.spdf1)
	summary(state.spdf1)
	
	
	
	# 2.6.2 Holes and Ring Direction
	length(slot(manitoulin_sp, 'polygons'))
	sapply( slot(slot(manitoulin_sp, 'polygons')[[1]], 'Polygons'), 
		    function(x) {slot(x, 'hole')} )
	sapply( slot(slot(manitoulin_sp, 'polygons')[[1]], 'Polygons'), 
		    function(x) {slot(x, 'ringDir')} )
	manitoulin_sp <- createSPComment(manitoulin_sp)
	sapply(slot(manitoulin_sp, 'polygons'), comment)
	plot(manitoulin_sp, col='wheat')


# 2.7 SpatialGrid and SpatialPixel Objects
getClass('GridTopology')

bb <- bbox(manitoulin_sp)
bb
cs <- c(0.01, 0.01)
cc <- bb[, 1] + cs/2
cd <- ceiling(diff(t(bb)) / cs)
manitoulin_grd <- GridTopology( cellcentre.offset = cc, cellsize = cs, 
								cells.dim = cd )
manitoulin_grd

getClass('SpatialGrid') 
p4s <- CRS(proj4string(manitoulin_sp))
manitoulin_SG <- SpatialGrid(manitoulin_grd, proj4string = p4s)
summary(manitoulin_SG)
plot(manitoulin_SG)	# just the grid

class(auck_el1)
slot(auck_el1, 'grid')
slot(auck_el1, 'bbox') 
object.size(auck_el1)
object.size(slot(auck_el1, 'data'))
is.na(auck_el1$band1) <- auck_el1$band1 <= 0
summary(auck_el1$band1)

auck_el2 <- as(auck_el1, 'SpatialPixelsDataFrame')
object.size(auck_el2)

auck_el_500 <- auck_el2[auck_el2$band1 > 500, ]
summary(auck_el_500)
object.size(auck_el_500)
rm(auck_el2)

data(meuse.grid)
mg_SP <- SpatialPoints(cbind(meuse.grid$x, meuse.grid$y))
summary(mg_SP)
mg_SPix0 <- SpatialPixels(mg_SP)
summary(mg_SPix0)
prod(slot(slot(mg_SPix0, 'grid'), 'cells.dim'))
mg_SPix1 <- as(mg_SP, 'SpatialPixels')
summary(mg_SPix1)



# 2.8 Raster Object and the raster Package
r <- raster('~/Desktop/R/Spatial/SpatialAnalysisBook/cm_bundle/70042108.tif')
r
par(mar = c(0, 0, 0, 0))
plot(log(r))
class(r)
inMemory(r)
object.size(r)
cellStats(r, min)
cellStats(r, max)

out <- raster(r)
bs <- blockSize(out)
out <- writeStart(out, filename=tempfile(), overwrite=T)
for (i in 1:bs$n) {
	v <- getValues(r, row=bs$row[i], nrows=bs$nrows[i])
	v[v <= 0] <- NA
	writeValues(out, v, bs$row[i])
}
out <- writeStop(out)
cellStats(out, min)
inMemory(out)

plot(out, col=terrain.colors(100))
plot(out^0.5, col=terrain.colors(100))
plot(out, col=topo.colors(100))
plot(out, col=heat.colors(100))
r1 <- as(out, 'SpatialGridDataFrame')
summary(r1)
r2 <- as(r1, 'RasterLayer')
summary(r2)



save.image('~/Desktop//R/Spatial/SpatialAnalysisBook/SpatialAnalysis.RData')


# To downlowd needed objects ... #
#ASDAR_BOOK <- "http://www.asdar-book.org/book2ed"
#chapters <- c("vis", "die", "cm2","std", "sppa", "geos", "lat", "dismap")
#for (i in chapters) {
#	fn <- paste(i, "mod.R", sep="_")
#	download.file(paste(ASDAR_BOOK, fn, sep = "/"), fn)
#}
#list.files()
