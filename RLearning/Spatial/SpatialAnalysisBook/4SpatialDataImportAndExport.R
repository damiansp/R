#=======================================================#
#                                                       #
#  Examples and Exercises from Bivand et al. 			#
#	"Applied Spatial Data Analysisi with R"				#
#  Chapter 4: Spatial Data Import and Export			#
#  Damian Satterthwaite-Phillips <damiansp@gmail.com>   #
#  Updated: 23May2015                                   #
#                                                       #
#=======================================================#

rm(list=ls())
load('~/Desktop/R/Spatial/SpatialAnalysisBook/SpatialAnalysis.RData')
#source('~/Desktop/R/Spatial/SpatialAnalysisBook/die_mod.R', chdir = TRUE)
library(DCluster)
library(gstat)
library(osmar)
library(raster)
library(rgeos)
library(rgdal)
library(RgoogleMaps)
library(spacetime)
library(spdep)
library(spgrass6)
#data(wrld_simpl)

# 4.1 Coordinate Reference System
NEWS <- 'http://svn.osgeo.org/metacrs/proj/trunk/proj/NEWS'
PROJ4_NEWS <- readLines(url(NEWS)) 	# use to determine the version of EPSG 
									# being used
lns <- grep('Release Notes|EPSG', PROJ4_NEWS)
head(PROJ4_NEWS[lns])

	# 4.1.1 Using the EPSG List
	EPSG <- make_EPSG()	# provides a data frome of all listed CRS in the EPSG 
						# set
	# Find a particular CRS:
	EPSG[grep("^# ED50$", EPSG$note),]
	# code: 4230 (ED50)
	# +proj=longlat +ellps=intl +towgs84=-87,-98,-121,0,0,0,0 +no_defs
	
	# 4.1.2 PROJ.4 CRS Specification
	CRS("+init=epsg:4230") # The ED50 CRS grepped above
	ED50 <- CRS("+init=epsg:4230 +towgs84=-87,-96,-120,0,0,0,0") 
	# can change/update portions manually
	ED50
	# not necessary to do so in this case, so...
	ED50 <- CRS('+init=epsg:4230') 

	# 4.1.3 Projection and Transformation
	IJ.east <- as(char2dms("4d31'00\"E"), 'numeric')
	IJ.north <- as(char2dms("52d28'00\"N"), 'numeric')
	IJ.ED50 <- SpatialPoints(cbind(x = IJ.east, y = IJ.north), ED50)
	res <- spTransform(IJ.ED50, CRS("+proj=longlat +datum=WGS84"))
	x <- as(dd2dms(coordinates(res)[1]), 'character')
	y <- as(dd2dms(coordinates(res)[2], T), 'character')
	cat(x, y, "\n")
	spDistsN1(coordinates(IJ.ED50), coordinates(res), longlat=T) * 1000
	
	gzAzimuth(coordinates(IJ.ED50), coordinates(res))
	
	proj4string(IJ.ED50) <- CRS('+init=epsg:4230')
	res <- spTransform(IJ.ED50, CRS('+proj=longlat +datum=WGS84'))
	spDistsN1(coordinates(IJ.ED50), coordinates(res), longlat=T) * 1000
	gzAzimuth(coordinates(IJ.ED50), coordinates(res))
	
	EPSG[grep('Atlas', EPSG$note), 1:2]
	CRS("+init=epsg:2163")
	
	proj <- projInfo('proj')
	proj[proj$name == 'laea',]
	ellps <- projInfo('ellps')
	ellps[grep('a=6370997', ellps$major), ]
	
	
	# 4.1.4 Degrees, Minutes, and Seconds
	IJ_east <- char2dms(IJ.dms.E)
	IJ_north <- char2dms(IJ.dms.N)
	getSlots('DMS')
	c(as(IJ_east, 'numeric'), as(IJ_north, 'numeric'))
	
	
# 4.2 Vector File Formats
	# 4.2.1 Using OGR Drivers in rgdal
	head(ogrDrivers())
	vignette('OGR_shape_encoding', package='rgdal')
	
	scot_dat <- read.table( 
				 '~/Desktop/R/Spatial/SpatialAnalysisBook/Scot/scotland.dat', 
				 skip=1
				)
	names(scot_dat) <- c( 'District', 'Observed', 'Expected', 'PcAFF', 
						  'Latitude', 'Longitude' )
	ogrInfo('./Desktop/R/Spatial/SpatialAnalysisBook/Scot/', 'scot')	
	scot_LL <- readOGR( dsn='./Desktop/R/Spatial/SpatialAnalysisBook/Scot', 
						layer='scot' )
	plot(scot_LL)
	proj4string(scot_LL) <- CRS('+proj=longlat +ellps=WGS84')
	plot(scot_LL)
	
	sapply(slot(scot_LL, 'data'), class)
	scot_LL$ID	
	scot_dat$District	# same data but differently ordered
	ID_D <- match(scot_LL$ID, scot_dat$District)
	scot_dat1 <- scot_dat[ID_D,]
	row.names(scot_dat1) <- row.names(scot_LL)
	scot_LLa <- spCbind(scot_LL, scot_dat1)
	all.equal(scot_LLa$ID, scot_LLa$District)	# bueno!
	names(scot_LLa)
	O <- scot_LLa$Observed
	E <- scot_LLa$Expected
	scot_LLa$SMR <- probmap(O, E)$relRisk / 100
	scot_LLa$smth <- empbaysmooth(O, E)$smthrr
	
	scot_BNG <- spTransform(scot_LLa, CRS('+init=epsg:27700'))
	plot(scot_BNG)
	spplot( scot_BNG, c("SMR", "smth"), 
			at=c(0, 0.25, 0.5, 0.8, 1, 1.5, 2.5, 4.5, 7),
			col.regions=rev(brewer.pal(8, "RdBu")) )

	drv <- 'ESRI Shapefile'
	writeOGR( scot_BNG, dsn='./Desktop/R/Spatial/SpatialAnalysisBook/Scot/',
			  layer='scot_BNG', driver=drv )
	list.files( './Desktop/R/Spatial/SpatialAnalysisBook/Scot/', 
				pattern='scot' )

	dsn <- "WFS:http://geohub.jrc.ec.europa.eu/effis/ows"	# site is temp 
															# down
	ogrListLayers(dsn)
	Fires <- readOGR(dsn, 'EFFIS:FiresAll')
	names(Fires)
	x <- c(-15, -15, 38, 38, -15)
	y <- c(28, 62, 62, 28, 28)
	crds <- cbind(x=x, y=y)
	bb <- SpatialPolygons(list(Polygons(list(Polygon(coords=crds)), '1')))
	proj4string(bb) <- CRS(proj4string(wrld_simpl))
	# get only that part of the wrld_simpl map that lies within bb:
	slbb <- gIntersection(bb, as(wrld_simpl, 'SpatialLines'))
	plot(slbb)
	
	Fires$dt <- as.Date(as.character(Fires$FireDate), format='%d-%m-%Y')
	# Remove data points for (off-map) Réunion
	Fires0 <- Fires[-which(coordinates(Fires)[, 2] < 0), ]
	Fires1 <- Fires0[order(Fires0$dt), ]
	Fires2 <- STIDF( as(Fires1, 'SpatialPoints'), Fires1$dt, 
					 as(Fires1, 'data.frame') )
	spl <- list('sp.lines', slbb, lwd=0.7, col='khaki4')
	stplot(Fires2, numbers=3, sp.layout=spl, cex=0.5)
	
	names(Fires1)[1] <- 'name'	# 'name' label mandatory for OGR GPX driver
	GR_Fires <- Fires1[Fires1$Counrtry == 'GR']	# Greece
	writeOGR( GR_Fires, 'EFFIS.gpx', 'waypoints', driver='GPX', 
			  dataset_options='GPX_USE_EXTENSIONS=YES' )
	
	GR <- readOGR('EFFIS.gpx', 'waypoints')
	GR[1, c(5, 24:28)]
	
	


	# 4.2.2 Other Import/Export Functions
	getinfo.shape('~/Desktop/R/Spatial/SpatialAnalysisBook/Scot/scot.shp')


# 4.3 Raster File Formats
	# 4.3.1 Using GDAL Drivers in rgdal
	auck_e11 <- readGDAL(
		'~/Desktop/R/Spatial/SpatialAnalysisBook/70042108/70042108.tif'
	)
	summary(auck_e11)
	par(mar=rep(0, 4))
	image(auck_e11)
	is.na(auck_e11$band1) <- auck_e11$band1 <= 0 | auck_e11$band1 > 10000
	image(auck_e11, col=terrain.colors(50))
	
	x <- GDAL.open(
		'~/Desktop/R/Spatial/SpatialAnalysisBook/70042108/70042108.tif'
	)
	xx <- getDriver(x)
	xx
	getDriverLongName(xx)
	dim(x)
	GDAL.close(x)
	
	GDALinfo('~/Desktop/R/Spatial/SpatialAnalysisBook/70042108/70042108.tif')

	brks <- c(0, 10, 20, 50, 100, 150, 200, 300, 400, 500, 600, 700)
	pal <- terrain.colors(11)
	pal
	length(pal) == length(brks) - 1
	auck_e11$band1 <- findInterval(auck_e11$band1, vec=brks, all.inside=T) - 		1
	writeGDAL( 
		auck_e11, 
		'~/Desktop/R/Spatial/SpatialAnalysisBook/70042108/demIndex.tif',
		drivername='GTiff', type='Byte', colorTable=list(pal),
		mvFlag=length(brks) - 1
	)
	Gi <- GDALinfo(
		'~/Desktop/R/Spatial/SpatialAnalysisBook/70042108/demIndex.tif', 
		returnColorTable=T
	)
	CT <- attr(Gi, 'ColorTable')[[1]]
	CT[CT > '#000000']
	
	log_zinc <- idw(log(zinc) ~ 1, meuse, meuse.grid)['var1.pred'] 
	# idw: inverse distance weighted interpolation
	summary(log_zinc)
	writeGDAL(
		log_zinc, 
		fname = 
			'~/Desktop/R/Spatial/SpatialAnalysisBook/70042108/log_zinc.tif',
		drivername='GTiff', type='Float32', options='INTERLEAVE=PIXEL'
	)
	GDALinfo('~/Desktop/R/Spatial/SpatialAnalysisBook/70042108/log_zinc.tif')
	
	Soil <- meuse.grid['soil']
	table(Soil$soil)
	Soil$soil <- as.integer(Soil$soil) - 1
	Cn <- c('Rd10A', 'Rd90C/VII', 'Bkd26/VII')
	writeGDAL( Soil, 
			   '~/Desktop/R/Spatial/SpatialAnalysisBook/70042108/Soil.tif', 
			   drivername='GTiff', type='Byte', catNames=list(Cn), 
			   mvFlag=length(Cn) )
	Gi <- GDALinfo(
		'~/Desktop/R/Spatial/SpatialAnalysisBook/70042108/Soil.tif',
	     returnCategoryNames=T
	)
	attr(Gi, 'CATlist')[[1]]
	summary(readGDAL(
		'~/Desktop/R/Spatial/SpatialAnalysisBook/70042108/Soil.tif'
	))
	
	# list available drivers
	head(gdalDrivers(), n=10)
	writeGDAL(
		log_zinc,
		fname = 
			'~/Desktop/R/Spatial/SpatialAnalysisBook/70042108/log_zinc.rda',
		drivername='R'
	)
	GDALinfo('~/Desktop/R/Spatial/SpatialAnalysisBook/70042108/log_zinc.rda')
	
	# Read data for OpenGIS Web Map Service (WMS)
	service_xml <- 'frmt_wms_openstreetmap_tms.xml'
	offset <- c(19339000, 34546000)
	osm <- readGDAL( osm_xml, offset=offset, region.dim=c(2000, 2000), 
					 output.dim=c(1000, 1000) )
	summary(osm)
	
	
	# 4.3.2 Other Import/Export Functions
	# readAsciiGrid() in maptools


# 4.4 Google Earth, Google Maps, and Other Formats
myMap <- GetMap( center=c(60.395, 5.322), zoom=16, 
				 destfile = 
				 	'~/Desktop/R/Spatial/SpatialAnalysisBook/MyTile2.png',
				 maptype='mobile' )
BB <- do.call('rbind', myMap$BBOX)
dBB <- rev(diff(BB))
DIM12 <- dim(myMap$myTile)[1:2]
cs <- dBB / DIM12
cc <- c(BB[1, 2] + cs[1] / 2, BB[1, 1] + cs[2] / 2)
GT <- GridTopology(cc, cs, DIM12)
p4s <- CRS('+proj=longlat +datum=WGS84')
SG_myMap <- SpatialGridDataFrame( GT, proj4string=p4s, 
								  data=data.frame( 
								  	  r=c(t(myMap$myTile[,, 1])) * 255, 
								 	  g=c(t(myMap$myTile[,, 2])) * 255,
								 	  b=c(t(myMap$myTile[,, 3])) * 255 
								  ) )
tstack <- stack('~/Desktop/R/Spatial/SpatialAnalysisBook/MyTile2.png')	
extent(tstack) <- extent(BB[1, 2], BB[2, 2], BB[1, 1], BB[2, 1])
plotRGB(tstack)				  
								  
								  
#myMap1 <- GetMap.OSM( 
#			lonR=c(5.319, 5.328), latR=c(60.392, 60.398), scale=4000, 
#			destfile='~/Desktop/R/Spatial/SpatialAnalysisBook/MyTile.png' 
#		  )
#image(myMap1$myTile)

api <- osmsource_api()
box <- corner_bbox(5.319, 60.392, 5.328, 60.398)
torget <- get_osm(box, source=api)
torget1 <- as_sp(torget, 'lines')
sort(table(torget1$user), decreasing=T)
bybane <- find(torget, way(tags(k == 'railway')))
bybane <- find_down(torget, way(bybane))
bybane <- subset(torget, ids=bybane)
bybane <- as_sp(bybane, 'lines')

#image(t(SG_myMap))
plotRGB(tstack)				  
plot(torget1, add=TRUE)
plot(bybane, add=TRUE, lwd=5, col="orange2")
plot(0:1, 0:1, type = "n", axes = FALSE, asp=1)
rasterImage(myMap1[[4]], 0, 0, 1, 1)


writeOGR( Fires[, c('gml_id', 'FireDate', 'Area_HA')], 
		  dsn='~/Desktop/R/Spatial/SpatialAnalysisBook/Fires.kml', 
		  layer='fires', driver='KML' )

grd <- as(meuse.grid, 'SpatialPolygons')
proj4string(meuse) <- CRS('+init=epsg:28992')
proj4string(grd) <- CRS(proj4string(meuse))
grd.union <- unionSpatialPolygons(grd, rep('x', length(slot( grd, 
															 'polygons' ))))
ll <- CRS('+proj=longlat +datum=WGS84')
grd.union.ll <- spTransform(grd.union, ll) 

llGRD <- GE_SpatialGrid(grd.union.ll)
llGRD_in <- over(llGRD$SG, grd.union.ll)
llSGDF <- SpatialGridDataFrame( grid=slot(llGRD$SG, 'grid'), 
								proj4string=CRS(proj4string(llGRD$SG)), 
								data=data.frame(in0=llGRD_in) )
llSPix <- as(llSGDF, 'SpatialPixelsDataFrame')
meuse_ll <- spTransform(meuse, CRS('+proj=longlat +datum=WGS84'))
llSPix$pred <- gstat::idw(log(zinc) ~ 1, meuse_ll, llSPix)$var1.pred
png( file='~/Desktop/R/Spatial/SpatialAnalysisBook/zinc_IDW.png', 
	 width=llGRD$width, height=llGRD$height, bg='transparent' )
par(mar=c(0, 0, 0, 0), xaxs='i', yaxs='i')
image(llSPix, 'pred', col=bpy.colors(50))
dev.off()
kmlOverlay( llGRD, '~/Desktop/R/Spatial/SpatialAnalysisBook/zinc_IDW.kml', 
			'~/Desktop/R/Spatial/SpatialAnalysisBook/zinc_IDW.png' )



# 4.5 Geographical Resources Analysis Support System (GRASS)
execGRASS('g.region', flags='p') # Nope
spear <- readRAST6(c('elevation.dem', 'geology'), cat=c(F, T)) # and nope
bugsDF <- readVECT6('bugsites') # more nope
vInfo('streams') # no
streams <- readVECT6('streams', type='line, boundary', remove.duplicates=F) # no
# ...still more section errors... 

	# 4.5.1
	# ...more errors ...
#	r <- as(SGDF, 'RasterLayer') # Cannot be run b/c of previous errors




save.image('~/Desktop/R/Spatial/SpatialAnalysisBook/SpatialAnalysis.RData')