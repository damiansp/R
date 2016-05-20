#================#
#                #
#  Mapping in R  #
#                #
#================#

rm(list=ls())
search()

library(RgoogleMaps)
library(png)
library(sp)
library(spdep)


AA83 <- CRS('+init=epsg:3338')

bccSp <- readShapeSpatial('~/Desktop/Shapefiles/S1BuckCCTopo100_Poly.shp', 
						  proj4string=AA83)
bccPoly <- readShapePoly('~/Desktop/Shapefiles/S1BuckCCTopo100_Poly.shp', 
						  proj4string=AA83)
plot(bccSp)
plot(bccPoly)


# Village coords
nwab <- data.frame(names=c('Buckland', 'Deering', 'Kivalina', 'Kotzebue', 'Noatak',
						   'Noorvik', 'Selawik'),
					lat=c(65.977, 66.074, 67.728, 66.909, 67.565, 66.828, 66.601),
					lon=c(-161.132, -162.721, -164.538, -162.602, -162.981, 
						  -161.031, -159.995))

schools.data <- read.csv('~/Desktop/R_Files/Spatial_ly/Intro_R_Data/schools.csv')
attach(schools.data)
names(schools.data)

stripchart(attainment, method='stack', xlab='Mean prior attainment by school')
hist(attainment, col='light blue', border='dark blue', freq=F, ylim=c(0, 0.3))
rug(attainment, col='dark blue')
lines(density(attainment))
xx <- seq(23, 35, 0.1)
yy <- dnorm(xx, mean(attainment), sd(attainment))
lines(xx, yy, lty=2)

school.type <- rep('NoFaith', times=nrow(schools.data))
school.type[coe==1] <- 'CoE'
school.type[rc==1] <- 'RC'
school.type[vol.con==1] <- 'VC'
school.type[other.faith==1] <- 'otherFaith'
school.type[selective==1] <- 'Selective'
school.type <- factor(school.type)

par(mai=c(1, 1.4, 0.5, 0.5)) # change margins
boxplot(attainment ~ school.type, horizontal=T, las=1) # las=1 rotates y-axis text
abline(v = median(attainment), lty=4)

palette <- c('red', 'orange', 'yellow', 'green')
map.class <- cut(fsm, quantile(fsm), labels=F, include.lowest=T)
plot(Easting, Northing, asp=1, cex=sqrt(esl * 5), pch=21, bg=palette[map.class]) 
# asp = 1 preserves x:y aspect ratio

# MapBackground(lat, lon, destfile, NEWMAP=T, myTile, zoom=NULL, size=c(640,640),
#				GREYSCALE = F, mar=c(0,0,0,0), PLOT=F, verbose=1, ...)
# { destfile: file to load from or save to acc. to NEWMAP
#   NEWMAP: if T, queries Google server and saves to destfile; if F loads from 
#           destfile
#   myTile: map tile fr prev downloads
#   PLOT: if T, leave plotting to PlotOnStaticMap (recommended) }
MyMap <- MapBackground(lat=Lat, lon=Long)
PlotOnStaticMap(MyMap, Lat, Long, cex=sqrt(esl * 5), pch=21, bg=palette[map.class])
legend('topleft', legend=paste('<', tapply(fsm, map.class, max)), pch=21, 
	   pt.bg=palette, pt.cex=1.5, bg='white', title='% FSM-Eligible')
legVals <- seq(0.2, 1, 0.2)
legend('topright', legen=round(legVals, 3), pch=21, pt.bg='white', 
	   pt.cex=sqrt(legVals * 5), bg='white', title='% ESL')

NWABMap <- MapBackground(nwab$lat, nwab$lon, maptype='satellite')
PlotOnStaticMap(NWABMap, nwab$lat, nwab$lon, pch=21, col='red')

# 2.6 Simple Geographical Analyses
# Check for spatial correlations with nearest neighbors
detach(schools.data)
schools.xy <- schools.data
attach(schools.xy)
# Convert to spatial object
coordinates(schools.xy) <- c('Easting', 'Northing')
detach(schools.xy)
proj4string(schools.xy) <- CRS('+proj=tmerc datum=OSGB36')

# find nearest 6 neigbors
# library(spdep); RANN=F: override the use of RANN package which may not be 
# installed
nearest.six <- knearneigh(schools.xy, k=6, RANN=F) # class = knn
neighbors <- knn2nb(nearest.six) # convert to more general neighbor class, nb
summary(neighbors)
plot(neighbors, coordinates(schools.xy)) #plot connections to neighbors

# Create a matrix of weightings with equal weight given to ea of the 6 neighbors
spatial.weights <- nb2listw(neighbors)
model1 <- lm(attainment ~ fsm, data=schools.data)
model2 <- lm(attainment ~ fsm + white, data=schools.data)
model3 <- lm(attainment ~ fsm + white + selective, data=schools.data)
summary(model3)
model4 <- lm(attainment ~ fsm + selective, data=schools.data)
summary(model4)

lm.morantest(model4, spatial.weights) # p < alpha -> signif. spat autocorrelation




#====================#
#                    #
#  ggplot2 tutorial  #
#                    #
#====================#

rm(list=ls())
search()

library(maptools)
library(ggplot2)
library(gpclib)
library(reshape)

sport <- readShapePoly('~/Desktop/R_Files/Spatial_ly/R-ggplot2-data/london_sport.shp')
names(sport)

p <- ggplot(sport@data, aes(Partic_Per, Pop_2001))
p + geom_point(aes(color=Partic_Per, size=Pop_2001)) + 
	geom_text(size=2, aes(label=name))


# To make polygon coordinate data readable to ggplot2:
# First activate the gpclib library
gpclibPermit()
# ...then convert with fortify()
sport_geom <- fortify(sport, region='ons_label')
bcc_geom <- fortify(bccPoly, region='MapID')
sport_geom <- merge(sport_geom, sport@data, by.x='id', by.y='ons_label')
bcc_geom <- merge(bcc_geom, bccPoly@data, by.x='id', by.y='MapID')
summary(sport_geom)
head(sport_geom)
head(bcc_geom)

# coord_equal() preserves x:y (lon:lat) aspect ratio
Map <- ggplot(sport_geom, aes(long, lat, group=group, fill=Partic_Per)) + 
	geom_polygon() + coord_equal() + 
	labs(x='Easting (m)', y='Northing (m)', fill='% Sport Partic.') +
	ggtitle('London Sports Participation')
Map
Map + scale_fill_gradient(low='red', high='green')
bccMap <- ggplot(bcc_geom, aes(long, lat, group=group, fill=log(Shape_Area))) + geom_polygon() + coord_equal()
bccMap

# To save map img
#ggsave('mapFileName.pdf')
#ggsave('mapFileName.png', scale=3, dpi=200)



input <- read.csv('~/Desktop/R_Files/Spatial_ly/R-ggplot2-data/ambulance_assault.csv')
head(input)
p_ass <- ggplot(input, aes(x=assault_09_11))
p_ass + geom_histogram()
p_ass + geom_histogram(binwidth=10) + geom_density(fill=NA, color='black')
p_ass + geom_histogram() + facet_wrap(~Bor_Code)

p2_ass <- ggplot(input, aes(x=assault_09_11, y=..density..))
p2_ass + geom_histogram() + geom_density(fill=NA, color='red')

p3_ass <- ggplot(input, aes(x=Bor_Code, y=assault_09_11))
p3_ass + geom_boxplot()
p3_ass + geom_boxplot() + coord_flip()

london.data <- read.csv('~/Desktop/R_Files/Spatial_ly/R-ggplot2-data/census-historic-population-borough.csv')
head(london.data)
# reshape library:
london.data.melt <- melt(london.data, id=c('Area.Code', 'Area.Name'))
head(london.data.melt)
plot.data <- merge(sport_geom, london.data.melt, by.x='id', by.y='Area.Code')
plot.data <- plot.data[order(plot.data$order), ]
ggplot(data=plot.data, aes(x=long, y=lat, fill=value, group=group)) + 
	geom_polygon() + geom_path(color='grey', lwd=0.1) + coord_equal() +
	facet_wrap(~variable) + scale_fill_gradient(low='green', high='red')




#================================#
#                                #
#  Handling Spatial Data with R  #
#                                #
#================================#
rm(list=ls())
search()

library(maptools)
library(rgdal)

cycle <- read.csv('~/Desktop/R_Files/Spatial_ly/London_cycle_hire_locs.csv')
head(cycle)

plot(cycle$X, cycle$Y)
coordinates(cycle) <- c('X', 'Y')
class(cycle)
str(cycle)

# To see available CRSs:
EPSG <- make_EPSG()
head(EPSG)

# In the case of these data, the British National Grid System is used
# Search for it:
with(EPSG, EPSG[grep('British National', note), ]) # note code value (27700)
with(EPSG, EPSG[grep('Alaska', note), ]) # note code value (3338) (#NAD83 / Alaska Albers)


BNG <- CRS('+init=epsg:27700')
proj4string(cycle) <- BNG

sport <- readShapePoly('~/Desktop/R_Files/Spatial_ly/R-ggplot2-data/london_sport.shp', proj4string=BNG)
names(sport)
sport@proj4string
plot(sport, col='blue')

# Add cycle points
plot(cycle, add=T, col='orange', pch=15, cex=0.5)

# To export (.csv, etc) to .shp:
#writePointsShape(cycle, 'cycle.shp') # point data
#writePolyShape(sport, 'londonSport.shp') # poly data
#writeLinesShape(lineObject, 'lineFile.shp') # path data





#=====================#
#                     #
#  Maps with ggplot2  #
#                     #
#=====================#

rm(list=ls())
search()

library(maptools)
library(RColorBrewer)
library(ggplot2)

poly_coords <- function(shapefile) {
	if (nrow(data.frame(shapefile$ID))< 1) {
		print ("No ID field in SpatialPolygon")
	} else {
		Order <- 0 
		YX3 <- as.numeric("XX", "XX", "XX", "XX")
		num_polys <- nrow(shapefile@data)+1
		YX3 <- as.numeric("XX", "XX", "XX")
	
		curr_poly <- shapefile@data[1,]
		curr_poly_start_row <- 1
		poly_old <- F
	
		for(curr_row in curr_poly_start_row:num_polys) {
			curr_poly_row <- shapefile@data[curr_row,]
			curr_poly_end_row <- curr_row - 1	
			Poly_n <- shapefile@data[curr_poly_start_row:curr_poly_end_row,]
			curr_poly_start_row <- curr_row
			Poly_Name <- as.vector(Poly_n$ID)
			Poly <- shapefile[shapefile$ID == Poly_Name,]
			PolyCoords <- lapply(slot(Poly, "polygons"), 
						     function(x) lapply(slot(x, "Polygons"),
						     				   function(y) slot(y, 
						     				                    "coords")))
			PolyCoordsY <- PolyCoords[[1]][[1]][,1]
			PolyCoordsX <- PolyCoords[[1]][[1]][,2]
			Order <- 1:nrow(data.frame(PolyCoordsX)) + max(Order)

			if (poly_old != Poly_n$ID) {
				YX1 <- data.frame(Poly_Name, Order, PolyCoordsY, PolyCoordsX)
				YX2 <-rbind(YX3,YX1)
				YX3 <-YX2
			}

			poly_old <- Poly_n$ID
		}
	
		join <- merge(YX3, shapefile@data, by.x="Poly_Name", by.y= "ID", all=T)
		join[order(join$Order),][1:nrow(join)-1,]
	}
}

sport <- readShapePoly('~/Desktop/R_Files/Spatial_ly/R-ggplot2-data/london_sport.shp')
names(sport)
names(sport)[1] <- 'ID'

sport_geom <- poly_coords(sport)
head(sport_geom)

map <- qplot(PolyCoordsY, PolyCoordsX, data=sport_geom, group=Poly_Name, 
		   fill=Partic_Per, geom='polygon')
map
map + scale_fill_gradient(low='blue', high='orange')

new_fill <- function(pal, lowerLim, upperLim) {
	scale_fill_gradient(colors=pal, limits=c(lowerLim, upperLim))
}

map + new_fill(brewer.pal(7, 'Blues'), 0, 30)





#====================#
#                    #
#  Using R as a GIS  #
#                    #
#====================#

rm(list=ls())
search()

library(sp)
library(raster)
library(rasterVis)
library(maptools)
library(rworldmap) 
library(googleVis)
library(RgoogleMaps)
library(dismo)
library(spatial)
library(spatstat)
library(spatgraphs)
library(ecespa)
library(gstat)
library(geoR)
library(akima)
library(spdep)

# Examples with rworldmap:
newmap <- getMap(resolution='less', projection='none')
plot(newmap)
mapCountryData()
mapCountryData(mapRegion='europe')
mapGriddedData()
mapGriddedData(mapRegion='europe')

# Examples with googleVis
data(Exports)
#View(Exports) #requires X11
Geo <- gvisGeoMap(Exports, locationvar='Country', numvar='Profit', 
				  options=list(height=400, dataMode='regions'))
plot(Geo)
# Generate an HTML doc:
print(Geo)

data(Andrew)
M1 <- gvisMap(Andrew, 'LatLong', 'Tip', 
			  options=list(showTip=T, showLine=F, enableScrollWheel=T, 
			  			   mapType='satellite', useMapTypeControl=T, width=800,
			  			   height=400))
plot(M1)

# Examples with RgoogleMaps
newmap <- GetMap(center=c(36.7, -5.9), zoom=10, destfile='newmap.png', 
				 maptype='satellite')
newmap2 <- GetMap.bbox(lonR=c(-5, -6), latR=c(36, 37), destfile='newmap.png', 
					   maptype='terrain')
PlotOnStaticMap(lat=c(36.3, 35.8, 36.4), lon=c(-5.5, -5.6, -5.8), zoom=10, cex=2,
				pch=19, col='red', FUN=points, add=F)

# Examples with dismo
mymap <- gmap('France')
plot(mymap)
mymap <- gmap('Spain', type='satellite')
plot(mymap)
mymap <- gmap('Spain', type='satellite', exp=3)
plot(mymap)
mymap <- gmap('Spain', type='satellite', exp=8)
plot(mymap)
# Save a map:
#mymap <- gmap('Spain', type='satellite', filename='Spain.gmap')
mymap <- gmap('Europe')
plot(mymap)
select.area <- drawExtent()
mymap <- gmap(select.area)
plot(mymap)
mymap <- gmap('North America', type='satellite', exp=8)
plot(mymap)
alaska <- drawExtent()
alaska <- gmap(alaska)
plot(alaska)
nwab <- drawExtent()
nwab <- gmap(nwab)
plot(nwab)

# Spatial Stats exs
data(fig1)
plot(fig1)
data(Helianthemum)
cosa12 <- K1K2(Helianthemum, j='deadpl', i='survpl', r=seq(0, 200, le=201), 
			   nsim=99, nrank=1, correction='isotropic')
plot(cosa12$k1k2, lty=c(2, 1, 2), col=c(2, 1, 2), xlim=c(0, 200),
	 main='Survival - Death', ylab=expression(K[1] - K[2]), legend=F)
	 
# Geostat exs

# Spatial vector data
laurus <- gbif('Laurus', 'nobilis') # kinda slow
locs <- subset(laurus, select=c('country', 'lat', 'lon'))
locs <- locs[complete.cases(locs) == T, ]
head(locs)
coordinates(locs) <- c('lon', 'lat') # make spatial
plot(locs)
crs.geo <- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84')
proj4string(locs) <- crs.geo
summary(locs)

data(wrld_simpl)
summary(wrld_simpl)
plot(locs, pch=20, col='steelblue')
plot(wrld_simpl, add=T)

# subsetting
table(locs@data$country)
locs.gr <- subset(locs, locs$country=='Greece')
plot(locs.gr, pch=20, cex=2, col='steelblue')
plot(wrld_simpl, add=T)

# Making maps
# Continue at:

https://sites.google.com/site/rodriguezsanchezf/news/usingrasagis
### MAKING MAPS ###
 
# Plotting onto a Google Map using RGoogleMaps
