#---------------------------#
#                           #
#  Spatial Analysis with R  #
#                           #
#---------------------------#

# Relevant libraries:
library(sp)	#!
library(gstat)
library(spatial)
library(geoR)
library(geoRglm)
library(spatstat)
library(circular)
library(RandomFields)
library(rgdal) #!
	# readGDAL, writeGDAL:	# read/write between GDAL grid maps and spatial objects
	# readOGR, writeOGR:	# read/write vector data using OGR, including KML for 
							# Google Earth
library(maptools)		# reading and handling spatial objs; mostly superceded by 
						# rgdal
	# read.shape, readShapePoints, readShapeLines, readShapePoly: # Read ESRI .shp
	# files
	# writeShapePoints, writeShapeLines, writeShapePoly: # Write ESRI .shp files
	# read.AsciiGrid, write.AsciiGrid: ESRI ASCII grid interchange

# 
library(gstat)
data(meuse)
head(meuse)

# convert to spatial data:
coordinates(meuse) <- ~ x + y
str(meuse)

# randomly sample points from meuse coords:
spsample(meuse, 5, 'random')

# compute default experimental variogram
v <- variogram(log(lead) ~ 1, meuse)	#ERROR

data(meuse.grid)

# convert to spatial obj
coordinates(meuse.grid) <- ~ 1	#ERROR