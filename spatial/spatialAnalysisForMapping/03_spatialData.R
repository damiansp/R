#!/usr/bin/env Rscript
#---------#---------#---------#---------#---------#---------#---------#---------
setwd('~/Learning/R/spatial/spatialAnalysisForMapping')
Sys.setenv(JAVA_HOME='/Library/Java/JavaVirtualMachines/jdk1.8.0_121.jdk/Contents/Home/jre')
rm(list=ls())

library(GISTools)
library(maps)
library(OpenStreetMap)
library(PBSmapping)
library(rJava)
library(RgoogleMaps)

data(georgia)
data(newhaven)
data(quakes)


# 2 Introduction: GISTools
# 2.2 Spatial tools in GISTools
class(roads)

par(mar=rep(0, 4))
plot(blocks, lwd=0.5, col='grey50')
plot(roads, add=T, col=2)
plot(breach, col=4, pch=1, add=T)
colors()

# 2.3 Embellishing the map
map.scale(534750, 152000, miles2ft(2), 'Miles', 4, 0.5)
#map.scale(xc, yc, length, 'text', n.grad, grad.len)
north.arrow(534750, 154000, miles2ft(0.25), col='lightblue')
#north.arrow(xc.base, yc.base, base.len, ...)
title('New Haven, CT')

# 2.4 Saving maps
#pdf(file='./images/newHaven.pdf')
#png(file='./images/newHaven.png')
#dev.off()



# 3 Mapping Spatial Objects
# 3.3 Plotting options
georgia.outline <- gUnaryUnion(georgia, id=NULL)
plot(georgia, col=2, bg='wheat', border=4)
plot(georgia.outline, lwd=3, add=T)
title(main='The State of Georgia', 
      font.main=2, 
      cex.main=1.5, 
      sub='and its Counties', 
      font.sub=3, 
      col.sub=4)

lat <- data.frame(georgia)[, 1]
lon <- data.frame(georgia)[, 2]
names <- data.frame(georgia)[, 13]
par(mar=rep(0, 4))
plot(georgia, col=NA)
point.label <- pointLabel(lon, lat, names, offset=0, cex=0.5)
county.tmp <- c(16, 17, 21, 53, 62, 81:83, 121, 124, 150)
georgia.sub <- georgia[county.tmp,]
par(mar=c(0, 0, 3, 0))
plot(georgia.sub, col='gold1', border='grey')
plot(georgia.outline, add=T, lwd=2)
title('Some Georgia Counties', cex.main=2, font.main=1)
point.label <- pointLabel(
  lon[county.tmp], lat[county.tmp], names[county.tmp], offset=3, cex=1.5)

plot(georgia, border='grey', lwd=0.5)
plot(georgia.sub, add=T, col='lightblue')
plot(georgia.outline, lwd=2, add=T)
title('Georgia: subset of counties')

# 3.4 Adding context
upper.left <- as.vector(cbind(bbox(georgia.sub)[2, 2], 
                              bbox(georgia.sub)[1, 1]))
lower.right <- as.vector(cbind(bbox(georgia.sub)[2, 1],
                               bbox(georgia.sub)[1, 2]))
georgia.osmap <- openmap(upper.left, lower.right, 9, 'mapquest')
par(mar=rep(0, 4))
plot(georgia.osmap, removeMargin=F)
plot(spTransform(georgia.sub, osm(), add=T, lwd=2))

shp <- SpatialPolygons2PolySet(georgia.sub)
bb <- qbbox(lat=shp[, 'Y'], lon=shp[, 'X'])
georgia.gmap <- GetMap.bbox(bb$lonR, bb$latR, destfile='./images/DC.jpg')
par(mar=rep(0, 4))
PlotPolysOnStaticMap(georgia.gmap, shp, lwd=2, col=rep(0.25, 4), add=F)
par(mar=c(5, 4, 4, 2))



# 4 Mapping Spatial Data Attributes
# 4.2 Attributes and data frames
ls()
summary(blocks)
summary(breach)
head(data.frame(blocks))
blocks$OCCUPIED
hist(blocks$P_VACANT)

breach.dens <- kde.points(breach, lims=tracts) # kernel density
summary(breach.dens)
class(breach.dens)
head(data.frame(breach.dens))
plot(breach.dens)
breach.dens.grid <- as(breach.dens, 'SpatialGridDataFrame')
summary(breach.dens.grid)

# 4.3 Mapping polygons and attributes
par(mfrow=c(1, 1))
display.brewer.all()

head(data.frame(blocks))
par(mfrow=c(2, 2))
par(mar=rep(0, 4))
choropleth(blocks, blocks$P_WHITE)
white.shades <- auto.shading(blocks$P_WHITE)
choro.legend(533000, 161000, white.shades)
black.shades <- auto.shading(blocks$P_BLACK, n=7)
choropleth(blocks, blocks$P_BLACK, shading=black.shades)
choro.legend(533000, 165000, black.shades)
es.shades <- auto.shading(blocks$P_AMERI_ES, cols=brewer.pal(7, 'BuGn'))
choropleth(blocks, blocks$P_AMERI_ES, shading=es.shades)
as.shades <- auto.shading(
  blocks$P_ASIAN_PI, n=5, cols=brewer.pal(5, 'Blues'), cutter=rangeCuts)
choropleth(blocks, blocks$P_ASIAN_PI, shading=as.shades)
choro.legend(533000, 161000, as.shades)

white.shades

# 4.4 Mapping points and attributes
par(mfrow=c(1, 1))
plot(blocks)
plot(breach, pch=16, col='#DE2D2680', add=T)

head(quakes)
quake.coords <- cbind(quakes$long, quakes$lat)
quake.spdf <- SpatialPointsDataFrame(quake.coords, data=data.frame(quakes))
plot(quake.spdf, pch=16, col='#FB6A4A80')
map('world2', fill=F, add=T)